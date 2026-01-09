#pragma once
#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include <utility>
#include <unordered_map>
#include <map>

// LLVM Headers
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

// Forward declarations
class FunctionAST;
class ConstructorAST;

// 1. Enhanced Type System for Pointers
enum class BaseType { Void, Int, Float, Bool, Char, Byte, Struct };

struct OType {
    BaseType base;
    int pointerDepth = 0; // 0 = value, 1 = *, 2 = **, etc.
    std::vector<int> arraySizes; // Empty = not array, [3] = 1D, [3,3] = 2D
    std::string structName; // For struct types
    std::vector<OType> genericArgs; // Generic type arguments like <int> in List<int>
    
    OType(BaseType b = BaseType::Void, int depth = 0, const std::string& name = "", std::vector<int> arrSizes = {}, std::vector<OType> genArgs = {}) 
        : base(b), pointerDepth(depth), arraySizes(arrSizes), structName(name), genericArgs(genArgs) {}
    
    // Compatibility constructor for single dimension code
    OType(BaseType b, int depth, const std::string& name, int arrSize) 
        : base(b), pointerDepth(depth), structName(name) {
        if (arrSize > 0) arraySizes.push_back(arrSize);
    }
    
    bool isPointer() const { return pointerDepth > 0; }
    bool isArray() const { return !arraySizes.empty(); }
    
    OType getPointeeType() const { 
        return OType(base, pointerDepth - 1, structName, arraySizes); 
    }
    
    OType getPointerTo() const { 
        return OType(base, pointerDepth + 1, structName, arraySizes); 
    }
    
    OType getElementType() const {
        if (arraySizes.empty()) return *this;
        std::vector<int> newSizes = arraySizes;
        newSizes.erase(newSizes.begin());
        // Preserve all other properties including genericArgs
        OType result(base, pointerDepth, structName, newSizes, genericArgs);
        return result;
    }

    uint64_t getArrayNumElements() const {
        if (arraySizes.empty()) return 0;
        return arraySizes[0];
    }

    OType substitute(const std::map<std::string, OType>& map) const {
        if (map.empty()) return *this;

        OType newType = *this;

        // First, handle the case where this type itself is a generic parameter to be substituted
        if (base == BaseType::Struct && map.count(structName)) {
            OType replacement = map.at(structName);

            // If the replacement has its own array sizes, we need to handle combining them
            std::vector<int> finalArraySizes = arraySizes; // Preserve original array dimensions
            if (!replacement.arraySizes.empty()) {
                // Combine array dimensions: original array dims + replacement array dims
                finalArraySizes.insert(finalArraySizes.end(),
                                     replacement.arraySizes.begin(),
                                     replacement.arraySizes.end());
            }

            newType = OType(replacement.base,
                           replacement.pointerDepth + pointerDepth,
                           replacement.structName,
                           finalArraySizes,
                           replacement.genericArgs);
        } else {
            // Handle generic arguments recursively
            for (auto& arg : newType.genericArgs) {
                arg = arg.substitute(map);
            }
        }

        return newType;
    }
};

// Struct field information
struct FieldInfo {
    std::string name;
    OType type;
    std::string structTypeName; // For nested structs
    size_t offset;
};

// Struct type information
struct StructInfo {
    std::string name;
    std::vector<FieldInfo> fields;
    size_t totalSize;
    std::vector<std::string> virtualMethods; // Names of virtual methods in VTable order
};

// Global type registry
class TypeRegistry {
public:
    static TypeRegistry& getInstance() {
        static TypeRegistry instance;
        return instance;
    }
    
    void registerStruct(const std::string& name, const std::vector<FieldInfo>& fields, const std::vector<std::string>& vMethods = {}) {
        StructInfo info;
        info.name = name;
        info.fields = fields;
        info.totalSize = calculateSize(fields);
        info.virtualMethods = vMethods;
        structs[name] = info;
    }

    void registerStruct(const std::string& name, const std::vector<FieldInfo>& fields, size_t size, const std::vector<std::string>& vMethods = {}) {
        StructInfo info;
        info.name = name;
        info.fields = fields;
        info.totalSize = size;
        info.virtualMethods = vMethods;
        structs[name] = info;
    }
    
    bool hasStruct(const std::string& name) const {
        return structs.find(name) != structs.end();
    }
    
    const StructInfo& getStruct(const std::string& name) const {
        return structs.at(name);
    }
    
    size_t getTypeSize(const OType& type) {
        if (type.isPointer()) return 8; // All pointers are 8 bytes
        
        switch (type.base) {
            case BaseType::Char:
            case BaseType::Byte: 
            case BaseType::Bool: return 1;
            case BaseType::Int: return 4;
            case BaseType::Float: return 8;
            default: return 4;
        }
    }
    
private:
    std::unordered_map<std::string, StructInfo> structs;
    
    size_t calculateSize(const std::vector<FieldInfo>& fields) {
        size_t offset = 0;
        for (auto& field : fields) {
            size_t fieldSize = getTypeSize(field.type);
            offset += fieldSize;
        }
        return offset;
    }
};

// Helper to get type from string
inline OType stringToType(const std::string& t) {
    if (t == "int") return OType(BaseType::Int);
    if (t == "float") return OType(BaseType::Float);
    if (t == "bool") return OType(BaseType::Bool);
    if (t == "char") return OType(BaseType::Char);
    if (t == "byte") return OType(BaseType::Byte);
    if (t == "void") return OType(BaseType::Void);
    
    // Check if it's a user-defined struct
    if (TypeRegistry::getInstance().hasStruct(t)) {
        return OType(BaseType::Struct, 0, t);
    }
    
    return OType(BaseType::Void);
}

// Base Class for all AST nodes
class ExprAST {
public:
    virtual ~ExprAST() = default;
    
    // The magical method that generates LLVM code
    virtual llvm::Value *codegen() = 0;
    
    // Optional: Get address for l-value usage (assignment, & operator)
    virtual llvm::Value *codegenAddress() { return nullptr; }

    // Get the OType of the expression
    virtual OType getOType() const { return OType(BaseType::Void); }

    // Clone method for deep copy with type substitution
    virtual std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const = 0;
};

// 2a. Boolean Node
class BoolExprAST : public ExprAST {
    bool Val;
public:
    BoolExprAST(bool Val) : Val(Val) {}
    llvm::Value *codegen() override;
    OType getOType() const override { return OType(BaseType::Bool); }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override { return std::make_unique<BoolExprAST>(Val); }
};

// 2b. While Loop Node
class WhileExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond;
    std::unique_ptr<ExprAST> Body;
public:
    WhileExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Body)
        : Cond(std::move(Cond)), Body(std::move(Body)) {}
    llvm::Value *codegen() override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<WhileExprAST>(Cond->clone(typeMap), Body->clone(typeMap));
    }
};

// 2c. For Loop Node
class ForExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Init;
    std::unique_ptr<ExprAST> Cond;
    std::unique_ptr<ExprAST> Step;
    std::unique_ptr<ExprAST> Body;
public:
    ForExprAST(std::unique_ptr<ExprAST> Init, std::unique_ptr<ExprAST> Cond,
               std::unique_ptr<ExprAST> Step, std::unique_ptr<ExprAST> Body)
        : Init(std::move(Init)), Cond(std::move(Cond)),
          Step(std::move(Step)), Body(std::move(Body)) {}
    llvm::Value *codegen() override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<ForExprAST>(
            Init ? Init->clone(typeMap) : nullptr,
            Cond ? Cond->clone(typeMap) : nullptr,
            Step ? Step->clone(typeMap) : nullptr,
            Body->clone(typeMap)
        );
    }
};

// 2d. Updated Number Node
class NumberExprAST : public ExprAST {
    double Val;
    OType Type; // Store the type
public:
    NumberExprAST(double Val, OType Type) : Val(Val), Type(Type) {}
    llvm::Value *codegen() override;
    OType getType() const { return Type; } // Type getter
    double getVal() const { return Val; } // Value getter
    OType getOType() const override { return Type; }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override { return std::make_unique<NumberExprAST>(Val, Type.substitute(typeMap)); }
};

/// StringExprAST - Expression class for string literals like "hello"
class StringExprAST : public ExprAST {
    std::string Val;
public:
    StringExprAST(const std::string &Val) : Val(Val) {}
    llvm::Value *codegen() override;
    OType getOType() const override { return OType(BaseType::Char, 1); } // char*
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override { return std::make_unique<StringExprAST>(Val); }
};

/// StructDeclAST - Represents a struct declaration
class StructDeclAST {
    std::string Name;
    std::vector<std::string> GenericParams; // Generic type parameters like <T, U>
    std::vector<std::pair<std::string, OType>> Fields; // field name, type
    std::vector<std::unique_ptr<FunctionAST>> Methods; // struct methods
    std::vector<std::unique_ptr<ConstructorAST>> Constructors; // struct constructors
public:
    StructDeclAST(const std::string &Name,
                  std::vector<std::string> GenericParams,
                  std::vector<std::pair<std::string, OType>> Fields,
                  std::vector<std::unique_ptr<FunctionAST>> Methods = {},
                  std::vector<std::unique_ptr<ConstructorAST>> Constructors = {})
        : Name(Name), GenericParams(std::move(GenericParams)), Fields(std::move(Fields)), 
          Methods(std::move(Methods)), Constructors(std::move(Constructors)) {}
    
    void codegen(); // Register the struct type
    const std::string& getName() const { return Name; }
    bool isGeneric() const { return !GenericParams.empty(); }
    const std::vector<std::string>& getGenericParams() const { return GenericParams; }
    void setName(const std::string& newName) { Name = newName; }
    void makeConcrete() { GenericParams.clear(); }
    
    std::unique_ptr<StructDeclAST> clone(const std::map<std::string, OType>& typeMap = {}) const;
};

/// ClassDeclAST - Represents a class declaration with inheritance
class ClassDeclAST {
    std::string Name;
    std::string ParentName; // Empty if no inheritance
    bool IsOpen; // Can be inherited from
    std::vector<std::pair<std::string, OType>> Fields;
    std::vector<std::unique_ptr<FunctionAST>> Methods;
    std::vector<std::unique_ptr<ConstructorAST>> Constructors;
    std::vector<std::string> VirtualMethods; // Names of virtual methods
public:
    ClassDeclAST(const std::string &Name,
                 const std::string &ParentName,
                 bool IsOpen,
                 std::vector<std::pair<std::string, OType>> Fields,
                 std::vector<std::unique_ptr<FunctionAST>> Methods = {},
                 std::vector<std::unique_ptr<ConstructorAST>> Constructors = {},
                 std::vector<std::string> VirtualMethods = {})
        : Name(Name), ParentName(ParentName), IsOpen(IsOpen), Fields(std::move(Fields)), 
          Methods(std::move(Methods)), Constructors(std::move(Constructors)),
          VirtualMethods(std::move(VirtualMethods)) {}
    
    void codegen(); // Register the class type with VTable
    void generateVTable(); // Generate VTable for virtual methods
    const std::string& getName() const { return Name; }
    bool hasParent() const { return !ParentName.empty(); }
    bool isOpen() const { return IsOpen; }
    
    std::unique_ptr<ClassDeclAST> clone(const std::map<std::string, OType>& typeMap = {}) const;
};

/// MemberAccessAST - Expression for accessing struct members (obj.field)
class MemberAccessAST : public ExprAST {
    std::unique_ptr<ExprAST> Object;
    std::string FieldName;
public:
    MemberAccessAST(std::unique_ptr<ExprAST> Object, const std::string &FieldName)
        : Object(std::move(Object)), FieldName(FieldName) {}
    llvm::Value *codegen() override;
    llvm::Value *codegenAddress() override;
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<MemberAccessAST>(Object->clone(typeMap), FieldName);
    }
};

/// ConstructorAST - Represents a struct constructor
class ConstructorAST {
    std::vector<std::pair<std::string, OType>> Params;
    std::unique_ptr<ExprAST> Body;
public:
    ConstructorAST(std::vector<std::pair<std::string, OType>> Params,
                   std::unique_ptr<ExprAST> Body)
        : Params(std::move(Params)), Body(std::move(Body)) {}
    llvm::Function *codegen(const std::string& structName);
    const std::vector<std::pair<std::string, OType>>& getParams() const { return Params; }
    std::unique_ptr<ConstructorAST> clone(const std::map<std::string, OType>& typeMap = {}) const {
        // ... (clone impl)
        std::vector<std::pair<std::string, OType>> NewParams;
        for(const auto& p : Params) NewParams.push_back({p.first, p.second.substitute(typeMap)});
        return std::make_unique<ConstructorAST>(NewParams, Body ? Body->clone(typeMap) : nullptr);
    }
};

/// AddressOfExprAST - Expression for getting address of a variable (&var)
class AddressOfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Operand;
public:
    AddressOfExprAST(std::unique_ptr<ExprAST> Operand)
        : Operand(std::move(Operand)) {}
    llvm::Value *codegen() override;
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<AddressOfExprAST>(Operand->clone(typeMap));
    }
};

/// DerefExprAST - Expression for dereferencing a pointer (*ptr)
class DerefExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Operand;
public:
    DerefExprAST(std::unique_ptr<ExprAST> Operand)
        : Operand(std::move(Operand)) {}
    llvm::Value *codegen() override;
    llvm::Value *codegenAddress() override; // For LHS assignment: *ptr = value
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<DerefExprAST>(Operand->clone(typeMap));
    }
};

/// NewExprAST - Expression for object instantiation (new ClassName(args))
class NewExprAST : public ExprAST {
    std::string ClassName;
    std::vector<OType> GenericArgs;
    std::vector<std::unique_ptr<ExprAST>> Args;
public:
    NewExprAST(const std::string &ClassName, std::vector<std::unique_ptr<ExprAST>> Args, std::vector<OType> GenArgs = {})
        : ClassName(ClassName), Args(std::move(Args)), GenericArgs(GenArgs) {}
    llvm::Value *codegen() override;
    OType getOType() const override { return OType(BaseType::Struct, 1, ClassName, {}, GenericArgs); } // Returns pointer to struct
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        OType temp(BaseType::Struct, 0, ClassName, {}, GenericArgs);
        temp = temp.substitute(typeMap);
        
        std::vector<std::unique_ptr<ExprAST>> NewArgs;
        for(const auto& a : Args) NewArgs.push_back(a->clone(typeMap));
        return std::make_unique<NewExprAST>(temp.structName, std::move(NewArgs), temp.genericArgs);
    }
};

/// NewArrayExprAST - Expression for array allocation (new int[size])
class NewArrayExprAST : public ExprAST {
    OType ElementType;
    std::unique_ptr<ExprAST> Size;
public:
    NewArrayExprAST(OType ElementType, std::unique_ptr<ExprAST> Size)
        : ElementType(ElementType), Size(std::move(Size)) {}
    llvm::Value *codegen() override;
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<NewArrayExprAST>(ElementType.substitute(typeMap), Size->clone(typeMap));
    }
};

/// IndexExprAST - Expression for array indexing (arr[index])
class IndexExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Array;
    std::unique_ptr<ExprAST> Index;
public:
    IndexExprAST(std::unique_ptr<ExprAST> Array, std::unique_ptr<ExprAST> Index)
        : Array(std::move(Array)), Index(std::move(Index)) {}
    llvm::Value *codegen() override;
    llvm::Value *codegenAddress() override;
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<IndexExprAST>(Array->clone(typeMap), Index->clone(typeMap));
    }
};

/// ArrayLiteralExprAST - Expression for array type literals (int[10])
class ArrayLiteralExprAST : public ExprAST {
    OType ElementType;
    int Size;
public:
    ArrayLiteralExprAST(OType ElementType, int Size)
        : ElementType(ElementType), Size(Size) {}
    llvm::Value *codegen() override;
    OType getOType() const override { return OType(ElementType.base, ElementType.pointerDepth, ElementType.structName, Size); }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<ArrayLiteralExprAST>(ElementType.substitute(typeMap), Size);
    }
};

/// ArrayInitExprAST - Expression for array initialization {1, 2, 3}
class ArrayInitExprAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Elements;
    OType ElementType;
public:
    ArrayInitExprAST(std::vector<std::unique_ptr<ExprAST>> Elements, OType ElementType)
        : Elements(std::move(Elements)), ElementType(ElementType) {}
    llvm::Value *codegen() override;
    OType getType() const { return OType(ElementType.base, 0, ElementType.structName, Elements.size()); }
    OType getOType() const override { return getType(); }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        std::vector<std::unique_ptr<ExprAST>> NewElements;
        for(const auto& e : Elements) NewElements.push_back(e->clone(typeMap));
        return std::make_unique<ArrayInitExprAST>(std::move(NewElements), ElementType.substitute(typeMap));
    }
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
    std::string Name;
    std::string TypeName; // For struct types
public:
    VariableExprAST(const std::string &Name, const std::string &TypeName = "") 
        : Name(Name), TypeName(TypeName) {}
    llvm::Value *codegen() override;
    llvm::Value *codegenAddress() override; // Return address without loading
    const std::string& getTypeName() const { return TypeName; }
    const std::string& getName() const { return Name; }
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        std::string newTypeName = TypeName;
        if (!TypeName.empty()) {
             OType temp(BaseType::Struct, 0, TypeName);
             temp = temp.substitute(typeMap);
             newTypeName = temp.structName;
        }
        return std::make_unique<VariableExprAST>(Name, newTypeName);
    }
};

/// BinaryExprAST - Expression class for a binary operator (e.g., "+", "==", "&&").
class BinaryExprAST : public ExprAST {
    std::string Op; // The operator string (e.g., "+", "==", "&&")
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> LHS,
                  std::unique_ptr<ExprAST> RHS)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    llvm::Value *codegen() override;
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<BinaryExprAST>(Op, LHS->clone(typeMap), RHS->clone(typeMap));
    }
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
public:
    CallExprAST(const std::string &Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
        : Callee(Callee), Args(std::move(Args)) {}
    llvm::Value *codegen() override;
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        std::vector<std::unique_ptr<ExprAST>> NewArgs;
        for(const auto& a : Args) NewArgs.push_back(a->clone(typeMap));
        return std::make_unique<CallExprAST>(Callee, std::move(NewArgs));
    }
};

/// MethodCallExprAST - Expression class for method calls (obj.method(args)).
class MethodCallExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Object;
    std::string MethodName;
    std::vector<std::unique_ptr<ExprAST>> Args;
public:
    MethodCallExprAST(std::unique_ptr<ExprAST> Object,
                      const std::string &MethodName,
                      std::vector<std::unique_ptr<ExprAST>> Args)
        : Object(std::move(Object)), MethodName(MethodName), Args(std::move(Args)) {}
    llvm::Value *codegen() override;
    OType getOType() const override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        std::vector<std::unique_ptr<ExprAST>> NewArgs;
        for(const auto& a : Args) NewArgs.push_back(a->clone(typeMap));
        return std::make_unique<MethodCallExprAST>(Object->clone(typeMap), MethodName, std::move(NewArgs));
    }
};

// 3. New Block Node
class BlockExprAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Expressions;
public:
    BlockExprAST(std::vector<std::unique_ptr<ExprAST>> Exprs) 
        : Expressions(std::move(Exprs)) {}
    
    llvm::Value *codegen() override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        std::vector<std::unique_ptr<ExprAST>> NewExprs;
        for(const auto& e : Expressions) NewExprs.push_back(e->clone(typeMap));
        return std::make_unique<BlockExprAST>(std::move(NewExprs));
    }
};

// 4. If Expression Node
class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;
public:
    IfExprAST(std::unique_ptr<ExprAST> Cond,
              std::unique_ptr<ExprAST> Then,
              std::unique_ptr<ExprAST> Else)
        : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    llvm::Value *codegen() override;
    OType getOType() const override { return Then->getOType(); }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<IfExprAST>(Cond->clone(typeMap), Then->clone(typeMap), Else ? Else->clone(typeMap) : nullptr);
    }
};

// 4b. Match Expression Node
struct MatchCase {
    std::unique_ptr<ExprAST> Pattern; // nullptr means wildcard '_'
    std::unique_ptr<ExprAST> Body;
    
    MatchCase(std::unique_ptr<ExprAST> Pat, std::unique_ptr<ExprAST> B)
        : Pattern(std::move(Pat)), Body(std::move(B)) {}
        
    MatchCase clone(const std::map<std::string, OType>& typeMap = {}) const {
        return MatchCase(Pattern ? Pattern->clone(typeMap) : nullptr, Body->clone(typeMap));
    }
};

class MatchExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond;
    std::vector<MatchCase> Cases;
public:
    MatchExprAST(std::unique_ptr<ExprAST> Cond, std::vector<MatchCase> Cases)
        : Cond(std::move(Cond)), Cases(std::move(Cases)) {}
    
    llvm::Value *codegen() override;
    OType getOType() const override { 
        if (!Cases.empty() && Cases[0].Body) return Cases[0].Body->getOType();
        return OType(BaseType::Void);
    }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        std::vector<MatchCase> NewCases;
        for (const auto& c : Cases) NewCases.push_back(c.clone(typeMap));
        return std::make_unique<MatchExprAST>(Cond->clone(typeMap), std::move(NewCases));
    }
};

// 5. Var Declaration Node: var x = 10; or var arr = int[10];
class VarDeclExprAST : public ExprAST {
    std::string Name;
    std::unique_ptr<ExprAST> Init;
    OType ExplicitType; // For explicit type annotations
    bool HasExplicitType;
public:
    VarDeclExprAST(const std::string &Name, std::unique_ptr<ExprAST> Init, 
                   OType ExplicitType = OType(), bool HasExplicitType = false)
        : Name(Name), Init(std::move(Init)), ExplicitType(ExplicitType), HasExplicitType(HasExplicitType) {}
    llvm::Value *codegen() override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<VarDeclExprAST>(Name, Init ? Init->clone(typeMap) : nullptr, ExplicitType.substitute(typeMap), HasExplicitType);
    }
};

// 6. Assignment Node: x = 20; or arr[i] = 20;
class AssignmentExprAST : public ExprAST {
    std::unique_ptr<ExprAST> LHS;
    std::unique_ptr<ExprAST> RHS;
public:
    AssignmentExprAST(std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
        : LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    llvm::Value *codegen() override;
    OType getOType() const override { return RHS->getOType(); }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<AssignmentExprAST>(LHS->clone(typeMap), RHS->clone(typeMap));
    }
};

// 7. Return Node: return x;
class ReturnExprAST : public ExprAST {
    std::unique_ptr<ExprAST> RetVal;
public:
    ReturnExprAST(std::unique_ptr<ExprAST> RetVal)
        : RetVal(std::move(RetVal)) {}
    llvm::Value *codegen() override;
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<ReturnExprAST>(RetVal ? RetVal->clone(typeMap) : nullptr);
    }
};

// 9. Delete Node
class DeleteExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Operand;
public:
    DeleteExprAST(std::unique_ptr<ExprAST> Operand)
        : Operand(std::move(Operand)) {}
    llvm::Value *codegen() override;
    OType getOType() const override { return OType(BaseType::Void); }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<DeleteExprAST>(Operand->clone(typeMap));
    }
};

// 10. Negate Node: -x
class NegateExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Operand;
public:
    NegateExprAST(std::unique_ptr<ExprAST> Operand)
        : Operand(std::move(Operand)) {}
    llvm::Value *codegen() override;
    OType getOType() const override { return Operand->getOType(); }
    std::unique_ptr<ExprAST> clone(const std::map<std::string, OType>& typeMap = {}) const override {
        return std::make_unique<NegateExprAST>(Operand->clone(typeMap));
    }
};

// 8. Updated Prototype to store types
class PrototypeAST {
    std::string Name;
    std::vector<std::pair<std::string, OType>> Args; // Name + Type
    OType ReturnType;
public:
    PrototypeAST(const std::string &Name, 
                 std::vector<std::pair<std::string, OType>> Args, 
                 OType RetType)
        : Name(Name), Args(std::move(Args)), ReturnType(RetType) {}

    llvm::Function *codegen();
    const std::string &getName() const { return Name; }
    void setName(const std::string &NewName) { Name = NewName; }
    void injectThisParameter(const std::string &StructName);
    const std::vector<std::pair<std::string, OType>>& getArgs() const { return Args; }
    OType getReturnType() const { return ReturnType; }
    
    std::unique_ptr<PrototypeAST> clone(const std::map<std::string, OType>& typeMap = {}) const {
        std::vector<std::pair<std::string, OType>> NewArgs;
        for(const auto& arg : Args) NewArgs.push_back(std::make_pair(arg.first, arg.second.substitute(typeMap)));
        return std::make_unique<PrototypeAST>(Name, NewArgs, ReturnType.substitute(typeMap));
    }
};

/// FunctionAST - Represents a full function definition (Proto + Body).
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body; // This will eventually be a BlockAST
public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) {}
    llvm::Function *codegen();
    PrototypeAST* getPrototype() { return Proto.get(); }
    ExprAST* getBody() { return Body.get(); }

    std::unique_ptr<FunctionAST> clone(const std::map<std::string, OType>& typeMap = {}) const {
        return std::make_unique<FunctionAST>(Proto->clone(typeMap), Body ? Body->clone(typeMap) : nullptr);
    }
};

inline std::unique_ptr<StructDeclAST> StructDeclAST::clone(const std::map<std::string, OType>& typeMap) const {
    std::vector<std::unique_ptr<FunctionAST>> NewMethods;
    for(const auto& m : Methods) NewMethods.push_back(m->clone(typeMap));
    std::vector<std::unique_ptr<ConstructorAST>> NewConstructors;
    for(const auto& c : Constructors) NewConstructors.push_back(c->clone(typeMap));
    
    std::vector<std::pair<std::string, OType>> NewFields;
    for(const auto& f : Fields) NewFields.push_back(std::make_pair(f.first, f.second.substitute(typeMap)));
    
    return std::make_unique<StructDeclAST>(Name, GenericParams, NewFields, std::move(NewMethods), std::move(NewConstructors));
}

inline std::unique_ptr<ClassDeclAST> ClassDeclAST::clone(const std::map<std::string, OType>& typeMap) const {
    std::vector<std::unique_ptr<FunctionAST>> NewMethods;
    for(const auto& m : Methods) NewMethods.push_back(m->clone(typeMap));
    std::vector<std::unique_ptr<ConstructorAST>> NewConstructors;
    for(const auto& c : Constructors) NewConstructors.push_back(c->clone(typeMap));

    std::vector<std::pair<std::string, OType>> NewFields;
    for(const auto& f : Fields) NewFields.push_back(std::make_pair(f.first, f.second.substitute(typeMap)));

    return std::make_unique<ClassDeclAST>(Name, ParentName, IsOpen, NewFields, std::move(NewMethods), std::move(NewConstructors), VirtualMethods);
}

// Forward declaration for the global registry functions
void RegisterFunctionProto(std::unique_ptr<PrototypeAST> Proto);
llvm::Function *GetFunctionFromPrototype(std::string Name);