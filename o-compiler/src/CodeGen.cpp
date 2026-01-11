#include "codegen/CodeGenerator.h"
#include "codegen/ExpressionCodeGen.h"
#include "codegen/FunctionCodeGen.h"
#include "codegen/TypeCodeGen.h"
#include "codegen/UtilityCodeGen.h"

// Global Compiler State - still needed for backward compatibility
std::unique_ptr<llvm::LLVMContext> TheContext;
std::unique_ptr<llvm::IRBuilder<>> Builder;
std::unique_ptr<llvm::Module> TheModule;

// Global registry for function prototypes to enable lazy symbol resolution during generic instantiation
std::map<std::string, std::shared_ptr<PrototypeAST>> GlobalFunctionProtos;

// Global instance of the code generator
std::unique_ptr<CodeGenerator> GlobalCodeGen;

// Initialize the LLVM infrastructure
void InitializeModuleAndPassManager() {
    // Initialize the global LLVM objects
    TheContext = std::make_unique<llvm::LLVMContext>();
    TheModule = std::make_unique<llvm::Module>("O_Module", *TheContext);
    Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

    // Initialize the global code generator
    GlobalCodeGen = std::make_unique<CodeGenerator>();

    // Set up the code generator to use the global objects
    GlobalCodeGen->TheContext = TheContext.get();
    GlobalCodeGen->TheModule = TheModule.get();
    GlobalCodeGen->Builder = Builder.get();

    // Initialize the code generator's internal state
    GlobalCodeGen->initializeModuleAndPassManager();
}

// Main code generation functions that delegate to the global code generator
llvm::Value *BoolExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *StringExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *CastExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *NumberExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *VariableExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *VariableExprAST::codegenAddress() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegenAddress(*this);
}

llvm::Value *VarDeclExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *AssignmentExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *ReturnExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *DeleteExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *NegateExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *BinaryExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *CallExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *MethodCallExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *BlockExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *IfExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *WhileExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *ForExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *AddressOfExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *DerefExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *DerefExprAST::codegenAddress() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegenAddress(*this);
}

llvm::Value *NewExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *NewArrayExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *MatchExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *IndexExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *IndexExprAST::codegenAddress() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegenAddress(*this);
}

llvm::Value *MemberAccessAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *MemberAccessAST::codegenAddress() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegenAddress(*this);
}

llvm::Value *ArrayInitExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Value *ArrayLiteralExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
}

llvm::Function *PrototypeAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->funcCodeGen->codegen(*this);
}

llvm::Function *FunctionAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->funcCodeGen->codegen(*this);
}

void StructDeclAST::codegen() {
    if (!GlobalCodeGen) return;
    GlobalCodeGen->typeCodeGen->codegen(*this);
}

void ClassDeclAST::codegen() {
    if (!GlobalCodeGen) return;
    GlobalCodeGen->typeCodeGen->codegen(*this);
}

llvm::Function *ConstructorAST::codegen(const std::string& structName) {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->funcCodeGen->codegen(*this, structName);
}

void PrototypeAST::injectThisParameter(const std::string &StructName) {
    // This method modifies the prototype in place, so we can implement it directly here
    // Create 'this' parameter: StructName* this
    OType thisType = OType(BaseType::Struct, 1, StructName); // Pointer to struct
    std::pair<std::string, OType> thisParam = {"this", thisType};

    // Insert at the beginning of the parameter list
    Args.insert(Args.begin(), thisParam);
}

OType VariableExprAST::getOType() const {
    if (!GlobalCodeGen) return OType(BaseType::Void);
    return GlobalCodeGen->getVariableType(Name);
}

OType BinaryExprAST::getOType() const {
    // Basic inference: assume LHS type
    if (LHS) return LHS->getOType();
    return OType(BaseType::Void);
}

OType CallExprAST::getOType() const {
    if (!GlobalCodeGen) return OType(BaseType::Void);
    if (GlobalCodeGen->FunctionReturnTypes.count(Callee)) return GlobalCodeGen->FunctionReturnTypes.at(Callee);
    return OType(BaseType::Void);
}

OType MethodCallExprAST::getOType() const {
    if (!GlobalCodeGen) return OType(BaseType::Void);
    
    OType ObjType = Object->getOType();
    std::string StructName = ObjType.structName;

    if (!ObjType.genericArgs.empty()) {
        if (GlobalCodeGen) {
            StructName = GlobalCodeGen->utilCodeGen->mangleGenericName(StructName, ObjType.genericArgs);
        }
    }

    std::string MangledName = StructName + "_" + MethodName;
    if (GlobalCodeGen->FunctionReturnTypes.count(MangledName)) return GlobalCodeGen->FunctionReturnTypes.at(MangledName);
    return OType(BaseType::Void);
}

OType NewArrayExprAST::getOType() const {
    // Return slice type
    std::vector<int> sizes = ElementType.arraySizes;
    sizes.insert(sizes.begin(), -1); // Prepend -1 for slice
    return OType(ElementType.base, ElementType.pointerDepth, ElementType.structName, sizes);
}

OType IndexExprAST::getOType() const {
    OType ArrType = Array->getOType();
    OType elemType = ArrType.getElementType();
    return elemType;
}

OType DerefExprAST::getOType() const {
    OType PtrType = Operand->getOType();
    return PtrType.getPointeeType();
}

OType AddressOfExprAST::getOType() const {
    OType OpType = Operand->getOType();
    return OpType.getPointerTo();
}

OType MemberAccessAST::getOType() const {
    if (!GlobalCodeGen) return OType(BaseType::Void);
    
    OType ObjType = Object->getOType();
    if (ObjType.base == BaseType::Struct || (ObjType.isPointer() && ObjType.getPointeeType().base == BaseType::Struct)) {
        // Handle Generics (similar to codegenAddress)
        std::string StructName;

        if (ObjType.isPointer()) {
            StructName = ObjType.getPointeeType().structName;
        } else {
            StructName = ObjType.structName;
        }

        if (!ObjType.genericArgs.empty()) {
            if (GlobalCodeGen) {
                StructName = GlobalCodeGen->utilCodeGen->mangleGenericName(StructName, ObjType.genericArgs);
            }
        } else if (ObjType.isPointer()) {
            // For pointer types, check if the pointee type has generic args
            OType pointeeType = ObjType.getPointeeType();
            if (!pointeeType.genericArgs.empty()) {
                if (GlobalCodeGen) {
                    StructName = GlobalCodeGen->utilCodeGen->mangleGenericName(pointeeType.structName, pointeeType.genericArgs);
                }
            }
        }

        // Lookup field type
        if (TypeRegistry::getInstance().hasStruct(StructName)) {
            const StructInfo& info = TypeRegistry::getInstance().getStruct(StructName);
            for (const auto& field : info.fields) {
                if (field.name == FieldName) {
                    // For instantiated generic structs, the field type should already be substituted
                    return field.type;
                }
            }
        }
    }
    // Handle Array/Slice len
    if (ObjType.isArray() && (FieldName == "len" || FieldName == "length")) {
        return OType(BaseType::Int);
    }
    // Handle Slice ptr
    if (ObjType.isArray() && ObjType.getArrayNumElements() == -1 && FieldName == "ptr") {
        OType ptrType = ObjType;
        ptrType.arraySizes.clear();
        ptrType.pointerDepth += 1;
        return ptrType;
    }
    return OType(BaseType::Void);
}

// Helper function implementations that were originally in CodeGen.cpp
void RegisterFunctionProto(std::unique_ptr<PrototypeAST> Proto) {
    if (GlobalCodeGen) {
        GlobalCodeGen->utilCodeGen->RegisterFunctionProto(std::move(Proto));
    }
}

llvm::Function *GetFunctionFromPrototype(std::string Name) {
    if (GlobalCodeGen) {
        return GlobalCodeGen->utilCodeGen->getFunctionFromPrototype(Name);
    }
    return nullptr;
}

void processDeferredInstantiations() {
    if (GlobalCodeGen) {
        GlobalCodeGen->processDeferredInstantiations();
    }
}