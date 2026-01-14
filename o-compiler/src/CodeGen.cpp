#include "codegen/CodeGenerator.h"
#include "codegen/ExpressionCodeGen.h"
#include "codegen/FunctionCodeGen.h"
#include "codegen/TypeCodeGen.h"
#include "codegen/UtilityCodeGen.h"
#include "codegen/TypeResolver.h"
#include "codegen/InstantiationManager.h"
#include "codegen/SemanticWalker.h"

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

llvm::Value *NotExprAST::codegen() {
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

llvm::Value *UnresolvedNewExprAST::codegen() {
    if (!GlobalCodeGen) return nullptr;
    return GlobalCodeGen->exprCodeGen->codegen(*this);
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
    // Only generate function code if not in import context
    if (!GlobalCodeGen->inImportContext) {
        return GlobalCodeGen->funcCodeGen->codegen(*this);
    }
    return nullptr; // Just return null during import
}

void StructDeclAST::codegen() {
    if (!GlobalCodeGen) return;
    // First, resolve semantics (no LLVM IR generation) - always do this
    GlobalCodeGen->typeResolver->resolve(*this);
    // Then, generate LLVM IR - only if not in import context
    if (!GlobalCodeGen->inImportContext) {
        GlobalCodeGen->typeCodeGen->codegen(*this);
    }
}

void ClassDeclAST::codegen() {
    if (!GlobalCodeGen) return;
    // First, resolve semantics (no LLVM IR generation) - always do this
    GlobalCodeGen->typeResolver->resolve(*this);
    // Then, generate LLVM IR - only if not in import context
    if (!GlobalCodeGen->inImportContext) {
        GlobalCodeGen->typeCodeGen->codegen(*this);
    }
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

    // If not found, try with corrected method name (workaround for method name mangling issue)
    std::string correctedMethodName = MethodName;
    if (MethodName.length() > 4) { // At least "int_"
        // Check if it starts with a common type name followed by "_"
        if (MethodName.substr(0, 4) == "int_") {
            correctedMethodName = MethodName.substr(4); // Remove "int_"
        } else if (MethodName.substr(0, 5) == "bool_") {
            correctedMethodName = MethodName.substr(5); // Remove "bool_"
        } else if (MethodName.substr(0, 6) == "float_") {
            correctedMethodName = MethodName.substr(6); // Remove "float_"
        } else if (MethodName.substr(0, 5) == "char_") {
            correctedMethodName = MethodName.substr(5); // Remove "char_"
        } else if (MethodName.substr(0, 5) == "byte_") {
            correctedMethodName = MethodName.substr(5); // Remove "byte_"
        }
    }

    // If we corrected the method name, try looking up with the corrected name
    if (correctedMethodName != MethodName) {
        std::string correctedMangledName = StructName + "_" + correctedMethodName;
        if (GlobalCodeGen->FunctionReturnTypes.count(correctedMangledName))
            return GlobalCodeGen->FunctionReturnTypes.at(correctedMangledName);
    }

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
            OType pointeeType = ObjType.getPointeeType();
            StructName = pointeeType.structName;

            // Handle Generics for pointer types
            if (!pointeeType.genericArgs.empty()) {
                StructName = GlobalCodeGen->utilCodeGen->mangleGenericName(pointeeType.structName, pointeeType.genericArgs);
            }
        } else {
            StructName = ObjType.structName;

            // Handle Generics for value types
            if (!ObjType.genericArgs.empty()) {
                StructName = GlobalCodeGen->utilCodeGen->mangleGenericName(ObjType.structName, ObjType.genericArgs);
            }
        }

        // Ensure the struct is properly instantiated if it's generic
        if (!ObjType.genericArgs.empty()) {
            GlobalCodeGen->utilCodeGen->instantiateStruct(ObjType.structName, ObjType.genericArgs);
            StructName = GlobalCodeGen->utilCodeGen->mangleGenericName(ObjType.structName, ObjType.genericArgs);
        } else if (ObjType.isPointer()) {
            OType pointeeType = ObjType.getPointeeType();
            if (!pointeeType.genericArgs.empty()) {
                GlobalCodeGen->utilCodeGen->instantiateStruct(pointeeType.structName, pointeeType.genericArgs);
                StructName = GlobalCodeGen->utilCodeGen->mangleGenericName(pointeeType.structName, pointeeType.genericArgs);
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

// New three-phase architecture functions
void semanticDiscoveryPhase() {
    if (GlobalCodeGen) {
        // Create a semantic walker to discover all required instantiations
        std::unique_ptr<SemanticWalker> semanticWalker = std::make_unique<SemanticWalker>(*GlobalCodeGen);

        // Process all pending instantiations until fixed point is reached
        while (GlobalCodeGen->instantiationManager->hasPending()) {
            InstantiationRequest req = GlobalCodeGen->instantiationManager->dequeue();

            // Process the instantiation request semantically (no LLVM IR generation)
            if (req.kind == InstantiationRequest::Struct) {
                // Look up the generic struct template
                auto it = GlobalCodeGen->GenericStructRegistry.find(req.baseName);
                if (it != GlobalCodeGen->GenericStructRegistry.end()) {
                    // Instantiate the struct with the given type arguments
                    // This creates the type and function prototypes but not the bodies
                    GlobalCodeGen->utilCodeGen->instantiateStruct(req.baseName, req.typeArgs);
                }
            }

            // Mark this instantiation as processed
            GlobalCodeGen->instantiationManager->markAsInstantiated(req);
        }

        // Freeze the instantiation manager to prevent further additions during codegen
        GlobalCodeGen->instantiationManager->freeze();
    }
}

void validationPhase() {
    // In this phase, we could validate that all required instantiations were successful
    // For now, we'll just ensure the instantiation manager is frozen
    if (GlobalCodeGen) {
        assert(GlobalCodeGen->instantiationManager->isFrozen() && "Instantiation manager should be frozen before validation");
    }
}

void codeGenerationPhase() {
    if (GlobalCodeGen) {
        // 1. Generate code for all top-level non-generic functions first
        // This is handled elsewhere in the compilation process

        // 2. Iteratively process the instantiation queue using worklist algorithm
        // This is the "Worklist" algorithm to avoid recursive instantiation issues
        while (GlobalCodeGen->instantiationManager->hasPending()) {
            // Get the next pending instantiation
            InstantiationRequest pendingItem = GlobalCodeGen->instantiationManager->dequeue();

            // Generate bodies for this specific instantiation
            // This might trigger NEW instantiations, which will be added to the queue
            if (pendingItem.kind == InstantiationRequest::Struct) {
                // The instantiated struct should already be in the registry with the mangled name
                std::string mangledName = GlobalCodeGen->utilCodeGen->mangleGenericName(
                    pendingItem.baseName, pendingItem.typeArgs);

                // Look for the instantiated struct in the registry
                auto instantiatedIt = GlobalCodeGen->GenericStructRegistry.find(mangledName);
                if (instantiatedIt != GlobalCodeGen->GenericStructRegistry.end() && instantiatedIt->second) {
                    // Generate the struct and its methods
                    GlobalCodeGen->codegen(*instantiatedIt->second);

                    // Mark this instantiation as processed
                    GlobalCodeGen->instantiationManager->markAsInstantiated(pendingItem);
                }
            }
        }

        // Process any remaining deferred instantiations that were added during the process
        GlobalCodeGen->processDeferredInstantiations();
    }
}