#include "AST.h"
#include <map>

// --- Global Compiler State ---
std::unique_ptr<llvm::LLVMContext> TheContext;
std::unique_ptr<llvm::IRBuilder<>> Builder;
std::unique_ptr<llvm::Module> TheModule;

// Scope Management
struct ScopeLayer {
    std::map<std::string, llvm::AllocaInst *> values;
    std::map<std::string, OType> types;
    std::vector<llvm::AllocaInst *> cleanupVars; // Variables to free on exit
};

std::vector<ScopeLayer> ScopeStack;
std::map<std::string, OType> FunctionReturnTypes;

std::unordered_map<std::string, llvm::StructType*> StructTypes;
// Generic Struct Registry for Monomorphization
std::map<std::string, std::unique_ptr<StructDeclAST>> GenericStructRegistry;

llvm::Function *getFreeFunc() {
    llvm::Function *FreeF = TheModule->getFunction("free");
    if (!FreeF) {
        std::vector<llvm::Type*> Args;
        Args.push_back(llvm::PointerType::get(*TheContext, 0)); // void* (opaque ptr)
        llvm::FunctionType *FT = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*TheContext), Args, false);
        FreeF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "free", TheModule.get());
    }
    return FreeF;
}

void EnterScope() {
    ScopeStack.push_back(ScopeLayer());
}

// Helper to emit cleanup code for a scope layer
void EmitCleanup(const ScopeLayer& layer) {
    // Check if the current block is terminated. If so, we can't emit instructions.
    // However, for Return statements, we might be emitting before the ret?
    // This helper just appends to the current insert block.
    // Caller must ensure insertion point is valid.
    
    if (Builder->GetInsertBlock()->getTerminator()) return;

    for (auto* alloca : layer.cleanupVars) {
        // Load the struct
        llvm::Value* sliceVal = Builder->CreateLoad(alloca->getAllocatedType(), alloca, "cleanup_load");
        
        // Extract the pointer (index 1)
        llvm::Value* ptrVal = Builder->CreateExtractValue(sliceVal, 1, "cleanup_ptr");
        
        // Cast to ptr for free (opaque pointers make this simple, but bitcast keeps it explicit if needed)
        llvm::Value* voidPtr = ptrVal; 
        if (ptrVal->getType() != llvm::PointerType::get(*TheContext, 0)) {
             voidPtr = Builder->CreateBitCast(ptrVal, llvm::PointerType::get(*TheContext, 0), "cleanup_cast");
        }
        
        // Call free
        Builder->CreateCall(getFreeFunc(), {voidPtr});
    }
}

void ExitScope() {
    if (!ScopeStack.empty()) {
        auto& layer = ScopeStack.back();
        EmitCleanup(layer);
        ScopeStack.pop_back();
    } else {
        fprintf(stderr, "Error: Scope stack underflow\n");
    }
}

void RegisterCleanup(llvm::AllocaInst* var) {
    if (ScopeStack.empty()) EnterScope();
    ScopeStack.back().cleanupVars.push_back(var);
}

void AddVariable(const std::string& name, llvm::AllocaInst* val) {
    if (ScopeStack.empty()) EnterScope();
    ScopeStack.back().values[name] = val;
}

llvm::AllocaInst* GetVariable(const std::string& name) {
    for (auto it = ScopeStack.rbegin(); it != ScopeStack.rend(); ++it) {
        if (it->values.count(name)) {
            return it->values.at(name);
        }
    }
    return nullptr;
}

void AddVariableType(const std::string& name, OType type) {
    if (ScopeStack.empty()) EnterScope();
    ScopeStack.back().types[name] = type;
}

OType GetVariableType(const std::string& name) {
    for (auto it = ScopeStack.rbegin(); it != ScopeStack.rend(); ++it) {
        if (it->types.count(name)) {
            return it->types.at(name);
        }
    }
    return OType(BaseType::Void);
}


// Initialize the LLVM infrastructure
void InitializeModuleAndPassManager() {
    TheContext = std::make_unique<llvm::LLVMContext>();
    TheModule = std::make_unique<llvm::Module>("O_Module", *TheContext);
    Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
}

void LogError(const char *Str) {
    fprintf(stderr, "Error: %s\n", Str);
}

// Forward declaration
llvm::StructType* getSliceType(llvm::Type* ElementType);

std::string mangleGenericName(const std::string& baseName, const std::vector<OType>& args) {
    std::string name = baseName;
    for (const auto& arg : args) {
        name += "_";
        for (int i=0; i<arg.pointerDepth; ++i) name += "ptr_"; // Add ptr prefix
        
        if (arg.base == BaseType::Int) name += "int";
        else if (arg.base == BaseType::Float) name += "float";
        else if (arg.base == BaseType::Bool) name += "bool";
        else if (arg.base == BaseType::Char) name += "char";
        else if (arg.base == BaseType::Byte) name += "byte";
        else if (arg.base == BaseType::Struct) {
             name += arg.structName;
        }
        else name += "void";
    }
    return name;
}

void instantiateStruct(const std::string& genericName, const std::vector<OType>& typeArgs) {
    if (GenericStructRegistry.count(genericName) == 0) return;
    
    std::string mangledName = mangleGenericName(genericName, typeArgs);
    if (StructTypes.count(mangledName)) return; // Already instantiated
    
    const auto& genericAST = GenericStructRegistry[genericName];
    if (genericAST->getGenericParams().size() != typeArgs.size()) {
        LogError(("Incorrect number of type arguments for " + genericName).c_str());
        return;
    }
    
    std::map<std::string, OType> typeMap;
    for (size_t i = 0; i < genericAST->getGenericParams().size(); ++i) {
        typeMap[genericAST->getGenericParams()[i]] = typeArgs[i];
    }
    
    auto newAST = genericAST->clone(typeMap);
    newAST->setName(mangledName);
    newAST->makeConcrete();
    
    // Save Context (Builder and Scope)
    llvm::BasicBlock *SavedInsertBlock = Builder->GetInsertBlock();
    auto SavedScopeStack = ScopeStack;
    
    newAST->codegen();
    
    // Restore Context
    ScopeStack = SavedScopeStack;
    if (SavedInsertBlock) {
        Builder->SetInsertPoint(SavedInsertBlock);
    }
}

// Helper to translate OType to llvm::Type
llvm::Type* getLLVMType(const OType& t) {
    llvm::Type* baseType;
    
    switch (t.base) {
        case BaseType::Int: baseType = llvm::Type::getInt32Ty(*TheContext); break;
        case BaseType::Float: baseType = llvm::Type::getDoubleTy(*TheContext); break;
        case BaseType::Bool: baseType = llvm::Type::getInt1Ty(*TheContext); break;
        case BaseType::Char: baseType = llvm::Type::getInt8Ty(*TheContext); break;
        case BaseType::Byte: baseType = llvm::Type::getInt8Ty(*TheContext); break;
        case BaseType::Struct: 
            if (StructTypes.find(t.structName) != StructTypes.end()) {
                baseType = StructTypes[t.structName];
            } else if (!t.genericArgs.empty()) {
                instantiateStruct(t.structName, t.genericArgs);
                std::string mangled = mangleGenericName(t.structName, t.genericArgs);
                if (StructTypes.count(mangled)) {
                    baseType = StructTypes[mangled];
                } else {
                    baseType = llvm::Type::getVoidTy(*TheContext);
                }
            } else {
                baseType = llvm::Type::getVoidTy(*TheContext);
            }
            break;
        default: baseType = llvm::Type::getVoidTy(*TheContext); break;
    }
    
    // Handle Arrays (Inner to Outer)
    for (auto it = t.arraySizes.rbegin(); it != t.arraySizes.rend(); ++it) {
        int size = *it;
        if (size == -1) {
            // Slice
            baseType = getSliceType(baseType);
        } else {
            // Fixed Array
            baseType = llvm::ArrayType::get(baseType, size);
        }
    }
    
    // Handle Pointers
    for (int i = 0; i < t.pointerDepth; ++i) {
        baseType = llvm::PointerType::get(*TheContext, 0);
    }
    
    return baseType;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of the function.
static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                          const std::string &VarName, llvm::Type* Type) {
    llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                     TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type, nullptr, VarName);
}

// --- Code Generation Implementations ---

llvm::Value *BoolExprAST::codegen() {
    return llvm::ConstantInt::get(*TheContext, llvm::APInt(1, Val ? 1 : 0, false));
}

llvm::Value *StringExprAST::codegen() {
    // Create global constant array for string
    std::string content = Val + '\0'; // Null-terminate
    llvm::Constant *strConstant = llvm::ConstantDataArray::getString(*TheContext, content, false);
    
    // Create global variable
    llvm::GlobalVariable *globalStr = new llvm::GlobalVariable(
        *TheModule,
        strConstant->getType(),
        true, // isConstant
        llvm::GlobalValue::PrivateLinkage,
        strConstant,
        "str"
    );
    
    // Return pointer to first character (i8*)
    llvm::Value *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
    return Builder->CreateInBoundsGEP(
        strConstant->getType(),
        globalStr,
        {zero, zero},
        "strptr"
    );
}

llvm::Value *NumberExprAST::codegen() {
    if (Type.base == BaseType::Float) {
        return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Val));
    } else if (Type.base == BaseType::Char || Type.base == BaseType::Byte) {
        return llvm::ConstantInt::get(*TheContext, llvm::APInt(8, (int)Val, false));
    } else {
        return llvm::ConstantInt::get(*TheContext, llvm::APInt(32, (int)Val, true));
    }
}

llvm::Value *VariableExprAST::codegen() {
    llvm::AllocaInst *A = GetVariable(Name);
    if (!A) {
        LogError("Unknown variable name");
        return nullptr;
    }
    return Builder->CreateLoad(A->getAllocatedType(), A, Name.c_str());
}

llvm::Value *VariableExprAST::codegenAddress() {
    llvm::AllocaInst *A = GetVariable(Name);
    if (!A) {
        LogError("Unknown variable name");
        return nullptr;
    }
    return A; // Return the alloca directly (address) without loading
}

llvm::Value *VarDeclExprAST::codegen() {
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    
    llvm::Type *AllocaType = nullptr;
    llvm::Value *InitVal = nullptr;
    
    if (Init) {
        InitVal = Init->codegen();
        if (!InitVal) return nullptr;
        AllocaType = InitVal->getType();
    }
    
    // If explicit type is provided, use it
    if (HasExplicitType) {
        llvm::Type *ExpectedType = getLLVMType(ExplicitType);
        if (InitVal && ExpectedType && ExpectedType != AllocaType) {
            // Check if we can implicitly cast
            if (InitVal->getType()->isPointerTy() && ExpectedType->isArrayTy()) {
                // This might be the ArrayInit returning pointer case?
                // But generally types should match
            } else {
                 LogError("Type mismatch in variable declaration");
                 return nullptr;
            }
        }
        if (ExpectedType) AllocaType = ExpectedType;
    }
    
    if (!AllocaType) {
        LogError("Cannot infer type for variable declaration");
        return nullptr;
    }
    
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Name, AllocaType);
    
    if (InitVal) {
        Builder->CreateStore(InitVal, Alloca);
    } else {
        // Zero initialization for safety
        llvm::Value *Zero = llvm::Constant::getNullValue(AllocaType);
        Builder->CreateStore(Zero, Alloca);
    }
    
    AddVariable(Name, Alloca);
    
    OType VarType;
    if (HasExplicitType) VarType = ExplicitType;
    else if (Init) VarType = Init->getOType();
    
    AddVariableType(Name, VarType);
    
    // RAII Registration: If type is Slice (Dynamic Array), register for cleanup
    if (VarType.isArray() && VarType.getArrayNumElements() == -1) {
        RegisterCleanup(Alloca);
    }
    
    return Alloca; // Return the address
}

llvm::Value *AssignmentExprAST::codegen() {
    llvm::Value *Val = RHS->codegen();
    if (!Val) return nullptr;
    
    llvm::Value *Ptr = LHS->codegenAddress();
    if (!Ptr) {
        LogError("Left-hand side of assignment must be an l-value (variable, array index, dereference)");
        return nullptr;
    }
    
    // Check types and cast if needed (e.g. char to int promotion in some cases, or int to byte)
    // For now, assume types match or rely on LLVM to complain/cast implicitly if compatible
    
    Builder->CreateStore(Val, Ptr);
    return Val;
}

llvm::Value *ReturnExprAST::codegen() {
    llvm::Value *V = nullptr;
    if (RetVal) {
        V = RetVal->codegen();
        if (!V) return nullptr;
    }
    
    // RAII: Cleanup all scopes before returning
    // Iterate in reverse (innermost to outermost)
    for (auto it = ScopeStack.rbegin(); it != ScopeStack.rend(); ++it) {
        EmitCleanup(*it);
    }

    if (RetVal) {
        Builder->CreateRet(V);
    } else {
        Builder->CreateRetVoid();
        V = llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext)); // Dummy
    }
    return V;
}

llvm::Value *DeleteExprAST::codegen() {
    llvm::Value *Val = Operand->codegen();
    if (!Val) return nullptr;
    
    // If we are deleting a variable, we must remove it from the RAII cleanup list to avoid double-free
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(Operand.get())) {
        llvm::AllocaInst* Alloca = GetVariable(VarExpr->getName());
        if (Alloca) {
            // Search from top of stack down
            for (auto it = ScopeStack.rbegin(); it != ScopeStack.rend(); ++it) {
                auto& cleanups = it->cleanupVars;
                auto found = std::find(cleanups.begin(), cleanups.end(), Alloca);
                if (found != cleanups.end()) {
                    cleanups.erase(found);
                    break; // Found and removed
                }
            }
        }
    }
    
    // Val is likely a pointer (or a Slice struct if it was a variable load)
    // Wait, VariableExprAST::codegen() loads the value.
    // If it's a slice (struct), we need the pointer inside.
    
    llvm::Value* PtrToDelete = Val;
    if (Val->getType()->isStructTy()) {
        // Assume Slice { size, ptr }
        if (Val->getType()->getStructNumElements() == 2) {
             PtrToDelete = Builder->CreateExtractValue(Val, 1, "delete_ptr");
        }
    }
    
    if (!PtrToDelete->getType()->isPointerTy()) {
        LogError("Cannot delete non-pointer type");
        return nullptr;
    }
    
    // Cast to opaque ptr if needed (LLVM 18 uses ptr)
    if (PtrToDelete->getType() != llvm::PointerType::get(*TheContext, 0)) {
        PtrToDelete = Builder->CreateBitCast(PtrToDelete, llvm::PointerType::get(*TheContext, 0), "delete_cast");
    }
    
    Builder->CreateCall(getFreeFunc(), {PtrToDelete});
    
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Value *NegateExprAST::codegen() {
    llvm::Value *Val = Operand->codegen();
    if (!Val) return nullptr;
    
    if (Val->getType()->isDoubleTy()) {
        return Builder->CreateFNeg(Val, "negtmp");
    } else if (Val->getType()->isIntegerTy()) {
        return Builder->CreateNeg(Val, "negtmp");
    }
    
    LogError("Invalid type for negation");
    return nullptr;
}

llvm::Value *BinaryExprAST::codegen() {
    // Handle short-circuiting logical operators
    if (Op == "&&" || Op == "||") {
        llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
        
        llvm::BasicBlock *LHSBlock = Builder->GetInsertBlock();
        llvm::BasicBlock *RHSBlock = llvm::BasicBlock::Create(*TheContext, "rhs", TheFunction);
        llvm::BasicBlock *MergeBlock = llvm::BasicBlock::Create(*TheContext, "merge", TheFunction);
        
        // Evaluate LHS
        llvm::Value *L = LHS->codegen();
        if (!L) return nullptr;
        
        // Convert to boolean if needed
        if (!L->getType()->isIntegerTy(1)) {
            if (L->getType()->isDoubleTy()) {
                L = Builder->CreateFCmpONE(L, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "tobool");
            } else {
                L = Builder->CreateICmpNE(L, llvm::ConstantInt::get(L->getType(), 0), "tobool");
            }
        }
        
        if (Op == "&&") {
            // For &&: if LHS is false, skip RHS
            Builder->CreateCondBr(L, RHSBlock, MergeBlock);
        } else { // ||
            // For ||: if LHS is true, skip RHS
            Builder->CreateCondBr(L, MergeBlock, RHSBlock);
        }
        
        // RHS Block
        Builder->SetInsertPoint(RHSBlock);
        llvm::Value *R = RHS->codegen();
        if (!R) return nullptr;
        
        // Convert RHS to boolean if needed
        if (!R->getType()->isIntegerTy(1)) {
            if (R->getType()->isDoubleTy()) {
                R = Builder->CreateFCmpONE(R, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "tobool");
            } else {
                R = Builder->CreateICmpNE(R, llvm::ConstantInt::get(R->getType(), 0), "tobool");
            }
        }
        
        Builder->CreateBr(MergeBlock);
        RHSBlock = Builder->GetInsertBlock(); // Update in case RHS created new blocks
        
        // Merge Block
        Builder->SetInsertPoint(MergeBlock);
        llvm::PHINode *PHI = Builder->CreatePHI(llvm::Type::getInt1Ty(*TheContext), 2, "logictmp");
        
        if (Op == "&&") {
            PHI->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), 0), LHSBlock); // false from LHS
            PHI->addIncoming(R, RHSBlock); // RHS result
        } else { // ||
            PHI->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), 1), LHSBlock); // true from LHS
            PHI->addIncoming(R, RHSBlock); // RHS result
        }
        
        return PHI;
    }
    
    // Regular binary operators
    llvm::Value *L = LHS->codegen();
    llvm::Value *R = RHS->codegen();
    if (!L || !R) return nullptr;

    // Handle type promotion: char/byte to int for arithmetic
    if (L->getType()->isIntegerTy(8) && R->getType()->isIntegerTy(32)) {
        L = Builder->CreateZExt(L, llvm::Type::getInt32Ty(*TheContext), "promote");
    } else if (L->getType()->isIntegerTy(32) && R->getType()->isIntegerTy(8)) {
        R = Builder->CreateZExt(R, llvm::Type::getInt32Ty(*TheContext), "promote");
    } else if (L->getType()->isIntegerTy(8) && R->getType()->isIntegerTy(8)) {
        L = Builder->CreateZExt(L, llvm::Type::getInt32Ty(*TheContext), "promote");
        R = Builder->CreateZExt(R, llvm::Type::getInt32Ty(*TheContext), "promote");
    }

    if (L->getType() != R->getType()) {
        LogError("Type mismatch in binary expression");
        return nullptr;
    }

    bool isFloat = L->getType()->isDoubleTy();

    if (Op == "+") {
        return isFloat ? Builder->CreateFAdd(L, R, "addtmp") : Builder->CreateAdd(L, R, "addtmp");
    } else if (Op == "-") {
        return isFloat ? Builder->CreateFSub(L, R, "subtmp") : Builder->CreateSub(L, R, "subtmp");
    } else if (Op == "*") {
        return isFloat ? Builder->CreateFMul(L, R, "multmp") : Builder->CreateMul(L, R, "multmp");
    } else if (Op == "/") {
        return isFloat ? Builder->CreateFDiv(L, R, "divtmp") : Builder->CreateSDiv(L, R, "divtmp");
    } else if (Op == "%") {
        return isFloat ? Builder->CreateFRem(L, R, "remtmp") : Builder->CreateSRem(L, R, "remtmp");
    } else if (Op == "<") {
        return isFloat ? Builder->CreateFCmpOLT(L, R, "cmptmp") : Builder->CreateICmpSLT(L, R, "cmptmp");
    } else if (Op == ">") {
        return isFloat ? Builder->CreateFCmpOGT(L, R, "cmptmp") : Builder->CreateICmpSGT(L, R, "cmptmp");
    } else if (Op == "<=") {
        return isFloat ? Builder->CreateFCmpOLE(L, R, "cmptmp") : Builder->CreateICmpSLE(L, R, "cmptmp");
    } else if (Op == ">=") {
        return isFloat ? Builder->CreateFCmpOGE(L, R, "cmptmp") : Builder->CreateICmpSGE(L, R, "cmptmp");
    } else if (Op == "==") {
        return isFloat ? Builder->CreateFCmpOEQ(L, R, "cmptmp") : Builder->CreateICmpEQ(L, R, "cmptmp");
    } else if (Op == "!=") {
        return isFloat ? Builder->CreateFCmpONE(L, R, "cmptmp") : Builder->CreateICmpNE(L, R, "cmptmp");
    }
    
    return nullptr;
}

llvm::Value *CallExprAST::codegen() {
    llvm::Function *CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF) {
        LogError("Unknown function referenced");
        return nullptr;
    }

    if (CalleeF->arg_size() != Args.size()) {
        LogError("Incorrect # arguments passed");
        return nullptr;
    }

    std::vector<llvm::Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        llvm::Value *ArgVal = Args[i]->codegen();
        if (!ArgVal) return nullptr;
        
        // Implicit Cast: Fixed Array -> Slice
        // Param Type
        llvm::Type *ParamType = CalleeF->getArg(i)->getType();
        llvm::Type *ArgType = ArgVal->getType();
        
        // Check if Param is Slice ({i32, T*})
        bool IsParamSlice = false;
        if (ParamType->isStructTy() && !ParamType->getStructName().starts_with("class.") && !ParamType->getStructName().starts_with("struct.")) {
            if (ParamType->getStructNumElements() == 2 && 
                ParamType->getStructElementType(0)->isIntegerTy(32) &&
                ParamType->getStructElementType(1)->isPointerTy()) {
                IsParamSlice = true;
            }
        }
        
        // Check if Arg is Fixed Array Value (Load from Alloca<[N x T]>) -> [N x T]
        // Wait, VariableExprAST loads the value. So ArgVal is [N x T].
        if (IsParamSlice && ArgType->isArrayTy()) {
             // We need the address of the array to create the pointer.
             // But ArgVal is the *value* (loaded). We can't take address of value easily without Alloca.
             // However, VariableExprAST::codegen() does CreateLoad.
             // If we could get the address...
             
             // Trick: If we have the value, we can store it to a temp alloca, then GEP.
             // Or, better: CallExprAST should ask for address if needed? No, too complex refactor.
             
             // Store the array value to a temp stack slot to get an address
             llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
             llvm::AllocaInst *TempAlloca = CreateEntryBlockAlloca(TheFunction, "tmparray", ArgType);
             Builder->CreateStore(ArgVal, TempAlloca);
             
             // Now decay: Get pointer to first element
             uint64_t ArraySize = ArgType->getArrayNumElements();
             
             llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
             std::vector<llvm::Value*> Indices = {Zero, Zero};
             
             llvm::Value *DecayedPtr = Builder->CreateInBoundsGEP(
                 ArgType,
                 TempAlloca,
                 Indices,
                 "decayed_ptr"
             );
             
             // Create Slice
             llvm::Value *Slice = llvm::UndefValue::get(ParamType);
             
             llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), ArraySize);
             Slice = Builder->CreateInsertValue(Slice, SizeVal, 0, "cast_slice_len");
             
             // Bitcast ptr if needed (e.g. if Slice expects i8* but we have i32*)
             // Ideally types match T.
             llvm::Value *TypedPtr = Builder->CreateBitCast(DecayedPtr, ParamType->getStructElementType(1), "cast_slice_ptr");
             Slice = Builder->CreateInsertValue(Slice, TypedPtr, 1, "cast_slice_ptr");
             
             ArgVal = Slice;
        }
        
        ArgsV.push_back(ArgVal);
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Value *MethodCallExprAST::codegen() {
    // 1. Resolve Object
    OType ObjType = Object->getOType();
    std::string StructName = ObjType.structName;
    
    if (StructName.empty()) {
        LogError("Method call on non-struct type");
        return nullptr;
    }
    
    if (!ObjType.genericArgs.empty()) {
        StructName = mangleGenericName(StructName, ObjType.genericArgs);
    }

    std::string MangledName = StructName + "_" + MethodName;
    llvm::Function *CalleeF = TheModule->getFunction(MangledName);
    if (!CalleeF) {
        std::string err = "Unknown method: " + MangledName; 
        LogError(err.c_str());
        return nullptr;
    }

    // 2. Prepare 'this' pointer
    llvm::Value *ThisPtr = nullptr;
    if (ObjType.isPointer()) {
        ThisPtr = Object->codegen();
    } else {
        ThisPtr = Object->codegenAddress();
        if (!ThisPtr) {
            // R-value struct (returned from function, etc). Store to temp.
            llvm::Value *Val = Object->codegen();
            if (!Val) return nullptr;
             llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
             llvm::AllocaInst *TempAlloca = CreateEntryBlockAlloca(TheFunction, "tmp_this", Val->getType());
             Builder->CreateStore(Val, TempAlloca);
             ThisPtr = TempAlloca;
        }
    }
    
    if (!ThisPtr) return nullptr;

    // 3. Prepare Arguments
    std::vector<llvm::Value *> ArgsV;
    ArgsV.push_back(ThisPtr); // Inject 'this'
    
    if (CalleeF->arg_size() != Args.size() + 1) {
        LogError("Incorrect # arguments passed to method");
        return nullptr;
    }

    for (unsigned i = 0, e = Args.size(); i != e; ++i) {
        llvm::Value *ArgVal = Args[i]->codegen();
        if (!ArgVal) return nullptr;
        
        // Implicit Cast: Fixed Array -> Slice
        // Param Type (shifted by 1 for 'this')
        llvm::Type *ParamType = CalleeF->getArg(i + 1)->getType();
        llvm::Type *ArgType = ArgVal->getType();
        
        // Check if Param is Slice ({i32, T*})
        bool IsParamSlice = false;
        if (ParamType->isStructTy() && !ParamType->getStructName().starts_with("class.") && !ParamType->getStructName().starts_with("struct.")) {
            if (ParamType->getStructNumElements() == 2 && 
                ParamType->getStructElementType(0)->isIntegerTy(32) &&
                ParamType->getStructElementType(1)->isPointerTy()) {
                IsParamSlice = true;
            }
        }
        
        if (IsParamSlice && ArgType->isArrayTy()) {
             llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
             llvm::AllocaInst *TempAlloca = CreateEntryBlockAlloca(TheFunction, "tmparray", ArgType);
             Builder->CreateStore(ArgVal, TempAlloca);
             
             uint64_t ArraySize = ArgType->getArrayNumElements();
             
             llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
             std::vector<llvm::Value*> Indices = {Zero, Zero};
             
             llvm::Value *DecayedPtr = Builder->CreateInBoundsGEP(
                 ArgType,
                 TempAlloca,
                 Indices,
                 "decayed_ptr"
             );
             
             llvm::Value *Slice = llvm::UndefValue::get(ParamType);
             
             llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), ArraySize);
             Slice = Builder->CreateInsertValue(Slice, SizeVal, 0, "cast_slice_len");
             
             llvm::Value *TypedPtr = Builder->CreateBitCast(DecayedPtr, ParamType->getStructElementType(1), "cast_slice_ptr");
             Slice = Builder->CreateInsertValue(Slice, TypedPtr, 1, "cast_slice_ptr");
             
             ArgVal = Slice;
        }
        
        ArgsV.push_back(ArgVal);
    }

    // 4. Dynamic Dispatch Check
    int virtualIndex = -1;
    if (TypeRegistry::getInstance().hasStruct(StructName)) {
        const StructInfo& info = TypeRegistry::getInstance().getStruct(StructName);
        for (size_t i = 0; i < info.virtualMethods.size(); ++i) {
            if (info.virtualMethods[i] == MethodName) {
                virtualIndex = i;
                break;
            }
        }
    }
    
    if (virtualIndex != -1) {
        // Dynamic Dispatch
        llvm::Value* Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
        
        // __vptr is at index 0
        llvm::Value* vptrAddr = Builder->CreateInBoundsGEP(
             StructTypes[StructName],
             ThisPtr,
             {Zero, Zero},
             "vptr_addr"
        );
        
        llvm::Type* i8PtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(*TheContext), 0); // i8*
        llvm::Type* vtableType = llvm::PointerType::get(i8PtrType, 0); // i8**
        
        llvm::Value* vtable = Builder->CreateLoad(vtableType, vptrAddr, "vtable");
        
        llvm::Value* funcPtrAddr = Builder->CreateInBoundsGEP(
            i8PtrType,
            vtable,
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), virtualIndex),
            "func_ptr_addr"
        );
        
        llvm::Value* funcVoidPtr = Builder->CreateLoad(i8PtrType, funcPtrAddr, "func_void_ptr");
        
        llvm::FunctionType* funcType = CalleeF->getFunctionType();
        llvm::Value* funcPtr = Builder->CreateBitCast(funcVoidPtr, llvm::PointerType::get(funcType, 0), "func_ptr");
        
        return Builder->CreateCall(funcType, funcPtr, ArgsV, "vcalltmp");
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Value *BlockExprAST::codegen() {
    EnterScope();
    llvm::Value *LastVal = nullptr;
    for (auto &Expr : Expressions) {
        LastVal = Expr->codegen();
        if (!LastVal) {
             ExitScope();
             return nullptr;
        }
        
        // If the block is already terminated (e.g. by return), stop emitting
        if (Builder->GetInsertBlock()->getTerminator()) {
             break;
        }
    }
    ExitScope();
    return LastVal;
}

llvm::Value *IfExprAST::codegen() {
    llvm::Value *CondV = Cond->codegen();
    if (!CondV) return nullptr;

    if (CondV->getType()->isDoubleTy()) {
        CondV = Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "ifcond");
    } else if (CondV->getType()->isIntegerTy()) {
         CondV = Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(CondV->getType(), 0), "ifcond");
    }

    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*TheContext, "then", TheFunction);
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "else");
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ifcont");

    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // Then
    Builder->SetInsertPoint(ThenBB);
    llvm::Value *ThenV = Then->codegen();
    if (!ThenV) return nullptr;

    bool ThenTerminated = (Builder->GetInsertBlock()->getTerminator() != nullptr);
    if (!ThenTerminated) Builder->CreateBr(MergeBB);
    ThenBB = Builder->GetInsertBlock();

    // Else
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);
    llvm::Value *ElseV = nullptr;
    if (Else) {
        ElseV = Else->codegen();
        if (!ElseV) return nullptr;
    } else {
        ElseV = llvm::Constant::getNullValue(ThenV->getType()); 
    }
    
    bool ElseTerminated = (Builder->GetInsertBlock()->getTerminator() != nullptr);
    if (!ElseTerminated) Builder->CreateBr(MergeBB);
    ElseBB = Builder->GetInsertBlock();

    // Merge
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);

    if (ThenTerminated && ElseTerminated) {
        // Both branches return, so MergeBB is unreachable. 
        // We can create a dummy instruction or Unreachable.
        Builder->CreateUnreachable();
        return llvm::Constant::getNullValue(ThenV->getType());
    }

    llvm::PHINode *PN = Builder->CreatePHI(ThenV->getType(), 2, "iftmp");
    if (!ThenTerminated) PN->addIncoming(ThenV, ThenBB);
    if (!ElseTerminated) PN->addIncoming(ElseV, ElseBB);

    return PN;
}

llvm::Value *WhileExprAST::codegen() {
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *LoopCondBB = llvm::BasicBlock::Create(*TheContext, "loopcond", TheFunction);
    llvm::BasicBlock *LoopBodyBB = llvm::BasicBlock::Create(*TheContext, "loopbody");
    llvm::BasicBlock *LoopEndBB = llvm::BasicBlock::Create(*TheContext, "loopend");

    Builder->CreateBr(LoopCondBB);

    Builder->SetInsertPoint(LoopCondBB);
    llvm::Value *CondV = Cond->codegen();
    if (!CondV) return nullptr;

    // Convert to bool (i1) if needed
    if (CondV->getType()->isDoubleTy()) {
        CondV = Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "loopcond");
    } else if (CondV->getType()->isIntegerTy() && !CondV->getType()->isIntegerTy(1)) {
        CondV = Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(CondV->getType(), 0), "loopcond");
    }

    Builder->CreateCondBr(CondV, LoopBodyBB, LoopEndBB);

    TheFunction->insert(TheFunction->end(), LoopBodyBB);
    Builder->SetInsertPoint(LoopBodyBB);
    
    if (!Body->codegen()) return nullptr;
    
    // Jump back to start
    if (!Builder->GetInsertBlock()->getTerminator())
        Builder->CreateBr(LoopCondBB);

    TheFunction->insert(TheFunction->end(), LoopEndBB);
    Builder->SetInsertPoint(LoopEndBB);

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Value *ForExprAST::codegen() {
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

    EnterScope(); // Scope for Init variables

    // 1. Emit Init
    if (Init) {
        if (!Init->codegen()) {
            ExitScope();
            return nullptr;
        }
    }

    // Prepare Blocks
    llvm::BasicBlock *LoopCondBB = llvm::BasicBlock::Create(*TheContext, "loopcond", TheFunction);
    llvm::BasicBlock *LoopBodyBB = llvm::BasicBlock::Create(*TheContext, "loopbody");
    llvm::BasicBlock *LoopEndBB = llvm::BasicBlock::Create(*TheContext, "looplatch");
    llvm::BasicBlock *AfterLoopBB = llvm::BasicBlock::Create(*TheContext, "afterloop");

    // Jump to Condition
    Builder->CreateBr(LoopCondBB);

    // 2. Loop Condition
    Builder->SetInsertPoint(LoopCondBB);
    llvm::Value *CondV = nullptr;
    if (Cond) {
        CondV = Cond->codegen();
        if (!CondV) {
            ExitScope();
            return nullptr;
        }
        
        // Convert to bool
        if (CondV->getType()->isDoubleTy()) {
            CondV = Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "loopcond");
        } else if (CondV->getType()->isIntegerTy() && !CondV->getType()->isIntegerTy(1)) {
            CondV = Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(CondV->getType(), 0), "loopcond");
        }
    } else {
        // Infinite loop
        CondV = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), 1);
    }

    Builder->CreateCondBr(CondV, LoopBodyBB, AfterLoopBB);

    // 3. Loop Body
    TheFunction->insert(TheFunction->end(), LoopBodyBB);
    Builder->SetInsertPoint(LoopBodyBB);
    
    // Note: Body (BlockExprAST) will create its own inner scope.
    if (!Body->codegen()) {
        ExitScope();
        return nullptr;
    }
    
    // Jump to Latch (Step) if not terminated
    if (!Builder->GetInsertBlock()->getTerminator())
        Builder->CreateBr(LoopEndBB);

    // 4. Latch (Step)
    TheFunction->insert(TheFunction->end(), LoopEndBB);
    Builder->SetInsertPoint(LoopEndBB);
    
    if (Step) {
        if (!Step->codegen()) {
            ExitScope();
            return nullptr;
        }
    }
    
    // Back to Cond
    Builder->CreateBr(LoopCondBB);

    // 5. After Loop
    TheFunction->insert(TheFunction->end(), AfterLoopBB);
    Builder->SetInsertPoint(AfterLoopBB);

    ExitScope(); // Exit Init scope

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Function *PrototypeAST::codegen() {
    std::vector<llvm::Type *> LLVMArgs;
    for (auto &ArgPair : Args) {
        LLVMArgs.push_back(getLLVMType(ArgPair.second));
    }

    llvm::FunctionType *FT = llvm::FunctionType::get(
        getLLVMType(ReturnType), LLVMArgs, false);

    llvm::Function *F = llvm::Function::Create(
        FT, llvm::Function::ExternalLinkage, Name, TheModule.get());

    FunctionReturnTypes[Name] = ReturnType;

    unsigned Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++].first);

    return F;
}

llvm::Function *FunctionAST::codegen() {
    llvm::Function *TheFunction = TheModule->getFunction(Proto->getName());
    if (!TheFunction) TheFunction = Proto->codegen();
    if (!TheFunction) return nullptr;

    if (!Body) return TheFunction;

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    ScopeStack.clear();
    EnterScope(); // Function Scope

    // Register types from prototype
    const auto& ProtoArgs = Proto->getArgs();
    for (const auto& ArgPair : ProtoArgs) {
        AddVariableType(ArgPair.first, ArgPair.second);
    }

    for (auto &Arg : TheFunction->args()) {
        llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()), Arg.getType());
        Builder->CreateStore(&Arg, Alloca);
        AddVariable(std::string(Arg.getName()), Alloca);
    }


    if (llvm::Value *RetVal = Body->codegen()) {
        // Only insert return if the block is not terminated
        if (!Builder->GetInsertBlock()->getTerminator()) {
             Builder->CreateRet(RetVal);
        }
        llvm::verifyFunction(*TheFunction);
        return TheFunction;
    }

    TheFunction->eraseFromParent();
    return nullptr;
}

void StructDeclAST::codegen() {
    // Skip codegen for generic structs - they need to be instantiated first
    if (isGeneric()) {
        GenericStructRegistry[Name] = this->clone();
        return;
    }
    
    // Convert fields to LLVM types
    std::vector<llvm::Type*> fieldTypes;
    std::vector<FieldInfo> fieldInfos;
    
    size_t offset = 0;
    for (const auto& field : Fields) {
        llvm::Type* fieldType = getLLVMType(field.second);
        if (!fieldType) return; // Error
        
        fieldTypes.push_back(fieldType);
        
        FieldInfo info;
        info.name = field.first;
        info.type = field.second;
        info.offset = offset;
        fieldInfos.push_back(info);
        
        // Simple offset calculation (no padding for now)
        if (field.second.base == BaseType::Int || field.second.base == BaseType::Float) offset += 4;
        else if (field.second.base == BaseType::Char || field.second.base == BaseType::Byte || field.second.base == BaseType::Bool) offset += 1;
        else offset += 8; // pointers and other types
    }
    
    // Create LLVM struct type
    llvm::StructType* structType = llvm::StructType::create(*TheContext, fieldTypes, Name);
    StructTypes[Name] = structType;
    
    // Get Layout for accurate offsets and size
    const llvm::StructLayout *Layout = TheModule->getDataLayout().getStructLayout(structType);
    
    // Update field offsets
    for(size_t i = 0; i < fieldInfos.size(); ++i) {
        fieldInfos[i].offset = Layout->getElementOffset(i);
    }
    
    // Register in type registry with correct size
    TypeRegistry::getInstance().registerStruct(Name, fieldInfos, Layout->getSizeInBytes());
    
    // Generate code for methods with proper name mangling and this pointer
    for (auto& method : Methods) {
        // Mangle method name: StructName_methodName
        std::string originalName = method->getPrototype()->getName();
        std::string mangledName = Name + "_" + originalName;
        
        // Update the method's prototype name
        method->getPrototype()->setName(mangledName);
        
        // Inject 'this' parameter as first argument
        method->getPrototype()->injectThisParameter(Name);
        
        method->codegen();
    }
    
    // Generate code for constructors
    for (auto& constructor : Constructors) {
        constructor->codegen(Name);
    }
}

llvm::Function *ConstructorAST::codegen(const std::string& structName) {
    // Create constructor function name: StructName_new[_ArgType...]
    std::string funcName = structName + "_new";
    
    // Mangle name if parameters exist (simple mangling)
    std::vector<OType> paramOTypes;
    for (const auto& param : Params) {
        paramOTypes.push_back(param.second);
    }
    
    if (!paramOTypes.empty()) {
        funcName = mangleGenericName(funcName, paramOTypes);
    }
    
    // Params: this, args...
    std::vector<llvm::Type*> paramTypes;
    std::vector<std::string> paramNames;
    
    // 1. Add 'this' parameter
    if (StructTypes.find(structName) == StructTypes.end()) {
        LogError("Unknown struct type for constructor");
        return nullptr;
    }
    llvm::Type* structType = StructTypes[structName];
    paramTypes.push_back(llvm::PointerType::get(structType, 0));
    paramNames.push_back("this");
    
    // 2. Add user parameters
    for (const auto& param : Params) {
        paramTypes.push_back(getLLVMType(param.second));
        paramNames.push_back(param.first);
    }
    
    // Constructor returns void (it initializes the memory passed in 'this')
    llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), paramTypes, false);
    
    // Create function
    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, funcName, TheModule.get());
    
    // Create basic block
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*TheContext, "entry", func);
    Builder->SetInsertPoint(BB);
    
    EnterScope();
    
    // Handle Arguments
    auto argIt = func->arg_begin();
    for (size_t i = 0; i < paramNames.size(); ++i, ++argIt) {
        argIt->setName(paramNames[i]);
        
        if (paramNames[i] == "this") {
            // Store 'this' pointer in an alloca so we can access it via load/store
            llvm::AllocaInst* alloca = CreateEntryBlockAlloca(func, "this", argIt->getType());
            Builder->CreateStore(&*argIt, alloca);
            
            AddVariable("this", alloca);
            AddVariableType("this", OType(BaseType::Struct, 1, structName));
        } else {
            // User arguments
            llvm::AllocaInst* alloca = CreateEntryBlockAlloca(func, paramNames[i], argIt->getType());
            Builder->CreateStore(&*argIt, alloca);
            
            AddVariable(paramNames[i], alloca);
            // We should register the type here if we had it easily available matching the param index
            // But for now, we rely on the parser having checked types? 
            // Or we iterate Params again.
        }
    }
    
    Body->codegen();
    
    // Initialize VTable Pointer if it exists
    if (TypeRegistry::getInstance().hasStruct(structName)) {
        const StructInfo& info = TypeRegistry::getInstance().getStruct(structName);
        
        bool hasVptr = !info.fields.empty() && info.fields[0].name == "__vptr";
        
        if (hasVptr) {
            // Load 'this' pointer
            llvm::AllocaInst* thisAlloca = GetVariable("this");
            if (thisAlloca) {
                llvm::Value* thisPtr = Builder->CreateLoad(thisAlloca->getAllocatedType(), thisAlloca, "thisptr");
                
                // Get Global VTable
                std::string vtableName = structName + "_vtable";
                llvm::GlobalVariable* vtableVar = TheModule->getNamedGlobal(vtableName);
                
                if (vtableVar) {
                    // Get address of __vptr field (index 0)
                    std::vector<llvm::Value*> indices;
                    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
                    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0)); // Field 0
                    
                    llvm::Value* vptrAddr = Builder->CreateInBoundsGEP(
                        StructTypes[structName],
                        thisPtr,
                        indices,
                        "vptr_addr"
                    );
                    
                    // Store VTable address
                    // VTable var is [N x i8*]*. Decay to i8**.
                     llvm::Value* Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
                     llvm::Value* vtablePtr = Builder->CreateInBoundsGEP(
                         vtableVar->getValueType(),
                         vtableVar,
                         {Zero, Zero},
                         "vtable_decay"
                     );
                     
                     Builder->CreateStore(vtablePtr, vptrAddr);
                }
            }
        }
    }
    
    Builder->CreateRetVoid();
    
    ExitScope();
    
    return func;
}

void PrototypeAST::injectThisParameter(const std::string &StructName) {
    // Create 'this' parameter: StructName* this
    OType thisType = OType(BaseType::Struct, 1, StructName); // Pointer to struct
    std::pair<std::string, OType> thisParam = {"this", thisType};
    
    // Insert at the beginning of the parameter list
    Args.insert(Args.begin(), thisParam);
}

llvm::Value *AddressOfExprAST::codegen() {
    // Try to cast operand to VariableExprAST to get address
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(Operand.get())) {
        return VarExpr->codegenAddress();
    }
    
    // Support address of array elements
    if (auto IndexExpr = dynamic_cast<IndexExprAST*>(Operand.get())) {
        return IndexExpr->codegenAddress();
    }
    
    // For now, only support address-of variables and array elements
    LogError("Address-of operator only supports variables and array elements currently");
    return nullptr;
}

llvm::Value *DerefExprAST::codegen() {
    // RHS (Reading): Generate a load instruction on the pointer address
    llvm::Value *PtrValue = Operand->codegen();
    if (!PtrValue) return nullptr;
    
    // Load the value that the pointer points to
    if (!PtrValue->getType()->isPointerTy()) {
        LogError("Cannot dereference non-pointer type");
        return nullptr;
    }
    
    // For newer LLVM, we need to determine the pointee type differently
    // For now, assume it's an i32 (this would need proper type tracking)
    llvm::Type *pointeeType = llvm::Type::getInt32Ty(*TheContext);
    return Builder->CreateLoad(pointeeType, PtrValue, "deref");
}

llvm::Value *DerefExprAST::codegenAddress() {
    // LHS (Assignment): Return the pointer address for storing
    llvm::Value *PtrValue = Operand->codegen();
    if (!PtrValue) return nullptr;
    
    if (!PtrValue->getType()->isPointerTy()) {
        LogError("Cannot dereference non-pointer type");
        return nullptr;
    }
    
    return PtrValue; // Return the pointer itself for assignment target
}

llvm::Value *NewExprAST::codegen() {
    std::string LookupName = ClassName;
    
    // Handle Generics
    if (!GenericArgs.empty()) {
        // Trigger instantiation
        instantiateStruct(ClassName, GenericArgs);
        LookupName = mangleGenericName(ClassName, GenericArgs);
    }

    // Check if the class/struct exists
    if (StructTypes.find(LookupName) == StructTypes.end()) {
        LogError(("Unknown class/struct name in 'new' expression: " + LookupName).c_str());
        return nullptr;
    }
    
    llvm::StructType *StructType = StructTypes[LookupName];
    
    // 1. Heap Allocation (malloc)
    llvm::Function *MallocF = TheModule->getFunction("malloc");
    if (!MallocF) {
        std::vector<llvm::Type*> Args;
        Args.push_back(llvm::Type::getInt64Ty(*TheContext)); // size_t
        llvm::FunctionType *FT = llvm::FunctionType::get(
            llvm::PointerType::get(llvm::Type::getInt8Ty(*TheContext), 0), Args, false);
        MallocF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "malloc", TheModule.get());
    }
    
    // Calculate size
    size_t size = 0;
    if (TypeRegistry::getInstance().hasStruct(LookupName)) {
        size = TypeRegistry::getInstance().getStruct(LookupName).totalSize;
    }
    if (size == 0) size = 1; // Minimum allocation
    
    llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), size);
    llvm::Value *VoidPtr = Builder->CreateCall(MallocF, {SizeVal}, "mallocptr");
    
    // Cast to Struct*
    llvm::Value *ObjPtr = Builder->CreateBitCast(VoidPtr, llvm::PointerType::get(StructType, 0), "objptr");

    // 2. Call Constructor: LookupName_new(this, args...)
    std::string ConstructorName = LookupName + "_new";
    
    // Attempt mangled name resolution if args exist
    std::vector<OType> argTypes;
    for (auto &Arg : Args) {
        argTypes.push_back(Arg->getOType());
    }
    
    std::string MangledName = ConstructorName;
    if (!argTypes.empty()) {
        MangledName = mangleGenericName(ConstructorName, argTypes);
    }
    
    llvm::Function *Constructor = TheModule->getFunction(MangledName);
    
    // Fallback to base name if mangled not found (backward compatibility or void args)
    if (!Constructor) {
        Constructor = TheModule->getFunction(ConstructorName);
    }
    
    if (Constructor) {
        // Prepare arguments: this pointer + user arguments
        std::vector<llvm::Value*> CallArgs;
        CallArgs.push_back(ObjPtr); // 'this' pointer
        
        // Add user arguments
        for (auto &Arg : Args) {
            llvm::Value *ArgVal = Arg->codegen();
            if (!ArgVal) return nullptr;
            CallArgs.push_back(ArgVal);
        }
        
        // Call constructor
        Builder->CreateCall(Constructor, CallArgs);
    } else {
        // Only error if we expected a constructor and didn't find one.
        // Default constructor might be implicit (do nothing).
        if (!Args.empty()) {
             // Try to print useful error
             std::string err = "No matching constructor found for " + LookupName + " with arguments: ";
             for (const auto& t : argTypes) {
                 // Convert OType to string representation?
                 // err += ...
             }
             // For now simple error
             // LogError(("No matching constructor found for " + LookupName).c_str());
             // But actually, we might just proceed with uninitialized memory if no constructor.
             // But if args provided, it's definitely an error.
             LogError(("No matching constructor found for " + LookupName + " (tried " + MangledName + ")").c_str());
             return nullptr;
        }
    }
    
    return ObjPtr; // Return pointer to the allocated object
}

// Helper to get slice type: { i32 size, T* ptr }
llvm::StructType* getSliceType(llvm::Type* ElementType) {
    std::vector<llvm::Type*> Elements;
    Elements.push_back(llvm::Type::getInt32Ty(*TheContext)); // Size
    Elements.push_back(llvm::PointerType::get(ElementType, 0)); // Pointer
    return llvm::StructType::get(*TheContext, Elements);
}

llvm::Value *NewArrayExprAST::codegen() {
    // 1. Get/Declare malloc
    llvm::Function *MallocF = TheModule->getFunction("malloc");
    if (!MallocF) {
        std::vector<llvm::Type*> Args;
        Args.push_back(llvm::Type::getInt64Ty(*TheContext)); // size_t
        llvm::FunctionType *FT = llvm::FunctionType::get(
            llvm::PointerType::get(llvm::Type::getInt8Ty(*TheContext), 0), Args, false);
        MallocF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "malloc", TheModule.get());
    }
    
    // 2. Size
    llvm::Value *SizeVal = Size->codegen();
    if (!SizeVal) return nullptr;
    
    // Promote to i64 for malloc
    llvm::Value *Size64 = Builder->CreateZExtOrBitCast(SizeVal, llvm::Type::getInt64Ty(*TheContext));
    
    // 3. Calculate Bytes
    size_t ElemBytes = 4; // Default int
    if (ElementType.base == BaseType::Char || ElementType.base == BaseType::Bool || ElementType.base == BaseType::Byte) ElemBytes = 1;
    else if (ElementType.base == BaseType::Float) ElemBytes = 8;
    else if (ElementType.isPointer()) ElemBytes = 8;
    else if (ElementType.base == BaseType::Struct) {
         if (TypeRegistry::getInstance().hasStruct(ElementType.structName)) {
             ElemBytes = TypeRegistry::getInstance().getStruct(ElementType.structName).totalSize;
         }
    }
    
    llvm::Value *ElemSizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), ElemBytes);
    llvm::Value *TotalBytes = Builder->CreateMul(Size64, ElemSizeVal, "mallocsize");
    
    // 4. Call Malloc
    llvm::Value *VoidPtr = Builder->CreateCall(MallocF, {TotalBytes}, "mallocptr");
    
    // Cast to ElementType*
    llvm::Type *ElemLLVMType = getLLVMType(ElementType);
    llvm::Value *TypedPtr = Builder->CreateBitCast(VoidPtr, llvm::PointerType::get(ElemLLVMType, 0), "typedptr");
    
    // 5. Create Slice { Size, Ptr }
    llvm::StructType *SliceType = getSliceType(ElemLLVMType);
    llvm::Value *Slice = llvm::UndefValue::get(SliceType);
    
    // Insert Size (cast back to i32 if needed)
    llvm::Value *Size32 = Builder->CreateTruncOrBitCast(Size64, llvm::Type::getInt32Ty(*TheContext));
    Slice = Builder->CreateInsertValue(Slice, Size32, 0, "slice_len");
    
    // Insert Ptr
    Slice = Builder->CreateInsertValue(Slice, TypedPtr, 1, "slice_ptr");
    
    return Slice;
}

// Helper to get or create the llvm.trap intrinsic
llvm::Function *getTrapFunc() {
    return llvm::Intrinsic::getDeclaration(TheModule.get(), llvm::Intrinsic::trap);
}

// Helper to create bounds check
void createBoundsCheck(llvm::Value *IndexVal, llvm::Value *ArraySizeVal) {
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    
    llvm::BasicBlock *CheckBlock = llvm::BasicBlock::Create(*TheContext, "bounds_check", TheFunction);
    llvm::BasicBlock *ContinueBlock = llvm::BasicBlock::Create(*TheContext, "bounds_ok", TheFunction);
    llvm::BasicBlock *ErrorBlock = llvm::BasicBlock::Create(*TheContext, "bounds_fail", TheFunction);
    
    Builder->CreateBr(CheckBlock);
    Builder->SetInsertPoint(CheckBlock);
    
    // Check if Index < 0 || Index >= Size
    // Using unsigned comparison (uge) handles both cases:
    // If Index is negative (e.g. -1), it wraps to a large unsigned number, which is >= Size.
    llvm::Value *OutOfBounds = Builder->CreateICmpUGE(IndexVal, ArraySizeVal, "out_of_bounds");
    
    Builder->CreateCondBr(OutOfBounds, ErrorBlock, ContinueBlock);
    
    // Error Block
    Builder->SetInsertPoint(ErrorBlock);
    Builder->CreateCall(getTrapFunc(), {});
    Builder->CreateUnreachable();
    
    // Continue Block
    Builder->SetInsertPoint(ContinueBlock);
}

llvm::Value *MatchExprAST::codegen() {
    llvm::Value *CondVal = Cond->codegen();
    if (!CondVal) return nullptr;
    
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "matchcont");
    llvm::BasicBlock *CurrentTestBB = llvm::BasicBlock::Create(*TheContext, "matchtest", TheFunction);
    
    Builder->CreateBr(CurrentTestBB);
    
    // PHI Node tracking
    std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> IncomingValues;
    bool HasDefault = false;
    
    for (auto &Case : Cases) {
        Builder->SetInsertPoint(CurrentTestBB);
        
        llvm::BasicBlock *BodyBB = llvm::BasicBlock::Create(*TheContext, "matchbody", TheFunction);
        llvm::BasicBlock *NextBB = llvm::BasicBlock::Create(*TheContext, "nexttest", TheFunction);
        
        if (Case.Pattern) {
            // Check Pattern
            llvm::Value *PatternVal = Case.Pattern->codegen();
            if (!PatternVal) return nullptr;
            
            // Generate Compare
            llvm::Value *Cmp = nullptr;
            if (CondVal->getType()->isFloatingPointTy()) {
                Cmp = Builder->CreateFCmpOEQ(CondVal, PatternVal, "matcheq");
            } else {
                // Integer or Pointer
                Cmp = Builder->CreateICmpEQ(CondVal, PatternVal, "matcheq");
            }
            
            Builder->CreateCondBr(Cmp, BodyBB, NextBB);
        } else {
            // Wildcard (_)
            Builder->CreateBr(BodyBB);
            HasDefault = true;
            // No next test needed as wildcard matches everything
            // Remove NextBB from function since we won't jump to it
            NextBB->eraseFromParent();
        }
        
        // Generate Body
        Builder->SetInsertPoint(BodyBB);
        llvm::Value *BodyVal = Case.Body->codegen();
        if (!BodyVal) return nullptr;
        
        // Add to PHI (if not void and not terminated)
        if (!Builder->GetInsertBlock()->getTerminator()) {
            Builder->CreateBr(MergeBB);
            IncomingValues.push_back({BodyVal, Builder->GetInsertBlock()});
        }
        
        if (HasDefault) {
            break; 
        }
        
        // Move to next
        CurrentTestBB = NextBB;
    }
    
    // If no default and we ran out of cases:
    if (!HasDefault) {
        Builder->SetInsertPoint(CurrentTestBB);
        // Trap (Non-exhaustive match)
        Builder->CreateCall(getTrapFunc(), {});
        Builder->CreateUnreachable();
    }
    
    // Merge Block
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);
    
    if (IncomingValues.empty()) {
        // Void or all returns
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext)); // Dummy
    }
    
    // Create PHI
    llvm::Type *PhiType = IncomingValues[0].first->getType();
    if (PhiType->isVoidTy()) {
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
    }
    
    llvm::PHINode *PN = Builder->CreatePHI(PhiType, IncomingValues.size(), "matchphi");
    for (auto &Pair : IncomingValues) {
        // Handle type mismatch? Assume uniform for now.
        PN->addIncoming(Pair.first, Pair.second);
    }
    
    return PN;
}

llvm::Value *IndexExprAST::codegen() {
    // 1. Get Array Address
    llvm::Value *ArrayAddr = nullptr;
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(Array.get())) {
        ArrayAddr = VarExpr->codegenAddress();
    } else {
        ArrayAddr = Array->codegen();
    }
    if (!ArrayAddr) return nullptr;
    
    // 2. Resolve Type Info from AST
    OType ArrayOType = Array->getOType();
    
    // Check for Slice (Dynamic Array)
    if (ArrayOType.isArray() && ArrayOType.getArrayNumElements() == -1) {
        // Slice Structure Type
        llvm::Type *SliceType = getLLVMType(ArrayOType); // This should be {i32, T*}
        
        // 1. Get Size
        llvm::Value *SizePtr = Builder->CreateStructGEP(SliceType, ArrayAddr, 0, "slice_len_ptr");
        llvm::Value *SizeVal = Builder->CreateLoad(llvm::Type::getInt32Ty(*TheContext), SizePtr, "slice_len");
        
        // 2. Bounds Check
        llvm::Value *IndexVal = Index->codegen();
        if (!IndexVal) return nullptr;
        createBoundsCheck(IndexVal, SizeVal);
        
        // 3. Get Data Pointer
        llvm::Value *DataPtrPtr = Builder->CreateStructGEP(SliceType, ArrayAddr, 1, "slice_data_ptr");
        llvm::Type *DataPtrType = SliceType->getStructElementType(1); // T*
        llvm::Value *DataPtr = Builder->CreateLoad(DataPtrType, DataPtrPtr, "slice_data");
        
        // 4. Indexing GEP
        std::vector<llvm::Value*> Indices;
        Indices.push_back(IndexVal); 
        
        // Element Type
        OType ElemOType = ArrayOType.getElementType();
        llvm::Type *ElemLLVMType = getLLVMType(ElemOType);

        llvm::Value *ElementAddr = Builder->CreateInBoundsGEP(
           ElemLLVMType, 
           DataPtr,
           Indices,
           "slice_idx"
        );
        
        return Builder->CreateLoad(ElemLLVMType, ElementAddr, "slice_val_load");
    }

    // Fixed Array
    llvm::Type *ArrayLLVMType = getLLVMType(ArrayOType);
    if (!ArrayLLVMType->isArrayTy()) {
        // Fallback or Error? 
        // Might be a pointer to array?
        if (ArrayLLVMType->isPointerTy()) {
             // Pointer arithmetic
             // ...
        }
    }
    
    // Get the index value
    llvm::Value *IndexVal = Index->codegen();
    if (!IndexVal) return nullptr;
    
    // Perform Bounds Check
    uint64_t Size = ArrayOType.getArrayNumElements();
    llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), Size);
    createBoundsCheck(IndexVal, SizeVal);
    
    // Create indices: {0, Index}
    std::vector<llvm::Value*> Indices;
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
    Indices.push_back(IndexVal);
    
    llvm::Value *ElementAddr = Builder->CreateInBoundsGEP(
        ArrayLLVMType,
        ArrayAddr,
        Indices,
        "arrayidx"
    );
    
    // If the element is an array (multidimensional), return the address (decay to pointer)
    // Otherwise, load the value (primitive)
    if (ArrayOType.getElementType().isArray()) {
        return ElementAddr;
    }
    
    return Builder->CreateLoad(getLLVMType(ArrayOType.getElementType()), ElementAddr, "arrayload");
}

llvm::Value *IndexExprAST::codegenAddress() {
    // 1. Get Array Address
    llvm::Value *ArrayAddr = nullptr;
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(Array.get())) {
        ArrayAddr = VarExpr->codegenAddress();
    } else {
        ArrayAddr = Array->codegen();
    }
    if (!ArrayAddr) return nullptr;
    
    // 2. Resolve Type Info from AST
    OType ArrayOType = Array->getOType();
    
    // Check for Slice (Dynamic Array)
    if (ArrayOType.isArray() && ArrayOType.getArrayNumElements() == -1) {
        // Slice Structure Type
        llvm::Type *SliceType = getLLVMType(ArrayOType); 
        
        // 1. Get Size
        llvm::Value *SizePtr = Builder->CreateStructGEP(SliceType, ArrayAddr, 0, "slice_len_ptr");
        llvm::Value *SizeVal = Builder->CreateLoad(llvm::Type::getInt32Ty(*TheContext), SizePtr, "slice_len");
        
        // 2. Bounds Check
        llvm::Value *IndexVal = Index->codegen();
        if (!IndexVal) return nullptr;
        createBoundsCheck(IndexVal, SizeVal);
        
        // 3. Get Data Pointer
        llvm::Value *DataPtrPtr = Builder->CreateStructGEP(SliceType, ArrayAddr, 1, "slice_data_ptr");
        llvm::Type *DataPtrType = SliceType->getStructElementType(1); // T*
        llvm::Value *DataPtr = Builder->CreateLoad(DataPtrType, DataPtrPtr, "slice_data");
        
        // 4. Indexing GEP
        std::vector<llvm::Value*> Indices;
        Indices.push_back(IndexVal); 
        
        // Element Type
        OType ElemOType = ArrayOType.getElementType();
        llvm::Type *ElemLLVMType = getLLVMType(ElemOType);

        return Builder->CreateInBoundsGEP(
           ElemLLVMType, 
           DataPtr,
           Indices,
           "slice_addr"
        );
    }
    
    // Check for Pointer Indexing (ptr[i])
    if (ArrayOType.isPointer()) {
         llvm::Value *PtrVal = Array->codegen();
         if (!PtrVal) return nullptr;
         
         llvm::Value *IndexVal = Index->codegen();
         if (!IndexVal) return nullptr;
         
         OType ElemOType = ArrayOType.getPointeeType();
         llvm::Type *ElemLLVMType = getLLVMType(ElemOType);
         
         std::vector<llvm::Value*> Indices;
         Indices.push_back(IndexVal);
         
         return Builder->CreateInBoundsGEP(ElemLLVMType, PtrVal, Indices, "ptridx");
    }
    
    // Fixed Array
    llvm::Type *ArrayLLVMType = getLLVMType(ArrayOType);
    
    // Get the index value
    llvm::Value *IndexVal = Index->codegen();
    if (!IndexVal) return nullptr;
    
    // Perform Bounds Check
    uint64_t Size = ArrayOType.getArrayNumElements();
    llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), Size);
    createBoundsCheck(IndexVal, SizeVal);
    
    // Create indices: {0, Index}
    std::vector<llvm::Value*> Indices;
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
    Indices.push_back(IndexVal);
    
    return Builder->CreateInBoundsGEP(
        ArrayLLVMType,
        ArrayAddr,
        Indices,
        "arrayaddr"
    );
}

void ClassDeclAST::codegen() {
    std::vector<llvm::Type*> fieldTypes;
    std::vector<FieldInfo> fieldInfos;
    
    size_t offset = 0;
    
    // Check inheritance logic
    if (hasParent()) {
         if (TypeRegistry::getInstance().hasStruct(ParentName)) {
             const StructInfo& parentInfo = TypeRegistry::getInstance().getStruct(ParentName);
             // Inherit Virtual Methods from Parent
             std::vector<std::string> parentVMethods = parentInfo.virtualMethods;
             if (!parentVMethods.empty()) {
                 VirtualMethods.insert(VirtualMethods.begin(), parentVMethods.begin(), parentVMethods.end());
             }
         }
    }

    // Add hidden vptr field at index 0 if class has virtual methods or inherits from class with virtual methods
    bool needsVTable = !VirtualMethods.empty() || (hasParent() && isOpen());
    
    if (needsVTable) {
        // vptr is a pointer to array of function pointers (i8**)
        llvm::Type* i8PtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(*TheContext), 0);
        llvm::Type* vptrType = llvm::PointerType::get(i8PtrType, 0);
        fieldTypes.push_back(vptrType);
        
        FieldInfo vptrInfo;
        vptrInfo.name = "__vptr";
        vptrInfo.type = OType(BaseType::Void, 2); // void**
        vptrInfo.offset = offset;
        fieldInfos.push_back(vptrInfo);
        
        offset += 8; // vptr is 8 bytes
    }
    
    // Struct Prefixing: If this class has a parent, include parent fields
    if (hasParent()) {
        if (StructTypes.find(ParentName) == StructTypes.end()) {
            LogError("Parent class not found");
            return;
        }
        
        if (TypeRegistry::getInstance().hasStruct(ParentName)) {
            const StructInfo& parentInfo = TypeRegistry::getInstance().getStruct(ParentName);

            // Copy parent fields (skip vptr if already added)
            for (const auto& parentField : parentInfo.fields) {
                if (needsVTable && parentField.name == "__vptr") continue; // Skip parent vptr
                
                llvm::Type* fieldType = getLLVMType(parentField.type);
                fieldTypes.push_back(fieldType);
                
                FieldInfo info;
                info.name = parentField.name;
                info.type = parentField.type;
                info.offset = offset;
                fieldInfos.push_back(info);
                
                offset += TypeRegistry::getInstance().getTypeSize(parentField.type);
            }
        }
    }
    
    // Add child-specific fields
    for (const auto& field : Fields) {
        llvm::Type* fieldType = getLLVMType(field.second);
        if (!fieldType) return;
        
        fieldTypes.push_back(fieldType);
        
        FieldInfo info;
        info.name = field.first;
        info.type = field.second;
        info.offset = offset;
        fieldInfos.push_back(info);
        
        offset += TypeRegistry::getInstance().getTypeSize(field.second);
    }
    
    // Create LLVM struct type
    llvm::StructType* structType = llvm::StructType::create(*TheContext, fieldTypes, Name);
    StructTypes[Name] = structType;
    
    // Register in type registry
    TypeRegistry::getInstance().registerStruct(Name, fieldInfos, VirtualMethods);
    
    // Generate code for methods with name mangling FIRST
    for (auto& method : Methods) {
        std::string originalName = method->getPrototype()->getName();
        std::string mangledName = Name + "_" + originalName;
        method->getPrototype()->setName(mangledName);
        method->getPrototype()->injectThisParameter(Name);
        method->codegen();
    }
    
    // Generate VTable AFTER methods are compiled but BEFORE constructors
    if (needsVTable) {
        generateVTable();
    }

    // Generate code for constructors
    for (auto& constructor : Constructors) {
        constructor->codegen(Name);
    }
}

void ClassDeclAST::generateVTable() {
    // Create VTable as global array of function pointers
    std::vector<llvm::Constant*> vtableEntries;
    
    // For each virtual method, add function pointer to VTable
    for (const auto& virtualMethod : VirtualMethods) {
        std::string mangledName = Name + "_" + virtualMethod;
        llvm::Function* func = TheModule->getFunction(mangledName);
        
        if (func) {
            // Cast function to i8* for VTable storage
            llvm::Type* i8PtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(*TheContext), 0);
            llvm::Constant* funcPtr = llvm::ConstantExpr::getBitCast(func, i8PtrType);
            vtableEntries.push_back(funcPtr);
        }
    }
    
    if (!vtableEntries.empty()) {
        // Create VTable type: array of i8*
        llvm::Type* i8PtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(*TheContext), 0);
        llvm::ArrayType* vtableType = llvm::ArrayType::get(i8PtrType, vtableEntries.size());
        
        // Create VTable constant
        llvm::Constant* vtableInit = llvm::ConstantArray::get(vtableType, vtableEntries);
        
        // Create global VTable variable
        std::string vtableName = Name + "_vtable";
        new llvm::GlobalVariable(*TheModule, vtableType, true, llvm::GlobalValue::ExternalLinkage, vtableInit, vtableName);
    }
}

llvm::Value *MemberAccessAST::codegen() {
    // 1. Get Address of Field
    llvm::Value *FieldPtr = codegenAddress();
    if (!FieldPtr) {
        // Fallback for non-lvalue access (e.g. array.len)
        OType ObjOType = Object->getOType();
        if (ObjOType.isArray() && (FieldName == "len" || FieldName == "length")) {
             return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), ObjOType.getArrayNumElements());
        }
        
        // Check for Slice Length Property
         if (ObjOType.isArray() && ObjOType.getArrayNumElements() == -1 && (FieldName == "len" || FieldName == "length")) {
             llvm::Value *ObjVal = Object->codegen();
             if (!ObjVal) return nullptr;
             
             if (ObjVal->getType()->isStructTy()) {
                 return Builder->CreateExtractValue(ObjVal, 0, "len");
             }
         }
         
        // Check for Slice Ptr Property
         if (ObjOType.isArray() && ObjOType.getArrayNumElements() == -1 && FieldName == "ptr") {
             llvm::Value *ObjVal = Object->codegen();
             if (!ObjVal) return nullptr;
             
             if (ObjVal->getType()->isStructTy()) {
                 return Builder->CreateExtractValue(ObjVal, 1, "ptr");
             }
         }
        
        return nullptr;
    }
    
    // 2. Load Value
    OType FieldType = getOType();
    llvm::Type *LLVMFieldType = getLLVMType(FieldType);
    
    return Builder->CreateLoad(LLVMFieldType, FieldPtr, FieldName);
}

llvm::Value *MemberAccessAST::codegenAddress() {
    llvm::Value *ObjAddr = nullptr;
    OType ObjOType = Object->getOType();
    
    // Strategy: Get the "base pointer" to the struct.
    if (ObjOType.isPointer()) {
        ObjAddr = Object->codegen();
    } else {
        ObjAddr = Object->codegenAddress();
    }
    
    if (!ObjAddr) return nullptr;
    
    // Handle Slice .ptr (if Obj is addressable)
    if (ObjOType.isArray() && ObjOType.getArrayNumElements() == -1 && FieldName == "ptr") {
        llvm::StructType *ST = llvm::cast<llvm::StructType>(getLLVMType(ObjOType));
        std::vector<llvm::Value*> Indices;
        Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
        Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 1));
        return Builder->CreateInBoundsGEP(ST, ObjAddr, Indices, "slice_ptr_addr");
    }
    
    if (ObjOType.base != BaseType::Struct) {
        return nullptr;
    }
    
    std::string StructName = ObjOType.structName;
    if (!TypeRegistry::getInstance().hasStruct(StructName)) {
        return nullptr;
    }
    
    const StructInfo& info = TypeRegistry::getInstance().getStruct(StructName);
    
    int fieldIndex = -1;
    for (size_t i = 0; i < info.fields.size(); ++i) {
        if (info.fields[i].name == FieldName) {
            fieldIndex = i;
            break;
        }
    }
    
    if (fieldIndex == -1) return nullptr;
    
    llvm::StructType *ST = StructTypes[StructName];
    if (!ST) return nullptr;
    
    std::vector<llvm::Value*> Indices;
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), fieldIndex));
    
    return Builder->CreateInBoundsGEP(ST, ObjAddr, Indices, "fieldaddr");
}

OType MemberAccessAST::getOType() const {
    OType ObjType = Object->getOType();
    if (ObjType.base == BaseType::Struct) {
        // Lookup field type
        if (TypeRegistry::getInstance().hasStruct(ObjType.structName)) {
            const StructInfo& info = TypeRegistry::getInstance().getStruct(ObjType.structName);
            for (const auto& field : info.fields) {
                if (field.name == FieldName) return field.type;
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

OType VariableExprAST::getOType() const {
    return GetVariableType(Name);
}

OType BinaryExprAST::getOType() const {
    // Basic inference: assume LHS type
    return LHS->getOType();
}

OType CallExprAST::getOType() const {
    if (FunctionReturnTypes.count(Callee)) return FunctionReturnTypes.at(Callee);
    return OType(BaseType::Void);
}

OType MethodCallExprAST::getOType() const {
    OType ObjType = Object->getOType();
    std::string StructName = ObjType.structName;
    
    if (!ObjType.genericArgs.empty()) {
        StructName = mangleGenericName(StructName, ObjType.genericArgs);
    }
    
    std::string MangledName = StructName + "_" + MethodName;
    if (FunctionReturnTypes.count(MangledName)) return FunctionReturnTypes.at(MangledName);
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
    return ArrType.getElementType();
}

OType DerefExprAST::getOType() const {
    OType PtrType = Operand->getOType();
    return PtrType.getPointeeType();
}

OType AddressOfExprAST::getOType() const {
    OType OpType = Operand->getOType();
    return OpType.getPointerTo();
}

llvm::Value *ArrayInitExprAST::codegen() {
    if (Elements.empty()) {
        LogError("Cannot create empty array without type information");
        return nullptr;
    }
    
    // Generate code for first element to infer type
    llvm::Value *FirstElement = Elements[0]->codegen();
    if (!FirstElement) return nullptr;
    
    llvm::Type *ElementType = FirstElement->getType();
    size_t ArraySize = Elements.size();
    
    // Create array type
    llvm::ArrayType *ArrayType = llvm::ArrayType::get(ElementType, ArraySize);
    
    // Allocate array on stack
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    llvm::AllocaInst *ArrayAlloca = CreateEntryBlockAlloca(TheFunction, "array", ArrayType);
    
    // Store first element
    llvm::Value *FirstIdx = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0));
    llvm::Value *FirstPtr = Builder->CreateInBoundsGEP(ArrayType, ArrayAlloca, {FirstIdx, FirstIdx});
    Builder->CreateStore(FirstElement, FirstPtr);
    
    // Store remaining elements
    for (size_t i = 1; i < Elements.size(); ++i) {
        llvm::Value *Element = Elements[i]->codegen();
        if (!Element) return nullptr;
        
        // Type check
        if (Element->getType() != ElementType) {
            LogError("Array elements must have the same type");
            return nullptr;
        }
        
        llvm::Value *Idx = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, i));
        llvm::Value *ElementPtr = Builder->CreateInBoundsGEP(ArrayType, ArrayAlloca, {FirstIdx, Idx});
        Builder->CreateStore(Element, ElementPtr);
    }
    
    return Builder->CreateLoad(ArrayType, ArrayAlloca, "arrayinit");
}
