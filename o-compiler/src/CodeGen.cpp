#include "AST.h"
#include <map>

// --- Global Compiler State ---
std::unique_ptr<llvm::LLVMContext> TheContext;
std::unique_ptr<llvm::IRBuilder<>> Builder;
std::unique_ptr<llvm::Module> TheModule;
std::map<std::string, llvm::AllocaInst *> NamedValues;
std::unordered_map<std::string, llvm::StructType*> StructTypes;

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
            } else {
                baseType = llvm::Type::getVoidTy(*TheContext);
            }
            break;
        default: baseType = llvm::Type::getVoidTy(*TheContext); break;
    }
    
    // Apply array type if needed
    // Iterate in reverse: int[2][3] -> [2 x [3 x i32]]
    // Inner dimension (3) wraps the base type first.
    if (t.isArray()) {
        for (auto it = t.arraySizes.rbegin(); it != t.arraySizes.rend(); ++it) {
            if (*it == -1) {
                // Slice: int[] -> { i32, int* }
                baseType = getSliceType(baseType);
            } else {
                // Fixed Array: int[5] -> [5 x int]
                baseType = llvm::ArrayType::get(baseType, *it);
            }
        }
    }
    
    // Apply pointer depth
    for (int i = 0; i < t.pointerDepth; i++) {
        baseType = llvm::PointerType::get(baseType, 0);
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
    llvm::AllocaInst *A = NamedValues[Name];
    if (!A) {
        LogError("Unknown variable name");
        return nullptr;
    }
    return Builder->CreateLoad(A->getAllocatedType(), A, Name.c_str());
}

llvm::Value *VariableExprAST::codegenAddress() {
    llvm::AllocaInst *A = NamedValues[Name];
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
    
    NamedValues[Name] = Alloca;
    
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
        Builder->CreateRet(V);
    } else {
        Builder->CreateRetVoid();
        V = llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext)); // Dummy
    }
    return V;
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

llvm::Value *BlockExprAST::codegen() {
    llvm::Value *LastVal = nullptr;
    for (auto &Expr : Expressions) {
        LastVal = Expr->codegen();
        if (!LastVal) return nullptr;
        
        // If the block is already terminated (e.g. by return), stop emitting
        if (Builder->GetInsertBlock()->getTerminator()) {
             break;
        }
    }
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

llvm::Function *PrototypeAST::codegen() {
    std::vector<llvm::Type *> LLVMArgs;
    for (auto &ArgPair : Args) {
        LLVMArgs.push_back(getLLVMType(ArgPair.second));
    }

    llvm::FunctionType *FT = llvm::FunctionType::get(
        getLLVMType(ReturnType), LLVMArgs, false);

    llvm::Function *F = llvm::Function::Create(
        FT, llvm::Function::ExternalLinkage, Name, TheModule.get());

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

    NamedValues.clear();
    for (auto &Arg : TheFunction->args()) {
        llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()), Arg.getType());
        Builder->CreateStore(&Arg, Alloca);
        NamedValues[std::string(Arg.getName())] = Alloca;
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
        // For now, just register the generic struct template
        // Full generic instantiation would be implemented later
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
    
    // Register in type registry
    TypeRegistry::getInstance().registerStruct(Name, fieldInfos);
    
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
    // Create constructor function name: StructName_new
    std::string funcName = structName + "_new";
    
    // Convert parameters to LLVM types
    std::vector<llvm::Type*> paramTypes;
    std::vector<std::string> paramNames;
    
    for (const auto& param : Params) {
        paramTypes.push_back(getLLVMType(param.second));
        paramNames.push_back(param.first);
    }
    
    // Constructor returns a pointer to the struct
    llvm::Type* structType = StructTypes[structName];
    llvm::Type* returnType = llvm::PointerType::get(structType, 0);
    
    // Create function type
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    
    // Create function
    llvm::Function* func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, funcName, TheModule.get());
    
    // Create basic block
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*TheContext, "entry", func);
    Builder->SetInsertPoint(BB);
    
    // Set parameter names and create allocas
    std::map<std::string, llvm::AllocaInst*> oldNamedValues = NamedValues;
    NamedValues.clear();
    
    auto argIt = func->arg_begin();
    for (size_t i = 0; i < paramNames.size(); ++i, ++argIt) {
        argIt->setName(paramNames[i]);
        llvm::AllocaInst* alloca = CreateEntryBlockAlloca(func, paramNames[i], paramTypes[i]);
        Builder->CreateStore(&*argIt, alloca);
        NamedValues[paramNames[i]] = alloca;
    }
    
    // Generate constructor body
    Body->codegen();
    
    // For now, return null pointer (proper struct allocation would go here)
    Builder->CreateRet(llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(returnType)));
    
    // Restore old named values
    NamedValues = oldNamedValues;
    
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
    // Check if the class/struct exists
    if (StructTypes.find(ClassName) == StructTypes.end()) {
        LogError("Unknown class/struct name in 'new' expression");
        return nullptr;
    }
    
    llvm::StructType *StructType = StructTypes[ClassName];
    
    // Allocate memory on stack for now (heap allocation would come later)
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, "obj", StructType);
    
    // Try to call constructor: ClassName_new
    std::string ConstructorName = ClassName + "_new";
    llvm::Function *Constructor = TheModule->getFunction(ConstructorName);
    
    if (Constructor) {
        // Prepare arguments: this pointer + user arguments
        std::vector<llvm::Value*> CallArgs;
        CallArgs.push_back(Alloca); // 'this' pointer
        
        // Add user arguments
        for (auto &Arg : Args) {
            llvm::Value *ArgVal = Arg->codegen();
            if (!ArgVal) return nullptr;
            CallArgs.push_back(ArgVal);
        }
        
        // Call constructor
        Builder->CreateCall(Constructor, CallArgs);
    }
    // If no constructor exists, just return the allocated memory
    
    return Alloca; // Return pointer to the allocated object
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
    // Use the working codegenAddress approach with correct size
    llvm::Value *ArrayAddr = nullptr;
    
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(Array.get())) {
        ArrayAddr = VarExpr->codegenAddress();
    } else {
        ArrayAddr = Array->codegen();
    }
    
    if (!ArrayAddr) return nullptr;
    
    // Dynamically get the array type from the allocated value
    llvm::Type *ArrayType = nullptr;
    if (llvm::AllocaInst *AI = llvm::dyn_cast<llvm::AllocaInst>(ArrayAddr)) {
        ArrayType = AI->getAllocatedType();
    } else if (llvm::GlobalVariable *GV = llvm::dyn_cast<llvm::GlobalVariable>(ArrayAddr)) {
        ArrayType = GV->getValueType();
    } else if (llvm::GetElementPtrInst *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(ArrayAddr)) {
        std::vector<llvm::Value*> Indices(GEP->idx_begin(), GEP->idx_end());
        ArrayType = llvm::GetElementPtrInst::getIndexedType(GEP->getSourceElementType(), Indices);
    }

    // Check for Slice (Dynamic Array)
    if (ArrayType && ArrayType->isStructTy() && !ArrayType->getStructName().starts_with("class.") && !ArrayType->getStructName().starts_with("struct.")) {
        if (ArrayType->getStructNumElements() == 2 && 
            ArrayType->getStructElementType(0)->isIntegerTy(32) &&
            ArrayType->getStructElementType(1)->isPointerTy()) {
            
            // 1. Get Size
            llvm::Value *SizePtr = Builder->CreateStructGEP(ArrayType, ArrayAddr, 0, "slice_len_ptr");
            llvm::Value *SizeVal = Builder->CreateLoad(llvm::Type::getInt32Ty(*TheContext), SizePtr, "slice_len");
            
            // 2. Bounds Check
            llvm::Value *IndexVal = Index->codegen();
            if (!IndexVal) return nullptr;
            createBoundsCheck(IndexVal, SizeVal);
            
            // 3. Get Data Pointer
            llvm::Value *DataPtrPtr = Builder->CreateStructGEP(ArrayType, ArrayAddr, 1, "slice_data_ptr");
            llvm::Value *DataPtr = Builder->CreateLoad(ArrayType->getStructElementType(1), DataPtrPtr, "slice_data");
            
            // 4. Indexing
            std::vector<llvm::Value*> Indices;
            Indices.push_back(IndexVal); 
            
            llvm::Value *ElementAddr = Builder->CreateInBoundsGEP(
               llvm::Type::getInt32Ty(*TheContext), 
               DataPtr,
               Indices,
               "slice_idx"
            );
            
            return Builder->CreateLoad(llvm::Type::getInt32Ty(*TheContext), ElementAddr, "slice_val_load");
        }
    }

    if (!ArrayType || !ArrayType->isArrayTy()) {
        LogError("Indexing target is not an array or type could not be determined");
        return nullptr;
    }
    
    // Get the index value
    llvm::Value *IndexVal = Index->codegen();
    if (!IndexVal) return nullptr;
    
    // Perform Bounds Check
    uint64_t Size = ArrayType->getArrayNumElements();
    llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), Size);
    createBoundsCheck(IndexVal, SizeVal);
    
    // Create indices: {0, Index}
    std::vector<llvm::Value*> Indices;
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
    Indices.push_back(IndexVal);
    
    llvm::Value *ElementAddr = Builder->CreateInBoundsGEP(
        ArrayType,
        ArrayAddr,
        Indices,
        "arrayidx"
    );
    
    // If the element is an array (multidimensional), return the address (decay to pointer)
    // Otherwise, load the value (primitive)
    if (ArrayType->getArrayElementType()->isArrayTy()) {
        return ElementAddr;
    }
    
    return Builder->CreateLoad(ArrayType->getArrayElementType(), ElementAddr, "arrayload");
}

llvm::Value *IndexExprAST::codegenAddress() {
    // Get the array address
    llvm::Value *ArrayAddr = nullptr;
    
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(Array.get())) {
        ArrayAddr = VarExpr->codegenAddress();
    } else {
        ArrayAddr = Array->codegen();
    }
    
    if (!ArrayAddr) return nullptr;
    
    // Dynamically get the array type from the allocated value
    llvm::Type *ArrayType = nullptr;
    if (llvm::AllocaInst *AI = llvm::dyn_cast<llvm::AllocaInst>(ArrayAddr)) {
        ArrayType = AI->getAllocatedType();
    } else if (llvm::GlobalVariable *GV = llvm::dyn_cast<llvm::GlobalVariable>(ArrayAddr)) {
        ArrayType = GV->getValueType();
    } else if (llvm::GetElementPtrInst *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(ArrayAddr)) {
        std::vector<llvm::Value*> Indices(GEP->idx_begin(), GEP->idx_end());
        ArrayType = llvm::GetElementPtrInst::getIndexedType(GEP->getSourceElementType(), Indices);
    }

    // Check for Slice (Dynamic Array)
    if (ArrayType && ArrayType->isStructTy() && !ArrayType->getStructName().starts_with("class.") && !ArrayType->getStructName().starts_with("struct.")) {
        if (ArrayType->getStructNumElements() == 2 && 
            ArrayType->getStructElementType(0)->isIntegerTy(32) &&
            ArrayType->getStructElementType(1)->isPointerTy()) {
            
            // 1. Get Size
            llvm::Value *SizePtr = Builder->CreateStructGEP(ArrayType, ArrayAddr, 0, "slice_len_ptr");
            llvm::Value *SizeVal = Builder->CreateLoad(llvm::Type::getInt32Ty(*TheContext), SizePtr, "slice_len");
            
            // 2. Bounds Check
            llvm::Value *IndexVal = Index->codegen();
            if (!IndexVal) return nullptr;
            createBoundsCheck(IndexVal, SizeVal);
            
            // 3. Get Data Pointer
            llvm::Value *DataPtrPtr = Builder->CreateStructGEP(ArrayType, ArrayAddr, 1, "slice_data_ptr");
            llvm::Value *DataPtr = Builder->CreateLoad(ArrayType->getStructElementType(1), DataPtrPtr, "slice_data");
            
            // 4. Indexing
            std::vector<llvm::Value*> Indices;
            Indices.push_back(IndexVal); 
            
            // Assumption: int array (slice)
            return Builder->CreateInBoundsGEP(
               llvm::Type::getInt32Ty(*TheContext), 
               DataPtr,
               Indices,
               "slice_addr"
            );
        }
    }

    if (!ArrayType || !ArrayType->isArrayTy()) {
        LogError("Indexing target is not an array or type could not be determined");
        return nullptr;
    }
    
    // Get the index value
    llvm::Value *IndexVal = Index->codegen();
    if (!IndexVal) return nullptr;
    
    // Perform Bounds Check
    uint64_t Size = ArrayType->getArrayNumElements();
    llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), Size);
    createBoundsCheck(IndexVal, SizeVal);
    
    // Use GEP to calculate element address (for assignment)
    std::vector<llvm::Value*> Indices;
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
    Indices.push_back(IndexVal);
    
    return Builder->CreateInBoundsGEP(
        ArrayType,
        ArrayAddr,
        Indices,
        "arrayaddr"
    );
}

void ClassDeclAST::codegen() {
    std::vector<llvm::Type*> fieldTypes;
    std::vector<FieldInfo> fieldInfos;
    
    size_t offset = 0;
    
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
    TypeRegistry::getInstance().registerStruct(Name, fieldInfos);
    
    // Generate code for methods with name mangling FIRST
    for (auto& method : Methods) {
        std::string originalName = method->getPrototype()->getName();
        std::string mangledName = Name + "_" + originalName;
        method->getPrototype()->setName(mangledName);
        method->getPrototype()->injectThisParameter(Name);
        method->codegen();
    }
    
    // Generate code for constructors
    for (auto& constructor : Constructors) {
        constructor->codegen(Name);
    }
    
    // Generate VTable AFTER methods are compiled
    if (needsVTable) {
        generateVTable();
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
    // Generate code for the object
    llvm::Value *ObjVal = nullptr;
    
    // If it's a variable, we want the address to check the type properly
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(Object.get())) {
        ObjVal = VarExpr->codegenAddress();
    } else {
        ObjVal = Object->codegen();
    }

    if (!ObjVal) return nullptr;

    llvm::Type *ObjType = ObjVal->getType();
    
    // Unwrap pointer if needed (AllocaInst returns pointer to array)
    if (ObjType->isPointerTy()) {
        if (llvm::AllocaInst *AI = llvm::dyn_cast<llvm::AllocaInst>(ObjVal)) {
             ObjType = AI->getAllocatedType();
        } else if (llvm::GlobalVariable *GV = llvm::dyn_cast<llvm::GlobalVariable>(ObjVal)) {
             ObjType = GV->getValueType();
        } else {
             // Try to peek element type (warning: deprecated in newer LLVM but useful for now)
             // ObjType = ObjType->getPointerElementType(); 
        }
    }

    // Check for Array Length Property
    if (ObjType->isArrayTy()) {
        if (FieldName == "len" || FieldName == "length") {
            uint64_t Size = ObjType->getArrayNumElements();
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), Size);
        }
    }
    
    // Check for Slice Length Property (Dynamic Array)
    if (ObjType->isStructTy() && !ObjType->getStructName().starts_with("class.") && !ObjType->getStructName().starts_with("struct.")) {
        // Assume anonymous struct { i32, T* } is a slice
        if (ObjType->getStructNumElements() == 2 && 
            ObjType->getStructElementType(0)->isIntegerTy(32) &&
            ObjType->getStructElementType(1)->isPointerTy()) {
            
            if (FieldName == "len" || FieldName == "length") {
                // ObjVal is a pointer to the slice struct (if l-value) or the struct value itself?
                // ObjVal came from codegenAddress() or codegen().
                // If ObjVal is pointer (Alloca), we GEP to field 0 and load.
                // If ObjVal is value, we ExtractValue.
                
                if (ObjVal->getType()->isPointerTy()) {
                    llvm::Value *LenPtr = Builder->CreateStructGEP(ObjType, ObjVal, 0, "len_ptr");
                    return Builder->CreateLoad(llvm::Type::getInt32Ty(*TheContext), LenPtr, "len_val");
                } else {
                    return Builder->CreateExtractValue(ObjVal, 0, "len_val");
                }
            }
        }
    }

    // TODO: Implement struct member access
    LogError("Member access not fully implemented (only array.len supported)");
    return nullptr;
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
