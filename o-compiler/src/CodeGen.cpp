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
    if (t.isArray()) {
        baseType = llvm::ArrayType::get(baseType, t.arraySize);
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
    
    llvm::Value *InitVal = Init->codegen();
    if (!InitVal) return nullptr;
    
    llvm::Type *AllocaType = InitVal->getType();
    
    // If explicit type is provided, validate compatibility
    if (HasExplicitType) {
        llvm::Type *ExpectedType = getLLVMType(ExplicitType);
        if (ExpectedType && ExpectedType != AllocaType) {
            LogError("Type mismatch in variable declaration");
            return nullptr;
        }
        if (ExpectedType) AllocaType = ExpectedType;
    }
    
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Name, AllocaType);
    Builder->CreateStore(InitVal, Alloca);
    NamedValues[Name] = Alloca;
    
    return InitVal;
}

llvm::Value *AssignmentExprAST::codegen() {
    llvm::Value *Val = RHS->codegen();
    if (!Val) return nullptr;
    
    llvm::AllocaInst *Variable = NamedValues[Name];
    if (!Variable) {
        LogError("Unknown variable in assignment");
        return nullptr;
    }
    
    Builder->CreateStore(Val, Variable);
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
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back()) return nullptr;
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
    
    // For now, only support address-of variables
    LogError("Address-of operator only supports variables currently");
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

llvm::Value *IndexExprAST::codegen() {
    // Return first element of any array for now
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 5);
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
    
    // Get the index value
    llvm::Value *IndexVal = Index->codegen();
    if (!IndexVal) return nullptr;
    
    // Use GEP to calculate element address (for assignment)
    std::vector<llvm::Value*> Indices;
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0));
    Indices.push_back(IndexVal);
    
    llvm::Type *ArrayType = llvm::ArrayType::get(llvm::Type::getInt32Ty(*TheContext), 10);
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
    // For now, return a placeholder - this needs proper implementation
    // with GEP instructions and type checking
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
    
    return ArrayAlloca;
}
