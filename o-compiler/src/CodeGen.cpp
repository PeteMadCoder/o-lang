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
    
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Name, InitVal->getType());
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
    
    // Generate code for methods
    for (auto& method : Methods) {
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

llvm::Value *MemberAccessAST::codegen() {
    // For now, return a placeholder - this needs proper implementation
    // with GEP instructions and type checking
    return nullptr;
}
