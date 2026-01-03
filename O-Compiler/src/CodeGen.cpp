#include "AST.h"
#include <map>

// --- Global Compiler State ---
std::unique_ptr<llvm::LLVMContext> TheContext;
std::unique_ptr<llvm::IRBuilder<>> Builder;
std::unique_ptr<llvm::Module> TheModule;
std::map<std::string, llvm::AllocaInst *> NamedValues;

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
llvm::Type* getLLVMType(OType t) {
    if (t == OType::Int) return llvm::Type::getInt32Ty(*TheContext);
    if (t == OType::Float) return llvm::Type::getDoubleTy(*TheContext);
    if (t == OType::Bool) return llvm::Type::getInt1Ty(*TheContext);
    return llvm::Type::getVoidTy(*TheContext);
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

llvm::Value *NumberExprAST::codegen() {
    if (Type == OType::Float) {
        return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Val));
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
    llvm::Value *L = LHS->codegen();
    llvm::Value *R = RHS->codegen();
    if (!L || !R) return nullptr;

    if (L->getType() != R->getType()) {
        LogError("Type mismatch in binary expression");
        return nullptr;
    }

    bool isFloat = L->getType()->isDoubleTy();

    switch (Op) {
        case '+': 
            return isFloat ? Builder->CreateFAdd(L, R, "addtmp") 
                           : Builder->CreateAdd(L, R, "addtmp");
        case '-': 
            return isFloat ? Builder->CreateFSub(L, R, "subtmp") 
                           : Builder->CreateSub(L, R, "subtmp");
        case '/': 
            return isFloat ? Builder->CreateFDiv(L, R, "divtmp") 
                           : Builder->CreateSDiv(L, R, "divtmp");
        case '<':
             return isFloat ? Builder->CreateFCmpULT(L, R, "cmptmp")
                            : Builder->CreateICmpULT(L, R, "cmptmp");
        case '>':
             return isFloat ? Builder->CreateFCmpOGT(L, R, "cmptmp")
                            : Builder->CreateICmpSGT(L, R, "cmptmp");
        default:
            return nullptr;
    }
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
