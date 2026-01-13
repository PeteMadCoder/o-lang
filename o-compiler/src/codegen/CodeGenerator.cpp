#include "CodeGenerator.h"
#include "ExpressionCodeGen.h"
#include "StatementCodeGen.h"
#include "FunctionCodeGen.h"
#include "TypeCodeGen.h"
#include "UtilityCodeGen.h"
#include "TypeResolver.h"
#include "InstantiationManager.h"

CodeGenerator::CodeGenerator() {
    instantiationManager = std::make_unique<InstantiationManager>();
    exprCodeGen = std::make_unique<ExpressionCodeGen>(*this);
    stmtCodeGen = std::make_unique<StatementCodeGen>(*this);
    funcCodeGen = std::make_unique<FunctionCodeGen>(*this);
    typeCodeGen = std::make_unique<TypeCodeGen>(*this);
    typeResolver = std::make_unique<TypeResolver>(*this);
    utilCodeGen = std::make_unique<UtilityCodeGen>(*this);
}

CodeGenerator::~CodeGenerator() = default;

void CodeGenerator::initializeModuleAndPassManager() {
    // TheContext, Builder, and TheModule are managed externally
    // This method is now a placeholder
    // Initialize the immutable vars stack with an empty set for the global scope
    ImmutableVars.push_back(std::set<std::string>());
}

llvm::Function *CodeGenerator::codegen(FunctionAST &F) {
    return funcCodeGen->codegen(F);
}

llvm::Value *CodeGenerator::codegen(ExprAST &E) {
    return exprCodeGen->codegen(E);
}

void CodeGenerator::codegen(StructDeclAST &S) {
    typeCodeGen->codegen(S);
}

void CodeGenerator::codegen(ClassDeclAST &C) {
    typeCodeGen->codegen(C);
}

void CodeGenerator::enterScope() {
    ScopeStack.push_back(ScopeLayer());
    ImmutableVars.push_back(std::set<std::string>());
}

void CodeGenerator::emitCleanup(const ScopeLayer& layer) {
    // Check if the current block is terminated. If so, we can't emit instructions.
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
        Builder->CreateCall(utilCodeGen->getFreeFunc(), {voidPtr});
    }
}

void CodeGenerator::exitScope() {
    if (!ScopeStack.empty()) {
        auto& layer = ScopeStack.back();
        emitCleanup(layer);
        ScopeStack.pop_back();
        if (!ImmutableVars.empty()) {
            ImmutableVars.pop_back();
        }
    } else {
        logError("Error: Scope stack underflow");
    }
}

void CodeGenerator::registerCleanup(llvm::AllocaInst* var) {
    if (ScopeStack.empty()) enterScope();
    ScopeStack.back().cleanupVars.push_back(var);
}

void CodeGenerator::addVariable(const std::string& name, llvm::AllocaInst* val) {
    if (ScopeStack.empty()) enterScope();
    ScopeStack.back().values[name] = val;
}

llvm::AllocaInst* CodeGenerator::getVariable(const std::string& name) {
    for (auto it = ScopeStack.rbegin(); it != ScopeStack.rend(); ++it) {
        if (it->values.count(name)) {
            return it->values.at(name);
        }
    }
    return nullptr;
}

void CodeGenerator::addVariableType(const std::string& name, OType type) {
    if (ScopeStack.empty()) enterScope();
    ScopeStack.back().types[name] = type;
}

OType CodeGenerator::getVariableType(const std::string& name) {
    for (auto it = ScopeStack.rbegin(); it != ScopeStack.rend(); ++it) {
        if (it->types.count(name)) {
            return it->types.at(name);
        }
    }
    return OType(BaseType::Void);
}

llvm::AllocaInst *CodeGenerator::createEntryBlockAlloca(llvm::Function *TheFunction,
                                          const std::string &VarName, llvm::Type* Type) {
    llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                     TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type, nullptr, VarName);
}

llvm::Type* CodeGenerator::getLLVMType(const OType& t) {
    return utilCodeGen->getLLVMType(t);
}

void CodeGenerator::logError(const char *Str) {
    fprintf(stderr, "Error: %s\n", Str);
}

void CodeGenerator::processDeferredInstantiations() {
    // Phase 1: Set phase to InstantiatingGenerics to process struct skeletons
    CompilerPhase oldPhase = CurrentPhase;
    CurrentPhase = CompilerPhase::InstantiatingGenerics;

    // Process all pending instantiations to create struct skeletons and method prototypes
    while (!utilCodeGen->getInstantiationQueue().empty()) {
        // Move all pending items to a local batch
        std::vector<PendingInstantiation> CurrentBatch;
        CurrentBatch.swap(utilCodeGen->getInstantiationQueue());

        // Process this batch - only create struct layouts and method prototypes
        for (auto& Item : CurrentBatch) {
            // The struct skeleton and prototypes should already be created by instantiateStructSkeleton
            // We just need to ensure all necessary instantiations are processed
            std::cerr << "Processing struct skeleton: " << Item.MangledName << "\n";
        }
    }

    // Phase 2: Set phase to GeneratingBodies to process method bodies
    CurrentPhase = CompilerPhase::GeneratingBodies;

    // Generate all method and constructor bodies
    utilCodeGen->generateInstantiatedBodies();

    // Restore the original phase
    CurrentPhase = oldPhase;
}