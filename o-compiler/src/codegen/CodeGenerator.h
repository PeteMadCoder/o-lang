#pragma once

#include "../AST.h"
#include <map>
#include <set>
#include <vector>
#include <string>
#include <memory>
#include <queue>

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

// Forward declarations for codegen modules
class ExpressionCodeGen;
class StatementCodeGen;
class FunctionCodeGen;
class TypeCodeGen;
class TypeResolver;
class SemanticWalker;
class UtilityCodeGen;
class InstantiationManager;

// Deferred Instantiation Queue (deprecated - kept for transition)
struct PendingInstantiation {
    std::unique_ptr<StructDeclAST> AST;
    std::string MangledName;
    std::vector<OType> TypeArgs;  // Store the type arguments for later substitution
};

// Scope Management
struct ScopeLayer {
    std::map<std::string, llvm::AllocaInst *> values;
    std::map<std::string, OType> types;
    std::vector<llvm::AllocaInst *> cleanupVars; // Variables to free on exit
};

// Compiler phase enumeration for strict phase separation
enum class CompilerPhase {
    Parsing,
    InstantiatingGenerics,
    GeneratingBodies
};

// Main Code Generator class that orchestrates the different modules
class CodeGenerator {
public:
    // Global Compiler State - References to global objects
    llvm::LLVMContext *TheContext;
    llvm::IRBuilder<> *Builder;
    llvm::Module *TheModule;

    // Compiler phase tracking
    CompilerPhase CurrentPhase = CompilerPhase::Parsing;

    // Scope management
    std::vector<ScopeLayer> ScopeStack;
    std::vector<std::set<std::string>> ImmutableVars; // Stack of scopes for immutable variables
    std::map<std::string, OType> FunctionReturnTypes;

    // Type management
    std::unordered_map<std::string, llvm::StructType*> StructTypes;
    // Generic Struct Registry for Monomorphization
    std::map<std::string, std::unique_ptr<StructDeclAST>> GenericStructRegistry;

    // Global registry for function prototypes to enable lazy symbol resolution during generic instantiation
    std::map<std::string, std::shared_ptr<PrototypeAST>> GlobalFunctionProtos;

    // Deferred Instantiation Queue (deprecated - kept for transition)
    std::vector<PendingInstantiation> InstantiationQueue;

    // New instantiation manager
    std::unique_ptr<InstantiationManager> instantiationManager;

    // Module instances
    std::unique_ptr<ExpressionCodeGen> exprCodeGen;
    std::unique_ptr<StatementCodeGen> stmtCodeGen;
    std::unique_ptr<FunctionCodeGen> funcCodeGen;
    std::unique_ptr<TypeCodeGen> typeCodeGen;
    std::unique_ptr<TypeResolver> typeResolver;
    std::unique_ptr<UtilityCodeGen> utilCodeGen;

    // Import context tracking
    bool inImportContext = false;

    CodeGenerator();
    ~CodeGenerator();

    // Initialize the LLVM infrastructure
    void initializeModuleAndPassManager();

    // Main code generation entry point
    llvm::Function *codegen(FunctionAST &F);
    llvm::Value *codegen(ExprAST &E);
    void codegen(StructDeclAST &S);
    void codegen(ClassDeclAST &C);

    // Scope management
    void enterScope();
    void exitScope();
    void registerCleanup(llvm::AllocaInst* var);
    void addVariable(const std::string& name, llvm::AllocaInst* val);
    llvm::AllocaInst* getVariable(const std::string& name);
    void addVariableType(const std::string& name, OType type);
    OType getVariableType(const std::string& name);

    // Utility functions that might be needed across modules
    llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *TheFunction,
                                           const std::string &VarName, llvm::Type* Type);
    llvm::Type* getLLVMType(const OType& t);
    void logError(const char *Str);

    // Deferred instantiation processing
    void processDeferredInstantiations();

    // Public method to emit cleanup for scope layers
    void emitCleanup(const ScopeLayer& layer);

private:
};