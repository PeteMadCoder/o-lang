#pragma once

#include "CodeGenerator.h"

class UtilityCodeGen {
private:
    CodeGenerator &codeGen;

public:
    explicit UtilityCodeGen(CodeGenerator &cg) : codeGen(cg) {}

    // Helper functions that were originally in CodeGen.cpp
    llvm::Function *getFreeFunc();
    llvm::Type* getLLVMType(const OType& t);
    std::string mangleGenericName(const std::string& baseName, const std::vector<OType>& args);
    llvm::Type* instantiateStruct(const std::string& genericName, const std::vector<OType>& typeArgs);
    llvm::Function *getFunctionFromPrototype(std::string Name);
    void RegisterFunctionProto(std::unique_ptr<PrototypeAST> Proto);
    llvm::StructType* getSliceType(llvm::Type* ElementType);
    void createBoundsCheck(llvm::Value *IndexVal, llvm::Value *ArraySizeVal);
    llvm::Function *getTrapFunc();

    // Accessor for the instantiation queue (accessed through codeGen)
    std::vector<PendingInstantiation>& getInstantiationQueue();

    // Check if we're in codegen phase (for hard ban)
    bool isInCodegenPhase() const { return inCodegenPhase; }

    // Setters for phase tracking
    void enterCodegenPhase() { inCodegenPhase = true; }
    void exitCodegenPhase() { inCodegenPhase = false; }

private:
    // Track ongoing instantiations to prevent infinite recursion
    std::set<std::string> InProgressInstantiations;

    // Track if we're currently in codegen phase
    bool inCodegenPhase = false;
};