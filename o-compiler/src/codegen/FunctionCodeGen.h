#pragma once

#include "CodeGenerator.h"

class FunctionCodeGen {
private:
    CodeGenerator &codeGen;

public:
    explicit FunctionCodeGen(CodeGenerator &cg) : codeGen(cg) {}

    llvm::Function *codegen(FunctionAST &F);
    llvm::Function *codegen(PrototypeAST &P);
    llvm::Function *codegen(ConstructorAST &C, const std::string& structName);
};