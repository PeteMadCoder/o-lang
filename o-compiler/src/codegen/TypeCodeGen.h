#pragma once

#include "CodeGenerator.h"

class TypeCodeGen {
private:
    CodeGenerator &codeGen;

public:
    explicit TypeCodeGen(CodeGenerator &cg) : codeGen(cg) {}

    void codegen(StructDeclAST &S);
    void codegen(ClassDeclAST &C);
    void processDeferredInstantiations();
    
private:
    void generateVTable(ClassDeclAST &C);
};