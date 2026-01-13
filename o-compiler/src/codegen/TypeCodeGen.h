#pragma once

#include "CodeGenerator.h"
#include "../AST.h"

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
    void generateMethodBodies(StructDeclAST &S, const std::string& mangledName);
    void generateConstructorBodies(StructDeclAST &S, const std::string& mangledName);
    void generateMethodBodies(ClassDeclAST &C, const std::string& mangledName);
    void generateConstructorBodies(ClassDeclAST &C, const std::string& mangledName);
};