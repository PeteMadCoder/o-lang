#pragma once

#include "CodeGenerator.h"
#include "../AST.h"

class TypeResolver {
private:
    CodeGenerator &codeGen;

public:
    explicit TypeResolver(CodeGenerator &cg) : codeGen(cg) {}

    // Semantic discovery methods - no LLVM IR generation
    void resolve(StructDeclAST &S);
    void resolve(ClassDeclAST &C);
    
    // Process all pending instantiations semantically
    void processSemanticResolution();

private:
    void resolveMethods(StructDeclAST &S);
    void resolveConstructors(StructDeclAST &S);
    void resolveMethods(ClassDeclAST &C);
    void resolveConstructors(ClassDeclAST &C);
};