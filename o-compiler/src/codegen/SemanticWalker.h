#pragma once

#include "CodeGenerator.h"
#include "../AST.h"
#include "InstantiationManager.h"

class SemanticWalker {
private:
    CodeGenerator &codeGen;

public:
    explicit SemanticWalker(CodeGenerator &cg) : codeGen(cg) {}

    // Walk through expressions to discover semantic requirements
    void walk(ExprAST &E);
    void walk(FunctionAST &F);
    void walk(StructDeclAST &S);
    void walk(ClassDeclAST &C);

    // Specific walker methods for different expression types
    void walk(BoolExprAST &E);
    void walk(StringExprAST &E);
    void walk(CastExprAST &E);
    void walk(NumberExprAST &E);
    void walk(VariableExprAST &E);
    void walk(VarDeclExprAST &E);
    void walk(AssignmentExprAST &E);
    void walk(ReturnExprAST &E);
    void walk(DeleteExprAST &E);
    void walk(NegateExprAST &E);
    void walk(BinaryExprAST &E);
    void walk(CallExprAST &E);
    void walk(MethodCallExprAST &E);
    void walk(BlockExprAST &E);
    void walk(IfExprAST &E);
    void walk(WhileExprAST &E);
    void walk(ForExprAST &E);
    void walk(AddressOfExprAST &E);
    void walk(DerefExprAST &E);
    void walk(NewExprAST &E);
    void walk(NewArrayExprAST &E);
    void walk(MatchExprAST &E);
    void walk(IndexExprAST &E);
    void walk(MemberAccessAST &E);
    void walk(ArrayInitExprAST &E);
    void walk(ArrayLiteralExprAST &E);

private:
    // Helper methods to enqueue instantiations
    void enqueueStructInstantiation(const std::string& baseName, const std::vector<OType>& typeArgs);
    void enqueueMethodInstantiation(const std::string& baseName, const std::string& methodName, const std::vector<OType>& typeArgs);
};