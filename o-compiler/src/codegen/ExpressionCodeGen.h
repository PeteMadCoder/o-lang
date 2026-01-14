#pragma once

#include "CodeGenerator.h"

class ExpressionCodeGen {
private:
    CodeGenerator &codeGen;

public:
    explicit ExpressionCodeGen(CodeGenerator &cg) : codeGen(cg) {}

    llvm::Value *codegen(ExprAST &E);
    
    // Individual expression codegen methods
    llvm::Value *codegen(BoolExprAST &E);
    llvm::Value *codegen(StringExprAST &E);
    llvm::Value *codegen(CastExprAST &E);
    llvm::Value *codegen(NumberExprAST &E);
    llvm::Value *codegen(VariableExprAST &E);
    llvm::Value *codegen(VarDeclExprAST &E);
    llvm::Value *codegen(AssignmentExprAST &E);
    llvm::Value *codegen(ReturnExprAST &E);
    llvm::Value *codegen(DeleteExprAST &E);
    llvm::Value *codegen(NegateExprAST &E);
    llvm::Value *codegen(NotExprAST &E);
    llvm::Value *codegen(BinaryExprAST &E);
    llvm::Value *codegen(CallExprAST &E);
    llvm::Value *codegen(MethodCallExprAST &E);
    llvm::Value *codegen(BlockExprAST &E);
    llvm::Value *codegen(IfExprAST &E);
    llvm::Value *codegen(WhileExprAST &E);
    llvm::Value *codegen(ForExprAST &E);
    llvm::Value *codegen(AddressOfExprAST &E);
    llvm::Value *codegen(DerefExprAST &E);
    llvm::Value *codegen(NewExprAST &E);
    llvm::Value *codegen(NewArrayExprAST &E);
    llvm::Value *codegen(MatchExprAST &E);
    llvm::Value *codegen(IndexExprAST &E);
    llvm::Value *codegen(MemberAccessAST &E);
    llvm::Value *codegen(ArrayInitExprAST &E);
    llvm::Value *codegen(ArrayLiteralExprAST &E);

    // Address generation methods
    llvm::Value *codegenAddress(VariableExprAST &E);
    llvm::Value *codegenAddress(IndexExprAST &E);
    llvm::Value *codegenAddress(MemberAccessAST &E);
    llvm::Value *codegenAddress(DerefExprAST &E);

    // Generic address generation method
    llvm::Value *codegenAddress(ExprAST &E);
};