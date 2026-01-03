#pragma once
#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include <utility>

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

// 1. Primitive Type System
enum class OType { Void, Int, Float, Bool };

// Helper to get type from string
inline OType stringToType(const std::string& t) {
    if (t == "int") return OType::Int;
    if (t == "float") return OType::Float;
    if (t == "bool") return OType::Bool;
    return OType::Void;
}

// Base Class for all AST nodes
class ExprAST {
public:
    virtual ~ExprAST() = default;
    
    // The magical method that generates LLVM code
    virtual llvm::Value *codegen() = 0;
};

// 2a. Boolean Node
class BoolExprAST : public ExprAST {
    bool Val;
public:
    BoolExprAST(bool Val) : Val(Val) {}
    llvm::Value *codegen() override;
};

// 2b. While Loop Node
class WhileExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond;
    std::unique_ptr<ExprAST> Body;
public:
    WhileExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Body)
        : Cond(std::move(Cond)), Body(std::move(Body)) {}
    llvm::Value *codegen() override;
};

// 2c. Updated Number Node
class NumberExprAST : public ExprAST {
    double Val;
    OType Type; // Store the type
public:
    NumberExprAST(double Val, OType Type) : Val(Val), Type(Type) {}
    llvm::Value *codegen() override;
    OType getType() const { return Type; } // Type getter
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
    std::string Name;
public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    llvm::Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator (e.g., "+", "<").
class BinaryExprAST : public ExprAST {
    char Op; // The operator token (e.g., '+')
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                  std::unique_ptr<ExprAST> RHS)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    llvm::Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;
public:
    CallExprAST(const std::string &Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
        : Callee(Callee), Args(std::move(Args)) {}
    llvm::Value *codegen() override;
};

// 3. New Block Node
class BlockExprAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Expressions;
public:
    BlockExprAST(std::vector<std::unique_ptr<ExprAST>> Exprs) 
        : Expressions(std::move(Exprs)) {}
    
    llvm::Value *codegen() override;
};

// 4. If Expression Node
class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond, Then, Else;
public:
    IfExprAST(std::unique_ptr<ExprAST> Cond,
              std::unique_ptr<ExprAST> Then,
              std::unique_ptr<ExprAST> Else)
        : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

    llvm::Value *codegen() override;
};

// 5. Var Declaration Node: var x = 10;
class VarDeclExprAST : public ExprAST {
    std::string Name;
    std::unique_ptr<ExprAST> Init;
public:
    VarDeclExprAST(const std::string &Name, std::unique_ptr<ExprAST> Init)
        : Name(Name), Init(std::move(Init)) {}
    llvm::Value *codegen() override;
};

// 6. Assignment Node: x = 20;
class AssignmentExprAST : public ExprAST {
    std::string Name;
    std::unique_ptr<ExprAST> RHS;
public:
    AssignmentExprAST(const std::string &Name, std::unique_ptr<ExprAST> RHS)
        : Name(Name), RHS(std::move(RHS)) {}
    llvm::Value *codegen() override;
};

// 7. Return Node: return x;
class ReturnExprAST : public ExprAST {
    std::unique_ptr<ExprAST> RetVal;
public:
    ReturnExprAST(std::unique_ptr<ExprAST> RetVal)
        : RetVal(std::move(RetVal)) {}
    llvm::Value *codegen() override;
};

// 8. Updated Prototype to store types
class PrototypeAST {
    std::string Name;
    std::vector<std::pair<std::string, OType>> Args; // Name + Type
    OType ReturnType;
public:
    PrototypeAST(const std::string &Name, 
                 std::vector<std::pair<std::string, OType>> Args, 
                 OType RetType)
        : Name(Name), Args(std::move(Args)), ReturnType(RetType) {}

    llvm::Function *codegen();
    const std::string &getName() const { return Name; }
};

/// FunctionAST - Represents a full function definition (Proto + Body).
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body; // This will eventually be a BlockAST
public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) {}
    llvm::Function *codegen();
};