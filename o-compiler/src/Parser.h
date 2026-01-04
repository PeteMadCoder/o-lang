#pragma once
#include "AST.h"
#include "Lexer.h"
#include <map>
#include <memory>
#include <vector>

class Parser {
    Lexer& lexer;
    Token curTok;

public:
    Parser(Lexer& lex);

    // Advance to the next token
    void getNextToken();

    // Get the precedence of the pending binary operator token.
    int GetTokPrecedence();

    // Error Handling
    std::unique_ptr<ExprAST> LogError(const char* str);
    std::unique_ptr<PrototypeAST> LogErrorP(const char* str);

    // --- Expression Parsing ---

    // Parse a type (e.g., "int", "float")
    OType ParseType();

    // num ::= number
    std::unique_ptr<ExprAST> ParseNumberExpr();

    // parenexpr ::= '(' expression ')'
    std::unique_ptr<ExprAST> ParseParenExpr();

    // identifierexpr
    //   ::= identifier
    //   ::= identifier '=' expression
    //   ::= identifier '(' expression* ')'
    std::unique_ptr<ExprAST> ParseIdentifierExpr();

    // ifexpr ::= 'if' '(' expression ')' block ('else' block)?
    std::unique_ptr<ExprAST> ParseIfExpr();

    // primary
    //   ::= identifierexpr
    //   ::= numberexpr
    //   ::= parenexpr
    //   ::= ifexpr
    std::unique_ptr<ExprAST> ParsePrimary();

    // binoprhs
    //   ::= ('+' primary)*
    std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);

    // expression
    //   ::= primary binoprhs
    std::unique_ptr<ExprAST> ParseExpression();
    
    // vardecl ::= 'var' identifier '=' expression ';'
    std::unique_ptr<ExprAST> ParseVarDecl();
    
    // whilestmt ::= 'while' '(' expression ')' block
    std::unique_ptr<ExprAST> ParseWhileStmt();

    // returnstmt ::= 'return' expression? ';'
    std::unique_ptr<ExprAST> ParseReturnStmt();
    
    // statement
    std::unique_ptr<ExprAST> ParseStatement();

    // New: Parse Block { stmt; stmt; }
    std::unique_ptr<ExprAST> ParseBlock();

    // --- Top Level Parsing ---

    // prototype
    //   ::= fn Name(Args) -> Type
    std::unique_ptr<PrototypeAST> ParsePrototype();

    // definition ::= prototype expression
    std::unique_ptr<FunctionAST> ParseDefinition();
    
    // Helper to check if we are at EOF
    bool isEOF();
};
