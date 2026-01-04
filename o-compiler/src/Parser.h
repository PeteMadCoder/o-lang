#pragma once
#include "AST.h"
#include "Lexer.h"
#include <map>
#include <memory>
#include <vector>

class Parser {
    Lexer& lexer;
    Token curTok;
    int unsafeDepth = 0; // Track unsafe block nesting depth

public:
    Parser(Lexer& lex);

    // Advance to the next token
    void getNextToken();

    // Get the precedence of the pending binary operator token.
    int GetTokPrecedence();

    // Error Handling
    std::unique_ptr<ExprAST> LogError(const char* str);
    std::unique_ptr<PrototypeAST> LogErrorP(const char* str);
    void LogSafetyError(const char* str); // Safety-specific error handling

    // Safety Context Management
    void enterUnsafe() { unsafeDepth++; }
    void exitUnsafe() { unsafeDepth--; }
    bool isInUnsafeContext() const { return unsafeDepth > 0; }
    
    // Demonstration method for safety checking
    void checkUnsafeOperation(const char* operation) {
        if (unsafeDepth == 0) {
            LogSafetyError(operation);
        }
    }

    // --- Expression Parsing ---

    // Parse a type (e.g., "int", "float", "char", "byte")
    OType ParseType();

    // num ::= number
    std::unique_ptr<ExprAST> ParseNumberExpr();
    
    // char ::= 'c'
    std::unique_ptr<ExprAST> ParseCharExpr();
    
    // string ::= "hello"
    std::unique_ptr<ExprAST> ParseStringExpr();
    
    // new ::= 'new' Identifier '(' args ')'
    std::unique_ptr<ExprAST> ParseNewExpr();
    
    // arraytype ::= Type '[' size ']'
    std::unique_ptr<ExprAST> ParseArrayTypeExpr();
    
    // arrayinit ::= '{' expression (',' expression)* '}'
    std::unique_ptr<ExprAST> ParseArrayInitExpr();

    // parenexpr ::= '(' expression ')'
    std::unique_ptr<ExprAST> ParseParenExpr();

    // identifierexpr
    //   ::= identifier
    //   ::= identifier '=' expression
    //   ::= identifier '(' expression* ')'
    std::unique_ptr<ExprAST> ParseIdentifierExpr();

    // ifexpr ::= 'if' '(' expression ')' block ('else' block)?
    std::unique_ptr<ExprAST> ParseIfExpr();

    // matchexpr ::= 'match' '(' expression ')' '{' case* '}'
    std::unique_ptr<ExprAST> ParseMatchExpr();

    // primary
    //   ::= identifierexpr
    //   ::= numberexpr
    //   ::= parenexpr
    //   ::= ifexpr
    std::unique_ptr<ExprAST> ParsePrimary();
    
    // postfix
    //   ::= primary postfixop*
    //   postfixop ::= '.' identifier
    std::unique_ptr<ExprAST> ParsePostfix();
    
    // unary
    //   ::= '&' unary
    //   ::= postfix
    std::unique_ptr<ExprAST> ParseUnary();

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

    // forstmt ::= 'for' '(' (statement | ';') expression? ';' expression? ')' block
    std::unique_ptr<ExprAST> ParseForStmt();

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
    
    // struct ::= 'struct' Name '{' field* '}'
    std::unique_ptr<StructDeclAST> ParseStruct();
    
    // class ::= 'class' Name [':' Parent] '{' member* '}'
    std::unique_ptr<ClassDeclAST> ParseClass();
    
    // Parse top-level declaration (function, struct, or class)
    bool ParseTopLevel();
    
    // Parse identifier list: name1, name2, name3
    std::vector<std::string> ParseIdentifierList();
    
    // Parse constructor: new(params) { ... }
    std::unique_ptr<ConstructorAST> ParseConstructor();
    
    // Parse generic parameters: <T, U, V>
    std::vector<std::string> ParseGenericParams();
    
    // Parse unsafe block: unsafe { ... }
    std::unique_ptr<ExprAST> ParseUnsafeBlock();
    
    // Helper to check if we are at EOF
    bool isEOF();
};
