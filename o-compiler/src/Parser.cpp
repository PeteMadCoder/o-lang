#include "Parser.h"
#include <iostream>

// Operator Precedence Table
static std::map<char, int> BinopPrecedence = {
    {'<', 10},
    {'>', 10},
    {'+', 20},
    {'-', 20},
    {'*', 40},
    {'/', 40}
};

Parser::Parser(Lexer& lex) : lexer(lex) {
    getNextToken(); // Prime the pump
}

void Parser::getNextToken() {
    curTok = lexer.next_token();
}

int Parser::GetTokPrecedence() {
    if (curTok.type == TokenType::Plus) return 20;
    if (curTok.type == TokenType::Minus) return 20;
    if (curTok.type == TokenType::Star) return 40;
    if (curTok.type == TokenType::Slash) return 40;
    if (curTok.type == TokenType::Less) return 10;
    if (curTok.type == TokenType::Greater) return 10;
    return -1;
}

// Error Handling
std::unique_ptr<ExprAST> Parser::LogError(const char* str) {
    fprintf(stderr, "Error at line %d, column %d: %s. Current Token: '%s'\n", 
            curTok.line, curTok.column, str, curTok.text.c_str());
    return nullptr;
}

std::unique_ptr<PrototypeAST> Parser::LogErrorP(const char* str) {
    LogError(str);
    return nullptr;
}

// --- Expression Parsing ---

OType Parser::ParseType() {
    if (curTok.type == TokenType::TypeInt) { getNextToken(); return OType::Int; }
    if (curTok.type == TokenType::TypeFloat) { getNextToken(); return OType::Float; }
    if (curTok.type == TokenType::TypeBool) { getNextToken(); return OType::Bool; }
    return OType::Void;
}

std::unique_ptr<ExprAST> Parser::ParseNumberExpr() {
    bool isFloat = (curTok.type == TokenType::Float);
    double val = std::stod(curTok.text);
    getNextToken();
    return std::make_unique<NumberExprAST>(val, isFloat ? OType::Float : OType::Int);
}

std::unique_ptr<ExprAST> Parser::ParseParenExpr() {
    getNextToken(); // eat (
    auto V = ParseExpression();
    if (!V) return nullptr;

    if (curTok.type != TokenType::RParen)
        return LogError("expected ')'");
    getNextToken(); // eat )
    return V;
}

std::unique_ptr<ExprAST> Parser::ParseIdentifierExpr() {
    std::string IdName = curTok.text;
    getNextToken(); // eat identifier

    // Check for Assignment: x = 10
    if (curTok.type == TokenType::Equal) {
            getNextToken(); // eat '='
            auto RHS = ParseExpression();
            if (!RHS) return nullptr;
            return std::make_unique<AssignmentExprAST>(IdName, std::move(RHS));
    }

    // Simple variable ref
    if (curTok.type != TokenType::LParen)
        return std::make_unique<VariableExprAST>(IdName);

    // Function Call
    getNextToken(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (curTok.type != TokenType::RParen) {
        while (true) {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (curTok.type == TokenType::RParen) break;

            if (curTok.type != TokenType::Comma)
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }
    getNextToken(); // eat )

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

std::unique_ptr<ExprAST> Parser::ParseIfExpr() {
    getNextToken(); // eat 'if'

    if (curTok.type != TokenType::LParen) 
        return LogError("Expected '(' after if");
    getNextToken(); // eat '('
    
    // Parse condition (e.g., "x < 10")
    auto Cond = ParseExpression();
    if (!Cond) return nullptr;

    if (curTok.type != TokenType::RParen)
        return LogError("Expected ')' after condition");
    getNextToken(); // eat ')'

    // Parse 'Then' block
    if (curTok.type != TokenType::LBrace)
        return LogError("Expected '{' for then block");
    auto Then = ParseBlock();
    if (!Then) return nullptr;

    // Parse Optional 'Else' block
    std::unique_ptr<ExprAST> Else = nullptr;
    if (curTok.type == TokenType::Else) {
        getNextToken(); // eat 'else'
        
        if (curTok.type != TokenType::LBrace)
            return LogError("Expected '{' for else block");
        Else = ParseBlock();
    }

    return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then), std::move(Else));
}

std::unique_ptr<ExprAST> Parser::ParsePrimary() {
    switch (curTok.type) {
        case TokenType::True:       getNextToken(); return std::make_unique<BoolExprAST>(true);
        case TokenType::False:      getNextToken(); return std::make_unique<BoolExprAST>(false);
        case TokenType::If:         return ParseIfExpr();
        case TokenType::Identifier: return ParseIdentifierExpr();
        case TokenType::Integer:    return ParseNumberExpr();
        case TokenType::Float:      return ParseNumberExpr();
        case TokenType::LParen:     return ParseParenExpr();
        default: return LogError("unknown token when expecting an expression");
    }
}

std::unique_ptr<ExprAST> Parser::ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = GetTokPrecedence();

        if (TokPrec < ExprPrec)
            return LHS;

        char BinOp = curTok.text[0]; 
        getNextToken(); // eat binop

        auto RHS = ParsePrimary();
        if (!RHS) return nullptr;

        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS) return nullptr;
        }

        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

std::unique_ptr<ExprAST> Parser::ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS) return nullptr;
    return ParseBinOpRHS(0, std::move(LHS));
}

std::unique_ptr<ExprAST> Parser::ParseVarDecl() {
    getNextToken(); // eat 'var'
    
    if (curTok.type != TokenType::Identifier)
        return LogError("Expected identifier after var");
    
    std::string Name = curTok.text;
    getNextToken(); // eat identifier
    
    if (curTok.type != TokenType::Equal)
            return LogError("Expected '=' in variable declaration");
    getNextToken(); // eat '='
    
    auto Init = ParseExpression();
    if (!Init) return nullptr;
    
    if (curTok.type != TokenType::Semicolon) 
        return LogError("Expected ';' after variable declaration");
    getNextToken(); // eat ';'
    
    return std::make_unique<VarDeclExprAST>(Name, std::move(Init));
}

std::unique_ptr<ExprAST> Parser::ParseWhileStmt() {
    getNextToken(); // eat 'while'
    
    if (curTok.type != TokenType::LParen)
        return LogError("Expected '(' after while");
    getNextToken(); // eat '('
    
    auto Cond = ParseExpression();
    if (!Cond) return nullptr;
    
    if (curTok.type != TokenType::RParen)
        return LogError("Expected ')' after condition");
    getNextToken(); // eat ')'
    
    if (curTok.type != TokenType::LBrace)
        return LogError("Expected '{' for while block");
        
    auto Body = ParseBlock();
    if (!Body) return nullptr;
    
    return std::make_unique<WhileExprAST>(std::move(Cond), std::move(Body));
}

std::unique_ptr<ExprAST> Parser::ParseReturnStmt() {
    getNextToken(); // eat 'return'
    
    std::unique_ptr<ExprAST> Res = nullptr;
    if (curTok.type != TokenType::Semicolon) {
        Res = ParseExpression();
    }
    
    if (curTok.type != TokenType::Semicolon)
        return LogError("Expected ';' after return");
    getNextToken(); // eat ';'
    
    return std::make_unique<ReturnExprAST>(std::move(Res));
}

std::unique_ptr<ExprAST> Parser::ParseStatement() {
    if (curTok.type == TokenType::Return) {
        return ParseReturnStmt();
    }
    if (curTok.type == TokenType::While) {
        return ParseWhileStmt();
    }
    if (curTok.type == TokenType::Var) {
        return ParseVarDecl();
    }
    
    // Expression statement
    auto Expr = ParseExpression();
    if (!Expr) return nullptr;
    
    if (curTok.type == TokenType::Semicolon) {
        getNextToken(); // eat ';'
    }
    return Expr;
}

std::unique_ptr<ExprAST> Parser::ParseBlock() {
    getNextToken(); // eat '{'
    
    std::vector<std::unique_ptr<ExprAST>> Stmts;
    
    // Loop until '}'
    while (curTok.type != TokenType::RBrace && curTok.type != TokenType::EoF) {
        auto Stmt = ParseStatement();
        if (!Stmt) return nullptr;
        
        Stmts.push_back(std::move(Stmt));
    }

    if (curTok.type != TokenType::RBrace) return LogError("Expected '}'");
    getNextToken(); // eat '}'

    return std::make_unique<BlockExprAST>(std::move(Stmts));
}

std::unique_ptr<PrototypeAST> Parser::ParsePrototype() {
    if (curTok.type != TokenType::Fn) return LogErrorP("Expected 'fn'");
    getNextToken(); // eat 'fn'

    if (curTok.type != TokenType::Identifier) return LogErrorP("Expected function name");
    std::string FnName = curTok.text;
    getNextToken();

    if (curTok.type != TokenType::LParen) return LogErrorP("Expected '('");
    getNextToken();

    std::vector<std::pair<std::string, OType>> Args;
    while (curTok.type == TokenType::TypeInt || curTok.type == TokenType::TypeFloat) {
        // Parse "Type Name"
        OType ArgType = ParseType();
        
        if (curTok.type != TokenType::Identifier) return LogErrorP("Expected arg name");
        std::string ArgName = curTok.text;
        Args.push_back({ArgName, ArgType});
        getNextToken();

        if (curTok.type == TokenType::Comma) getNextToken();
    }

    if (curTok.type != TokenType::RParen) return LogErrorP("Expected ')'");
    getNextToken();

    // Parse Return Type
    OType RetType = OType::Void;
    if (curTok.type == TokenType::Arrow) {
        getNextToken(); // eat ->
        RetType = ParseType(); // Parse "int" or "float"
    }

    return std::make_unique<PrototypeAST>(FnName, std::move(Args), RetType);
}

std::unique_ptr<FunctionAST> Parser::ParseDefinition() {
    auto Proto = ParsePrototype();
    if (!Proto) return nullptr;

    if (curTok.type == TokenType::Semicolon) {
            getNextToken(); // eat ;
            return std::make_unique<FunctionAST>(std::move(Proto), nullptr);
    }

    if (curTok.type != TokenType::LBrace) return nullptr;
    
    auto Body = ParseBlock(); 
    if (Body) {
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
    }
    return nullptr;
}

bool Parser::isEOF() { return curTok.type == TokenType::EoF; }