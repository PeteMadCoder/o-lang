#pragma once
#include <string>
#include <string_view>

enum class TokenType {
    // End of File
    EoF,

    // Identifiers & Literals
    Identifier, Integer, Float, CharLit, StringLit,

    // Keywords
    Fn, Var, Let, Mut, Return, If, Else, While, For, Struct, Class, Import, Module,
    Open, Virtual, Override, New, Shared, Unsafe, Match, Delete, As, Static,
    
    // Primitive Types
    TypeInt, TypeFloat, TypeBool, TypeVoid, TypeChar, TypeByte,
    
    // Boolean Literals
    True, False,
    
    // Operators
    Plus, Minus, Star, Slash, Mod, Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
    Assign, Ampersand, Arrow, Dot, Comma, Semicolon, Colon, EqualEqual,
    LogicalAnd, LogicalOr, Bang,
    
    // Delimiters
    LParen, RParen, LBrace, RBrace, LBracket, RBracket
};

struct Token {
    TokenType type;
    std::string text; // The actual string content (e.g., "myVar", "123")
    int line;
    int column;
};

class Lexer {
public:
    Lexer(const std::string& source);
    Token next_token(); // Returns the next token and advances

private:
    std::string src;
    size_t pos = 0;
    int line = 1;
    int col = 1;

    char current();
    char peek();
    void advance();
    void skip_whitespace();
    Token atom(TokenType type);
    Token identifier(); // Handles keywords vs identifiers
    Token number();
    Token character(); // Handle character literals
    Token string(); // Handle string literals
};