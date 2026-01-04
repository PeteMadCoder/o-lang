#pragma once
#include <string>
#include <string_view>

enum class TokenType {
    // End of File
    EoF,

    // Keywords
    Fn, Var, Mut, Return, If, Else, While, Class, Struct, Module, Import,
    Open, Virtual, Override, New, Shared, Unsafe, Match,

    // Types
    TypeInt, TypeFloat, TypeBool, TypeVoid,

    // Identifiers & Literals
    Identifier, Integer, Float, StringLit, True, False,

    // Operators & Punctuation
    Plus, Minus, Star, Slash, Equal,           // + - * / =
    EqualEqual, NotEqual, Less, Greater,       // == != < >
    Arrow,                                     // ->
    LParen, RParen, LBrace, RBrace,            // ( ) { }
    LBracket, RBracket,                        // [ ]
    Semicolon, Colon, Comma, Dot               // ; : , .
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
};