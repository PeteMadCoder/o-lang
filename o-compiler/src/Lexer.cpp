#include "Lexer.h"
#include <cctype>
#include <unordered_map>

// Helper to map string keywords to TokenTypes
static std::unordered_map<std::string, TokenType> keywords = {
    {"fn", TokenType::Fn}, {"var", TokenType::Var}, {"mut", TokenType::Mut},
    {"return", TokenType::Return}, {"if", TokenType::If}, {"else", TokenType::Else},
    {"while", TokenType::While}, {"for", TokenType::For}, {"class", TokenType::Class}, {"struct", TokenType::Struct},
    {"module", TokenType::Module}, {"import", TokenType::Import}, {"new", TokenType::New},
    {"unsafe", TokenType::Unsafe}, {"open", TokenType::Open}, {"virtual", TokenType::Virtual},
    {"match", TokenType::Match}, {"delete", TokenType::Delete}, {"override", TokenType::Override},
    {"int", TokenType::TypeInt}, {"float", TokenType::TypeFloat},
    {"bool", TokenType::TypeBool}, {"void", TokenType::TypeVoid},
    {"char", TokenType::TypeChar}, {"byte", TokenType::TypeByte},
    {"true", TokenType::True}, {"false", TokenType::False}
};

Lexer::Lexer(const std::string& source) : src(source) {}

char Lexer::peek() {
    if (pos >= src.length()) return '\0';
    return src[pos];
}

char Lexer::current() {
    if (pos > src.length()) return '\0';
    return src[pos - 1];
}

void Lexer::advance() {
    if (pos < src.length()) {
        pos++;
        col++;
    }
}

Token Lexer::atom(TokenType type) {
    return Token{type, std::string(1, current()), line, col};
}

void Lexer::skip_whitespace() {
    while (true) {
        char c = peek();
        if (c == ' ' || c == '\r' || c == '\t') {
            advance();
        } else if (c == '\n') {
            line++;
            col = 0;
            advance();
        } else if (c == '/' && pos + 1 < src.length() && src[pos + 1] == '/') {
            // Handle comments: // ...
            while (peek() != '\n' && peek() != '\0') advance();
        } else {
            break;
        }
    }
}

Token Lexer::identifier() {
    size_t start = pos - 1; // We already consumed the first char
    while (isalnum(peek()) || peek() == '_') advance();
    
    std::string text = src.substr(start, pos - start);
    
    // Check if it's a keyword
    if (keywords.count(text)) {
        return Token{keywords[text], text, line, col};
    }
    return Token{TokenType::Identifier, text, line, col};
}

Token Lexer::number() {
    size_t start = pos - 1;
    bool isFloat = false;
    
    while (isdigit(peek())) advance();
    
    if (peek() == '.' && isdigit(src[pos + 1])) {
        isFloat = true;
        advance(); // consume '.'
        while (isdigit(peek())) advance();
    }
    
    std::string text = src.substr(start, pos - start);
    return Token{isFloat ? TokenType::Float : TokenType::Integer, text, line, col};
}

Token Lexer::character() {
    size_t start = pos - 1; // We already consumed the '
    
    if (peek() == '\\') {
        advance(); // consume '\'
        char escaped = peek();
        advance(); // consume escaped char
        if (peek() != '\'') return Token{TokenType::EoF, "INVALID_CHAR", line, col};
        advance(); // consume closing '
        
        std::string text = src.substr(start, pos - start);
        return Token{TokenType::CharLit, text, line, col};
    } else {
        advance(); // consume character
        if (peek() != '\'') return Token{TokenType::EoF, "INVALID_CHAR", line, col};
        advance(); // consume closing '
        
        std::string text = src.substr(start, pos - start);
        return Token{TokenType::CharLit, text, line, col};
    }
}

Token Lexer::string() {
    size_t start = pos - 1; // We already consumed the "
    
    while (peek() != '"' && peek() != '\0') {
        if (peek() == '\\') {
            advance(); // consume '\'
            advance(); // consume escaped char
        } else {
            advance();
        }
    }
    
    if (peek() != '"') return Token{TokenType::EoF, "UNTERMINATED_STRING", line, col};
    advance(); // consume closing "
    
    std::string text = src.substr(start, pos - start);
    return Token{TokenType::StringLit, text, line, col};
}

Token Lexer::next_token() {
    skip_whitespace();
    
    if (pos >= src.length()) return Token{TokenType::EoF, "", line, col};

    char c = peek();
    advance(); // Consume character

    if (isalpha(c) || c == '_') return identifier();
    if (isdigit(c)) return number();

    switch (c) {
        case '(': return atom(TokenType::LParen);
        case ')': return atom(TokenType::RParen);
        case '{': return atom(TokenType::LBrace);
        case '}': return atom(TokenType::RBrace);
        case '[': return atom(TokenType::LBracket);
        case ']': return atom(TokenType::RBracket);
        case ';': return atom(TokenType::Semicolon);
        case ':': return atom(TokenType::Colon);
        case ',': return atom(TokenType::Comma);
        case '.': return atom(TokenType::Dot);
        case '+': return atom(TokenType::Plus);
        case '*': return atom(TokenType::Star);
        case '/': return atom(TokenType::Slash); // Added Slash
        case '<': 
            if (peek() == '=') { advance(); return Token{TokenType::LessEqual, "<=", line, col}; }
            return atom(TokenType::Less);
        case '>': 
            if (peek() == '=') { advance(); return Token{TokenType::GreaterEqual, ">=", line, col}; }
            return atom(TokenType::Greater);
        case '&':
            if (peek() == '&') { advance(); return Token{TokenType::LogicalAnd, "&&", line, col}; }
            return atom(TokenType::Ampersand);
        case '|':
            if (peek() == '|') { advance(); return Token{TokenType::LogicalOr, "||", line, col}; }
            return Token{TokenType::EoF, "UNKNOWN", line, col}; // Single | not supported yet
        case '\'': return character(); // Handle character literals
        case '"': return string(); // Handle string literals
        
        // Multi-character tokens
        case '-':
            if (peek() == '>') { advance(); return Token{TokenType::Arrow, "->", line, col}; }
            return atom(TokenType::Minus);
        case '=':
            if (peek() == '=') { advance(); return Token{TokenType::EqualEqual, "==", line, col}; }
            if (peek() == '>') { advance(); return Token{TokenType::Arrow, "=>", line, col}; }
            return atom(TokenType::Equal);
        case '!':
            if (peek() == '=') { advance(); return Token{TokenType::NotEqual, "!=", line, col}; }
            return Token{TokenType::EoF, "UNKNOWN", line, col}; // Single ! not supported yet
            
        default:
            // For now, return EOF on unknown char to avoid infinite loop, 
            // but normally you'd emit an Error token.
            return Token{TokenType::EoF, "UNKNOWN", line, col}; 
    }
}
