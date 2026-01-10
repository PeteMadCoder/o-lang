#include "Parser.h"
#include "CompilerDriver.h"
#include "AST.h"  // Include AST.h to access the global registry functions
#include <iostream>
#include <cassert>

// Operator Precedence Table
static std::map<char, int> BinopPrecedence = {
    {'<', 10},
    {'>', 10},
    {'+', 20},
    {'-', 20},
    {'*', 40},
    {'/', 40},
    {'%', 40}
};

Parser::Parser(Lexer& lex, CompilerDriver& drv) : lexer(lex), driver(drv), currentFileDir(".") {
    getNextToken(); // Prime the pump

    // Verify initial safety state
    assert(unsafeDepth == 0);
    assert(!isInUnsafeContext());
}

Parser::Parser(Lexer& lex, CompilerDriver& drv, const std::string& currentFile) : lexer(lex), driver(drv) {
    // Extract directory from current file path
    size_t lastSlash = currentFile.find_last_of("/\\");
    if (lastSlash != std::string::npos) {
        currentFileDir = currentFile.substr(0, lastSlash);
    } else {
        currentFileDir = ".";  // Current directory if no slash found
    }

    getNextToken(); // Prime the pump

    // Verify initial safety state
    assert(unsafeDepth == 0);
    assert(!isInUnsafeContext());
}

void Parser::getNextToken() {
    curTok = lexer.next_token();
    // std::cerr << "Token: " << curTok.text << " (" << (int)curTok.type << ")\n";
}

int Parser::GetTokPrecedence() {
    // Logical operators (lowest precedence)
    if (curTok.type == TokenType::LogicalOr) return 5;
    if (curTok.type == TokenType::LogicalAnd) return 6;
    
    // Comparison operators
    if (curTok.type == TokenType::EqualEqual) return 10;
    if (curTok.type == TokenType::NotEqual) return 10;
    if (curTok.type == TokenType::Less) return 10;
    if (curTok.type == TokenType::Greater) return 10;
    if (curTok.type == TokenType::LessEqual) return 10;
    if (curTok.type == TokenType::GreaterEqual) return 10;
    
    // Arithmetic operators
    if (curTok.type == TokenType::Plus) return 20;
    if (curTok.type == TokenType::Minus) return 20;
    if (curTok.type == TokenType::Star) return 40;
    if (curTok.type == TokenType::Slash) return 40;
    if (curTok.type == TokenType::Mod) return 40;
    
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

void Parser::LogSafetyError(const char* str) {
    fprintf(stderr, "Safety Error: %s\n", str);
    exit(1); // Abort compilation immediately
}

// --- Expression Parsing ---

OType Parser::ParseType() {
    int pointerDepth = 0;
    
    // Count leading asterisks for pointer depth
    while (curTok.type == TokenType::Star) {
        pointerDepth++;
        getNextToken(); // eat '*'
    }
    
    // Parse base type
    OType baseType;
    if (curTok.type == TokenType::TypeInt) { 
        getNextToken(); 
        baseType = OType(BaseType::Int); 
    } else if (curTok.type == TokenType::TypeFloat) { 
        getNextToken(); 
        baseType = OType(BaseType::Float); 
    } else if (curTok.type == TokenType::TypeBool) { 
        getNextToken(); 
        baseType = OType(BaseType::Bool); 
    } else if (curTok.type == TokenType::TypeVoid) { 
        getNextToken(); 
        baseType = OType(BaseType::Void); 
    } else if (curTok.type == TokenType::TypeChar) { 
        getNextToken(); 
        baseType = OType(BaseType::Char); 
    } else if (curTok.type == TokenType::TypeByte) { 
        getNextToken(); 
        baseType = OType(BaseType::Byte); 
    } else if (curTok.type == TokenType::Identifier) {
        // User-defined types or Generic Parameters
        std::string typeName = curTok.text;
        getNextToken(); // eat identifier
        
        std::vector<OType> genArgs;
        if (curTok.type == TokenType::Less) {
            getNextToken(); // eat '<'
            while (curTok.type != TokenType::Greater && curTok.type != TokenType::EoF) {
                genArgs.push_back(ParseType());
                if (curTok.type == TokenType::Comma) getNextToken();
            }
            if (curTok.type == TokenType::Greater) getNextToken(); // eat '>'
        }
        
        baseType = OType(BaseType::Struct, 0, typeName, {}, genArgs);
    } else {
        baseType = OType(BaseType::Void);
    }
    
    // Apply pointer depth
    baseType.pointerDepth = pointerDepth;
    
    // Check for array syntax: Type[size][size]...
    while (curTok.type == TokenType::LBracket) {
        getNextToken(); // eat '['
        
        if (curTok.type == TokenType::Integer) {
            int arraySize = std::stoi(curTok.text);
            baseType.arraySizes.push_back(arraySize);
            getNextToken(); // eat size
        } else {
            // Empty brackets [] for unsized arrays
            baseType.arraySizes.push_back(-1); // Mark as unsized
        }
        
        if (curTok.type != TokenType::RBracket) {
            LogError("Expected ']' after array size");
            return OType(BaseType::Void);
        }
        getNextToken(); // eat ']'
    }
    
    return baseType;
}

std::unique_ptr<ExprAST> Parser::ParseNumberExpr() {
    bool isFloat = (curTok.type == TokenType::Float);
    double val = std::stod(curTok.text);
    getNextToken();
    return std::make_unique<NumberExprAST>(val, isFloat ? OType(BaseType::Float) : OType(BaseType::Int));
}

std::unique_ptr<ExprAST> Parser::ParseCharExpr() {
    std::string charText = curTok.text;
    char val;
    
    if (charText.length() == 3) { // 'a'
        val = charText[1];
    } else if (charText.length() == 4 && charText[1] == '\\') { // '\n'
        switch (charText[2]) {
            case 'n': val = '\n'; break;
            case 't': val = '\t'; break;
            case 'r': val = '\r'; break;
            case '\\': val = '\\'; break;
            case '\'': val = '\''; break;
            default: val = charText[2]; break;
        }
    } else {
        val = 0; // Error case
    }
    
    getNextToken();
    return std::make_unique<NumberExprAST>((double)val, OType(BaseType::Char));
}

std::unique_ptr<ExprAST> Parser::ParseStringExpr() {
    std::string strText = curTok.text;
    // Remove quotes: "hello" -> hello
    std::string content = strText.substr(1, strText.length() - 2);
    getNextToken();
    return std::make_unique<StringExprAST>(content);
}

std::unique_ptr<ExprAST> Parser::ParseNewExpr() {
    if (curTok.type != TokenType::New) return nullptr;
    getNextToken(); // eat 'new'
    
    // Parse Type (simplified for now: Identifier or Primitive)
    OType ElementType;
    std::string ClassName;
    std::vector<OType> GenericArgs;
    
    if (curTok.type == TokenType::Identifier) {
        ClassName = curTok.text;
        // Check if it's a struct type
        if (TypeRegistry::getInstance().hasStruct(ClassName)) {
            ElementType = OType(BaseType::Struct, 0, ClassName);
        } else {
             // Assume class for now or void
             ElementType = OType(BaseType::Void); // Placeholder
        }
        getNextToken();
        
        // Parse generic arguments: <int>
        if (curTok.type == TokenType::Less) {
            getNextToken(); // eat '<'
            while (curTok.type != TokenType::Greater && curTok.type != TokenType::EoF) {
                GenericArgs.push_back(ParseType());
                if (curTok.type == TokenType::Comma) getNextToken();
            }
            if (curTok.type == TokenType::Greater) getNextToken(); // eat '>'
        }
        
    } else if (curTok.type == TokenType::TypeInt) { ElementType = OType(BaseType::Int); getNextToken(); }
    else if (curTok.type == TokenType::TypeFloat) { ElementType = OType(BaseType::Float); getNextToken(); }
    else if (curTok.type == TokenType::TypeBool) { ElementType = OType(BaseType::Bool); getNextToken(); }
    else if (curTok.type == TokenType::TypeChar) { ElementType = OType(BaseType::Char); getNextToken(); }
    else if (curTok.type == TokenType::TypeByte) { ElementType = OType(BaseType::Byte); getNextToken(); }
    else {
        return LogError("Expected type after 'new'");
    }

    // Check for Array Allocation: new Type[Expr]
    if (curTok.type == TokenType::LBracket) {
        getNextToken(); // eat '['
        
        auto SizeExpr = ParseExpression();
        if (!SizeExpr) return nullptr;
        
        if (curTok.type != TokenType::RBracket)
            return LogError("Expected ']' after array size");
        getNextToken(); // eat ']'
        
        // Update ElementType with generic args if needed
        if (!GenericArgs.empty()) {
            ElementType = OType(BaseType::Struct, 0, ClassName, {}, GenericArgs);
        }
        
        return std::make_unique<NewArrayExprAST>(ElementType, std::move(SizeExpr));
    }
    
    // Object Instantiation: new Class(...)
    if (ClassName.empty()) return LogError("Cannot instantiate primitive type with '()'");
    
    if (curTok.type != TokenType::LParen) {
        return LogError("Expected '(' after class name");
    }
    getNextToken(); // eat '('
    
    std::vector<std::unique_ptr<ExprAST>> Args;
    while (curTok.type != TokenType::RParen && curTok.type != TokenType::EoF) {
        auto Arg = ParseExpression();
        if (!Arg) return nullptr;
        Args.push_back(std::move(Arg));
        
        if (curTok.type == TokenType::Comma) {
            getNextToken(); // eat ','
        }
    }
    
    if (curTok.type != TokenType::RParen) {
        return LogError("Expected ')' after arguments");
    }
    getNextToken(); // eat ')'
    
    return std::make_unique<NewExprAST>(ClassName, std::move(Args), GenericArgs);
}

std::unique_ptr<ExprAST> Parser::ParseArrayTypeExpr() {
    // This handles parsing array types as expressions: int[10]
    // We need to backtrack and reparse as array type
    
    // For now, this is a placeholder - array types in expressions
    // would need more sophisticated parsing
    return nullptr;
}

std::unique_ptr<ExprAST> Parser::ParseArrayInitExpr() {
    getNextToken(); // eat '{'
    
    std::vector<std::unique_ptr<ExprAST>> Elements;
    
    // Handle empty array: {}
    if (curTok.type == TokenType::RBrace) {
        getNextToken(); // eat '}'
        return std::make_unique<ArrayInitExprAST>(std::move(Elements), OType(BaseType::Void));
    }
    
    // Parse first element
    auto FirstElement = ParseExpression();
    if (!FirstElement) return nullptr;
    Elements.push_back(std::move(FirstElement));
    
    // Parse remaining elements
    while (curTok.type == TokenType::Comma) {
        getNextToken(); // eat ','
        auto Element = ParseExpression();
        if (!Element) return nullptr;
        Elements.push_back(std::move(Element));
    }
    
    if (curTok.type != TokenType::RBrace)
        return LogError("Expected '}' after array elements");
    getNextToken(); // eat '}'
    
    // Type will be inferred from first element during codegen
    return std::make_unique<ArrayInitExprAST>(std::move(Elements), OType(BaseType::Void));
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

std::unique_ptr<ExprAST> Parser::ParseMatchExpr() {
    getNextToken(); // eat 'match'
    
    if (curTok.type != TokenType::LParen) 
        return LogError("Expected '(' after match");
    getNextToken(); // eat '('
    
    auto Cond = ParseExpression();
    if (!Cond) return nullptr;
    
    if (curTok.type != TokenType::RParen)
        return LogError("Expected ')' after match condition");
    getNextToken(); // eat ')'
    
    if (curTok.type != TokenType::LBrace)
        return LogError("Expected '{' to start match cases");
    getNextToken(); // eat '{'
    
    std::vector<MatchCase> Cases;
    
    // Parse cases until '}'
    while (curTok.type != TokenType::RBrace && curTok.type != TokenType::EoF) {
        std::unique_ptr<ExprAST> Pattern = nullptr;
        
        // Parse Pattern: Literal or '_'
        if (curTok.type == TokenType::Identifier && curTok.text == "_") {
            // Wildcard
            Pattern = nullptr;
            getNextToken(); // eat '_'
        } else if (curTok.type == TokenType::Integer || curTok.type == TokenType::Float ||
                   curTok.type == TokenType::CharLit || curTok.type == TokenType::StringLit ||
                   curTok.type == TokenType::True || curTok.type == TokenType::False) {
            // Literal - assume ParsePrimary handles literal tokens correctly without consuming extra if we peek?
            // ParsePrimary consumes the token.
            Pattern = ParsePrimary(); 
            if (!Pattern) return nullptr;
        } else {
            return LogError("Expected literal or '_' as match pattern");
        }
        
        // Expect '=>'
        if (curTok.type != TokenType::Arrow) {
            return LogError("Expected '=>' after pattern");
        }
        getNextToken(); // eat '=>'
        
        // Parse Body (Statement or Block)
        auto Body = ParseStatement();
        if (!Body) return nullptr;
        
        Cases.emplace_back(std::move(Pattern), std::move(Body));
    }
    
    if (curTok.type != TokenType::RBrace)
        return LogError("Expected '}' to end match cases");
    getNextToken(); // eat '}'
    
    return std::make_unique<MatchExprAST>(std::move(Cond), std::move(Cases));
}

std::unique_ptr<ExprAST> Parser::ParsePrimary() {
    switch (curTok.type) {
        case TokenType::True:       getNextToken(); return std::make_unique<BoolExprAST>(true);
        case TokenType::False:      getNextToken(); return std::make_unique<BoolExprAST>(false);
        case TokenType::If:         return ParseIfExpr();
        case TokenType::Match:      return ParseMatchExpr(); // Handle match
        case TokenType::Unsafe:     return ParseUnsafeBlock();
        case TokenType::New:        return ParseNewExpr();
        case TokenType::Identifier: return ParseIdentifierExpr();
        case TokenType::Integer:    return ParseNumberExpr();
        case TokenType::Float:      return ParseNumberExpr();
        case TokenType::CharLit:    return ParseCharExpr();
        case TokenType::StringLit:  return ParseStringExpr();
        case TokenType::LParen:     return ParseParenExpr();
        case TokenType::LBrace:     return ParseArrayInitExpr();
        default: return LogError("unknown token when expecting an expression");
    }
}

std::unique_ptr<ExprAST> Parser::ParsePostfix() {
    auto Expr = ParsePrimary();
    if (!Expr) return nullptr;
    
    while (curTok.type == TokenType::Dot || curTok.type == TokenType::LBracket) {
        if (curTok.type == TokenType::Dot) {
            getNextToken(); // eat '.'
            
            if (curTok.type != TokenType::Identifier) {
                return LogError("Expected field name after '.'");
            }
            
            std::string MemberName = curTok.text;
            getNextToken(); // eat member name
            
            // Check for Method Call: obj.method(...)
            if (curTok.type == TokenType::LParen) {
                getNextToken(); // eat '('
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
                getNextToken(); // eat ')'
                
                Expr = std::make_unique<MethodCallExprAST>(std::move(Expr), MemberName, std::move(Args));
            } else {
                // Field Access: obj.field
                Expr = std::make_unique<MemberAccessAST>(std::move(Expr), MemberName);
            }
        } else if (curTok.type == TokenType::LBracket) {
            getNextToken(); // eat '['
            
            auto Index = ParseExpression();
            if (!Index) {
                return LogError("Expected index expression");
            }
            
            if (curTok.type != TokenType::RBracket) {
                return LogError("Expected ']' after index");
            }
            getNextToken(); // eat ']'
            
            Expr = std::make_unique<IndexExprAST>(std::move(Expr), std::move(Index));
        }
    }
    
    return Expr;
}

std::unique_ptr<ExprAST> Parser::ParseUnary() {
    // Handle address-of operator
    if (curTok.type == TokenType::Ampersand) {
        getNextToken(); // eat '&'
        auto Operand = ParseUnary(); // Allow chaining: &(&x)
        if (!Operand) return nullptr;
        return std::make_unique<AddressOfExprAST>(std::move(Operand));
    }
    
    // Handle negation operator (-x)
    if (curTok.type == TokenType::Minus) {
        getNextToken(); // eat '-'
        auto Operand = ParseUnary();
        if (!Operand) return nullptr;
        return std::make_unique<NegateExprAST>(std::move(Operand));
    }
    
    // Handle dereference operator (unsafe operation)
    if (curTok.type == TokenType::Star) {
        // Safety check: dereference only allowed in unsafe blocks
        if (unsafeDepth == 0) {
            LogSafetyError("Dereference is only allowed inside unsafe blocks");
        }
        
        getNextToken(); // eat '*'
        auto Operand = ParseUnary(); // Allow chaining: *(*ptr)
        if (!Operand) return nullptr;
        return std::make_unique<DerefExprAST>(std::move(Operand));
    }
    
    // Otherwise, parse postfix expression
    return ParsePostfix();
}

std::unique_ptr<ExprAST> Parser::ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = GetTokPrecedence();

        if (TokPrec < ExprPrec)
            return LHS;

        char BinOp = curTok.text[0]; 
        std::string BinOpStr = curTok.text; // Store full operator string
        getNextToken(); // eat binop

        auto RHS = ParseUnary();
        if (!RHS) return nullptr;

        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS) return nullptr;
        }

        LHS = std::make_unique<BinaryExprAST>(BinOpStr, std::move(LHS), std::move(RHS));
    }
}

std::unique_ptr<ExprAST> Parser::ParseExpression() {
    auto LHS = ParseUnary();
    if (!LHS) return nullptr;
    
    // Check for assignment
    if (curTok.type == TokenType::Equal) {
        getNextToken(); // eat '='
        auto RHS = ParseExpression();
        if (!RHS) return nullptr;
        return std::make_unique<AssignmentExprAST>(std::move(LHS), std::move(RHS));
    }
    
    return ParseBinOpRHS(0, std::move(LHS));
}

std::unique_ptr<ExprAST> Parser::ParseVarDecl() {
    TokenType declType = curTok.type; // Store whether it's var or let
    bool isConst = (declType == TokenType::Let);
    getNextToken(); // eat 'var' or 'let'

    if (curTok.type != TokenType::Identifier)
        return LogError("Expected identifier after var/let");

    std::string Name = curTok.text;
    getNextToken(); // eat identifier

    // Check for optional type annotation: var name: type = value
    OType ExplicitType;
    bool HasExplicitType = false;
    if (curTok.type == TokenType::Colon) {
        getNextToken(); // eat ':'
        ExplicitType = ParseType();
        HasExplicitType = true;
    }

    std::unique_ptr<ExprAST> Init = nullptr;
    if (curTok.type == TokenType::Equal) {
        getNextToken(); // eat '='
        Init = ParseExpression();
        if (!Init) return nullptr;
    } else if (HasExplicitType && curTok.type == TokenType::Semicolon) {
        // Declaration without initialization (e.g. var x: int;)
        // We will handle zero-init in codegen
    } else if (isConst) {
        // Constants must be initialized
        return LogError("Constants must be initialized");
    } else {
        return LogError("Expected '=' in variable declaration");
    }

    if (curTok.type != TokenType::Semicolon)
        return LogError("Expected ';' after variable declaration");
    getNextToken(); // eat ';'

    return std::make_unique<VarDeclExprAST>(Name, std::move(Init), ExplicitType, HasExplicitType, isConst);
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

std::unique_ptr<ExprAST> Parser::ParseForStmt() {
    getNextToken(); // eat 'for'
    
    if (curTok.type != TokenType::LParen)
        return LogError("Expected '(' after for");
    getNextToken(); // eat '('
    
    // Parse Init
    std::unique_ptr<ExprAST> Init = nullptr;
    if (curTok.type == TokenType::Semicolon) {
        getNextToken(); // eat ';'
    } else {
        Init = ParseStatement(); // Handles VarDecl or ExpressionStmt
        if (!Init) return nullptr;
    }
    
    // Parse Cond
    std::unique_ptr<ExprAST> Cond = nullptr;
    if (curTok.type != TokenType::Semicolon) {
        Cond = ParseExpression();
        if (!Cond) return nullptr;
    } else {
        Cond = std::make_unique<BoolExprAST>(true); // Infinite loop
    }
    
    if (curTok.type != TokenType::Semicolon)
        return LogError("Expected ';' after for condition");
    getNextToken(); // eat ';'
    
    // Parse Step
    std::unique_ptr<ExprAST> Step = nullptr;
    if (curTok.type != TokenType::RParen) {
        Step = ParseExpression();
        if (!Step) return nullptr;
    }
    
    if (curTok.type != TokenType::RParen)
        return LogError("Expected ')' after for step");
    getNextToken(); // eat ')'
    
    if (curTok.type != TokenType::LBrace)
        return LogError("Expected '{' for loop body");
        
    auto Body = ParseBlock();
    if (!Body) return nullptr;
    
    return std::make_unique<ForExprAST>(std::move(Init), std::move(Cond), std::move(Step), std::move(Body));
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
    if (curTok.type == TokenType::For) {
        return ParseForStmt();
    }
    if (curTok.type == TokenType::Var || curTok.type == TokenType::Let) {
        return ParseVarDecl();
    }
    
    // Manual Memory Management: delete expr;
    if (curTok.type == TokenType::Delete) {
        if (unsafeDepth == 0) {
            LogSafetyError("'delete' is only allowed inside unsafe blocks");
        }
        getNextToken(); // eat 'delete'
        
        auto Expr = ParseExpression();
        if (!Expr) return nullptr;
        
        if (curTok.type != TokenType::Semicolon) {
            return LogError("Expected ';' after delete statement");
        }
        getNextToken(); // eat ';'
        
        return std::make_unique<DeleteExprAST>(std::move(Expr));
    }
    
    // Block statement
    if (curTok.type == TokenType::LBrace) {
        return ParseBlock();
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
    while (curTok.type != TokenType::RParen && curTok.type != TokenType::EoF) {
        // Parse "Name: Type" (modern syntax similar to var declarations)
        if (curTok.type != TokenType::Identifier) return LogErrorP("Expected parameter name");
        std::string ArgName = curTok.text;
        getNextToken(); // eat Name

        if (curTok.type != TokenType::Colon) return LogErrorP("Expected ':' after parameter name");
        getNextToken(); // eat ':'

        OType ArgType = ParseType();

        Args.push_back({ArgName, ArgType});

        if (curTok.type == TokenType::Comma) {
            getNextToken(); // eat ','
        } else if (curTok.type != TokenType::RParen) {
            return LogErrorP("Expected ',' or ')' in parameter list");
        }
    }

    if (curTok.type != TokenType::RParen) return LogErrorP("Expected ')'");
    getNextToken();

    // Parse Return Type
    OType RetType = OType(BaseType::Void);
    if (curTok.type == TokenType::Arrow) {
        getNextToken(); // eat ->
        RetType = ParseType(); // Parse "int" or "float"
    }

    return std::make_unique<PrototypeAST>(FnName, std::move(Args), RetType);
}

std::unique_ptr<FunctionAST> Parser::ParseDefinition() {
    auto Proto = ParsePrototype();
    if (!Proto) return nullptr;

    // Check if this is an extern declaration (semicolon after prototype)
    bool isExtern = (curTok.type == TokenType::Semicolon);

    // --- CRITICAL FIX ---
    // Explicitly call the global registration function.
    // Do this for EVERY function, especially those with no body (externs).
    auto ProtoForAST = Proto->clone();  // Clone the prototype to preserve it for the AST
    RegisterFunctionProto(std::move(Proto));  // Register the original in the global registry
    // --------------------

    if (isExtern) {
        getNextToken(); // eat ;
        return std::make_unique<FunctionAST>(std::move(ProtoForAST), nullptr);
    }

    if (curTok.type != TokenType::LBrace) return nullptr;

    auto Body = ParseBlock();
    if (Body) {
        return std::make_unique<FunctionAST>(std::move(ProtoForAST), std::move(Body));
    }
    return nullptr;
}

std::unique_ptr<StructDeclAST> Parser::ParseStruct() {
    if (curTok.type != TokenType::Struct) return nullptr;
    getNextToken(); // eat 'struct'
    
    if (curTok.type != TokenType::Identifier) {
        LogError("Expected struct name");
        return nullptr;
    }
    
    std::string StructName = curTok.text;
    getNextToken(); // eat struct name
    
    // Parse optional generic parameters
    std::vector<std::string> GenericParams = ParseGenericParams();
    
    if (curTok.type != TokenType::LBrace) {
        LogError("Expected '{'");
        return nullptr;
    }
    getNextToken(); // eat '{'
    
    std::vector<std::pair<std::string, OType>> Fields;
    std::vector<std::unique_ptr<FunctionAST>> Methods;
    std::vector<std::unique_ptr<ConstructorAST>> Constructors;
    
    while (curTok.type != TokenType::RBrace && curTok.type != TokenType::EoF) {
        if (curTok.type == TokenType::Fn) {
            // Parse method
            auto method = ParseDefinition();
            if (!method) {
                LogError("Failed to parse struct method");
                return nullptr;
            }
            Methods.push_back(std::move(method));
        } else if (curTok.type == TokenType::New) {
            // Parse constructor
            std::cerr << "Parsing constructor for " << StructName << "\n";
            auto constructor = ParseConstructor();
            if (!constructor) {
                LogError("Failed to parse struct constructor");
                return nullptr;
            }
            Constructors.push_back(std::move(constructor));
        } else {
            // Parse field: Name: Type;
            if (curTok.type != TokenType::Identifier) {
                LogError("Expected field name");
                return nullptr;
            }
            std::string fieldName = curTok.text;
            getNextToken(); // eat name

            if (curTok.type != TokenType::Colon) {
                LogError("Expected ':' after field name");
                return nullptr;
            }
            getNextToken(); // eat ':'

            OType fieldType = ParseType();

            if (curTok.type != TokenType::Semicolon) {
                LogError("Expected ';' after field");
                return nullptr;
            }
            getNextToken(); // eat ';'

            Fields.push_back({fieldName, fieldType});
        }
    }
    
    if (curTok.type != TokenType::RBrace) {
        LogError("Expected '}'");
        return nullptr;
    }
    getNextToken(); // eat '}'
    
    return std::make_unique<StructDeclAST>(StructName, std::move(GenericParams), std::move(Fields), std::move(Methods), std::move(Constructors));
}

std::unique_ptr<ClassDeclAST> Parser::ParseClass() {
    bool isOpen = false;
    
    // Check for optional 'open' modifier
    if (curTok.type == TokenType::Open) {
        isOpen = true;
        getNextToken(); // eat 'open'
    }
    
    if (curTok.type != TokenType::Class) return nullptr;
    getNextToken(); // eat 'class'
    
    if (curTok.type != TokenType::Identifier) {
        LogError("Expected class name");
        return nullptr;
    }
    
    std::string ClassName = curTok.text;
    getNextToken(); // eat class name
    
    // Parse optional inheritance: : ParentName
    std::string ParentName = "";
    if (curTok.type == TokenType::Colon) {
        getNextToken(); // eat ':'
        
        if (curTok.type != TokenType::Identifier) {
            LogError("Expected parent class name after ':'");
            return nullptr;
        }
        
        ParentName = curTok.text;
        getNextToken(); // eat parent name
    }
    
    if (curTok.type != TokenType::LBrace) {
        LogError("Expected '{'");
        return nullptr;
    }
    getNextToken(); // eat '{'
    
    std::vector<std::pair<std::string, OType>> Fields;
    std::vector<std::unique_ptr<FunctionAST>> Methods;
    std::vector<std::unique_ptr<ConstructorAST>> Constructors;
    std::vector<std::string> VirtualMethods;
    
    while (curTok.type != TokenType::RBrace && curTok.type != TokenType::EoF) {
        if (curTok.type == TokenType::Virtual) {
            // Parse virtual method
            getNextToken(); // eat 'virtual'
            
            if (curTok.type != TokenType::Fn) {
                LogError("Expected 'fn' after 'virtual'");
                return nullptr;
            }
            
            auto method = ParseDefinition();
            if (!method) {
                LogError("Failed to parse virtual method");
                return nullptr;
            }
            
            // Track virtual method name
            VirtualMethods.push_back(method->getPrototype()->getName());
            Methods.push_back(std::move(method));
            
        } else if (curTok.type == TokenType::Override) {
            getNextToken(); // eat 'override'
            
            if (curTok.type != TokenType::Fn) {
                LogError("Expected 'fn' after 'override'");
                return nullptr;
            }
            
            auto method = ParseDefinition();
            if (!method) {
                LogError("Failed to parse override method");
                return nullptr;
            }
            Methods.push_back(std::move(method));
            
        } else if (curTok.type == TokenType::Fn) {
            auto method = ParseDefinition();
            if (!method) {
                LogError("Failed to parse class method");
                return nullptr;
            }
            Methods.push_back(std::move(method));
        } else if (curTok.type == TokenType::New) {
            auto constructor = ParseConstructor();
            if (!constructor) {
                LogError("Failed to parse class constructor");
                return nullptr;
            }
            Constructors.push_back(std::move(constructor));
        } else {
            // Parse field: Name: Type;
            if (curTok.type != TokenType::Identifier) {
                LogError("Expected field name");
                return nullptr;
            }
            std::string fieldName = curTok.text;
            getNextToken(); // eat name

            if (curTok.type != TokenType::Colon) {
                LogError("Expected ':' after field name");
                return nullptr;
            }
            getNextToken(); // eat ':'

            OType fieldType = ParseType();

            if (curTok.type != TokenType::Semicolon) {
                LogError("Expected ';' after field");
                return nullptr;
            }
            getNextToken(); // eat ';'

            Fields.push_back({fieldName, fieldType});
        }
    }
    
    if (curTok.type != TokenType::RBrace) {
        LogError("Expected '}'");
        return nullptr;
    }
    getNextToken(); // eat '}'
    
    return std::make_unique<ClassDeclAST>(ClassName, ParentName, isOpen, std::move(Fields), std::move(Methods), std::move(Constructors), std::move(VirtualMethods));
}

bool Parser::ParseTopLevel() {
    if (curTok.type == TokenType::Import) {
        return ParseImport();
    } else if (curTok.type == TokenType::Struct) {
        auto structAST = ParseStruct();
        if (structAST) {
            structAST->codegen();
            return true;
        }
        return false;
    } else if (curTok.type == TokenType::Class || curTok.type == TokenType::Open) {
        auto classAST = ParseClass();
        if (classAST) {
            classAST->codegen();
            return true;
        }
        return false;
    } else if (curTok.type == TokenType::Fn) {
        auto funcAST = ParseDefinition();
        if (funcAST) {
            auto *IR = funcAST->codegen();
            return IR != nullptr;
        }
        return false;
    }
    return false;
}

std::vector<std::string> Parser::ParseIdentifierList() {
    std::vector<std::string> identifiers;
    
    if (curTok.type != TokenType::Identifier) return identifiers;
    
    identifiers.push_back(curTok.text);
    getNextToken(); // eat first identifier
    
    while (curTok.type == TokenType::Comma) {
        getNextToken(); // eat ','
        
        if (curTok.type != TokenType::Identifier) {
            LogError("Expected identifier after ','");
            return identifiers;
        }
        
        identifiers.push_back(curTok.text);
        getNextToken(); // eat identifier
    }
    
    return identifiers;
}

std::unique_ptr<ConstructorAST> Parser::ParseConstructor() {
    if (curTok.type != TokenType::New) return nullptr;
    getNextToken(); // eat 'new'
    
    if (curTok.type != TokenType::LParen) {
        LogError("Expected '(' after 'new'");
        return nullptr;
    }
    getNextToken(); // eat '('
    
    std::vector<std::pair<std::string, OType>> Params;

    while (curTok.type != TokenType::RParen && curTok.type != TokenType::EoF) {
        // Parse parameter: Name: Type (modern syntax)
        if (curTok.type != TokenType::Identifier) {
            LogError("Expected parameter name");
            return nullptr;
        }

        std::string ParamName = curTok.text;
        getNextToken(); // eat param name

        if (curTok.type != TokenType::Colon) {
            LogError("Expected ':' after parameter name");
            return nullptr;
        }
        getNextToken(); // eat ':'

        OType ParamType = ParseType();

        Params.push_back({ParamName, ParamType});

        if (curTok.type == TokenType::Comma) {
            getNextToken(); // eat ','
        } else if (curTok.type != TokenType::RParen) {
            LogError("Expected ',' or ')' in parameter list");
            return nullptr;
        }
    }
    
    if (curTok.type != TokenType::RParen) {
        LogError("Expected ')'");
        return nullptr;
    }
    getNextToken(); // eat ')'
    
    // Parse body
    auto Body = ParseBlock();
    if (!Body) {
        LogError("Expected constructor body");
        return nullptr;
    }
    
    return std::make_unique<ConstructorAST>(std::move(Params), std::move(Body));
}

std::vector<std::string> Parser::ParseGenericParams() {
    std::vector<std::string> genericParams;
    
    if (curTok.type != TokenType::Less) return genericParams;
    getNextToken(); // eat '<'
    
    while (curTok.type != TokenType::Greater && curTok.type != TokenType::EoF) {
        if (curTok.type != TokenType::Identifier) {
            LogError("Expected generic parameter name");
            return genericParams;
        }
        
        genericParams.push_back(curTok.text);
        getNextToken(); // eat identifier
        
        if (curTok.type == TokenType::Comma) {
            getNextToken(); // eat ','
        }
    }
    
    if (curTok.type != TokenType::Greater) {
        LogError("Expected '>' after generic parameters");
        return genericParams;
    }
    getNextToken(); // eat '>'
    
    return genericParams;
}

bool Parser::ParseImport() {
    if (curTok.type != TokenType::Import) return false;
    getNextToken(); // eat 'import'

    if (curTok.type != TokenType::StringLit) {
        LogError("Expected string literal after 'import'");
        return false;
    }

    std::string filename = curTok.text.substr(1, curTok.text.length() - 2); // Remove quotes
    getNextToken(); // eat string

    if (curTok.type != TokenType::Semicolon) {
        LogError("Expected ';' after import");
        return false;
    }
    getNextToken(); // eat ';'

    // Resolve relative path based on current file's directory
    // Only do this if the imported filename is a simple filename (no directory separators)
    // and the current file is in a subdirectory
    std::string resolvedFilename = filename;
    if (currentFileDir != "." && filename.find('/') == std::string::npos && filename.find('\\') == std::string::npos) {
        // If the imported filename has no directory separators and the current file is in a subdirectory,
        // resolve it relative to the current file's directory
        resolvedFilename = currentFileDir + "/" + filename;
    }

    driver.processFile(resolvedFilename);
    return true;
}

std::unique_ptr<ExprAST> Parser::ParseUnsafeBlock() {
    if (curTok.type != TokenType::Unsafe) return nullptr;
    getNextToken(); // eat 'unsafe'
    
    // Enter unsafe context
    enterUnsafe();
    
    // Parse the block
    auto Block = ParseBlock();
    
    // Exit unsafe context immediately after
    exitUnsafe();
    
    return Block; // Return the block AST - no special unsafe AST needed
}

bool Parser::isEOF() { return curTok.type == TokenType::EoF; }