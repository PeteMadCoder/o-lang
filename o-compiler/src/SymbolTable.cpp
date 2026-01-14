#include "SymbolTable.h"
#include "AST.h"

void SymbolTable::registerStructFromAST(const StructDeclAST& ast) {
    // Create a new struct symbol
    auto symbol = std::make_unique<StructSymbol>(ast.getName());
    
    // Copy fields
    for (const auto& field : ast.getFields()) {
        symbol->fields.push_back(field);
    }
    
    // Copy method signatures (not bodies)
    for (const auto& method : ast.getMethods()) {
        if (method) {
            const auto& proto = method->getPrototype();
            if (proto) {
                std::vector<std::pair<std::string, OType>> params;
                for (const auto& param : proto->getArgs()) {
                    params.push_back(param);
                }
                symbol->methods.push_back({proto->getName(), params});
            }
        }
    }
    
    // Copy constructor signatures (not bodies)
    for (const auto& constructor : ast.getConstructors()) {
        if (constructor) {
            std::vector<std::pair<std::string, OType>> params;
            for (const auto& param : constructor->getParams()) {
                params.push_back(param);
            }
            symbol->constructors.push_back(params);
        }
    }
    
    // Add to symbol table
    addStructSymbol(std::move(symbol));
}