#pragma once

#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include "AST.h"

// Symbol representing a struct declaration (for import phase)
struct StructSymbol {
    std::string name;
    std::vector<std::pair<std::string, OType>> fields;  // field name, type
    std::vector<std::pair<std::string, std::vector<std::pair<std::string, OType>>>> methods;  // method name, params
    std::vector<std::vector<std::pair<std::string, OType>>> constructors;  // list of constructor params
    bool isComplete;  // false during import, true after all imports complete
    
    StructSymbol(const std::string& n) : name(n), isComplete(false) {}
};

// Symbol table to hold all declared symbols
class SymbolTable {
private:
    std::unordered_map<std::string, std::unique_ptr<StructSymbol>> structSymbols;
    
public:
    void addStructSymbol(std::unique_ptr<StructSymbol> symbol) {
        structSymbols[symbol->name] = std::move(symbol);
    }
    
    StructSymbol* getStructSymbol(const std::string& name) {
        auto it = structSymbols.find(name);
        return (it != structSymbols.end()) ? it->second.get() : nullptr;
    }
    
    bool hasStructSymbol(const std::string& name) const {
        return structSymbols.find(name) != structSymbols.end();
    }
    
    void finalizeAllSymbols() {
        for (auto& pair : structSymbols) {
            pair.second->isComplete = true;
        }
    }
    
    const std::unordered_map<std::string, std::unique_ptr<StructSymbol>>& getAllStructs() const {
        return structSymbols;
    }

    // Helper method to create and register a struct symbol from a StructDeclAST
    void registerStructFromAST(const StructDeclAST& ast);
};