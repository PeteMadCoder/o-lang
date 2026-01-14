#include "CompilerDriver.h"
#include "Parser.h"
#include "Lexer.h"
#include "SymbolTable.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <filesystem>

// Define destructor here to resolve the unique_ptr<SymbolTable>
CompilerDriver::~CompilerDriver() = default;

namespace fs = std::filesystem;

CompilerDriver::CompilerDriver() {
    symbolTable = std::make_unique<SymbolTable>();
}

void CompilerDriver::symbolCollectionPhase(const std::string& filename) {
    // Process the main file to collect symbols
    processFileInternal(filename, true); // true indicates this is for symbol collection only
}

void CompilerDriver::semanticResolutionPhase() {
    // All symbols have been collected, now mark them as complete
    if (symbolTable) {
        symbolTable->finalizeAllSymbols();
    }
    // Additional semantic resolution can happen here
}

void CompilerDriver::codeGenerationPhase() {
    // Code generation happens in the existing flow after semantic resolution
    // This is handled by the existing codegen infrastructure
}

void CompilerDriver::processFileInternal(const std::string& filename, bool forSymbolCollection) {
    // 1. Resolve Filename
    std::string path = filename;
    bool found = false;

    // Helper lambda to check existence
    auto checkPath = [&](std::string p) -> std::string {
        if (fs::exists(p)) return p;
        if (fs::exists(p + ".o")) return p + ".o";
        if (fs::exists(p + ".olang")) return p + ".olang";
        return "";
    };

    // 1a. Check current directory / absolute path
    std::string res = checkPath(path);
    if (!res.empty()) {
        path = res;
        found = true;
    }

    // 1b. Check Include Paths
    if (!found) {
        for (const auto& inc : includePaths) {
            std::string tryPath = inc + "/" + filename; // Handle path separator?
            if (inc.back() != '/') tryPath = inc + "/" + filename;
            else tryPath = inc + filename;

            res = checkPath(tryPath);
            if (!res.empty()) {
                path = res;
                found = true;
                break;
            }
        }
    }

    // 1c. Legacy fallback (keep for now, or assume caller adds these to includePaths)
    if (!found) {
         std::vector<std::string> legacyPaths = {"../tests/", "o-compiler/tests/", "tests/"};
         for (const auto& legacy : legacyPaths) {
            std::string tryPath = legacy + filename;
            res = checkPath(tryPath);
            if (!res.empty()) {
                path = res;
                found = true;
                break;
            }
         }
    }

    if (!found) {
        std::cerr << "Error: Could not find file '" << filename << "'\n";
        return;
    }

    // Canonicalize path to ensure uniqueness
    try {
        path = fs::canonical(path).string();
    } catch (const fs::filesystem_error& e) {
        std::cerr << "Error resolving path for '" << filename << "': " << e.what() << "\n";
        return;
    }

    // 2. Check if already processed for this phase
    if (forSymbolCollection) {
        if (symbolCollectionProcessedFiles.count(path)) {
            return; // Already processed for symbol collection
        }
        symbolCollectionProcessedFiles.insert(path);
    } else {
        if (codeGenerationProcessedFiles.count(path)) {
            return; // Already processed for code generation
        }
        codeGenerationProcessedFiles.insert(path);
    }

    std::cout << "Importing: " << path << "\n";

    // 3. Read File
    std::ifstream file(path);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open file '" << path << "'\n";
        return;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source_code = buffer.str();

    // 4. Parse - pass the forSymbolCollection flag to the parser
    Lexer lexer(source_code);
    Parser parser(lexer, *this, path, forSymbolCollection);

    while (!parser.isEOF()) {
        if (!parser.ParseTopLevel()) {
            std::cerr << "Error: Parsing failed in '" << path << "'\n";
            // Don't abort the whole process, just stop parsing this file?
            // Or maybe we should exit(1) in Parser::LogError?
            // For now, continue to see other errors.
            return;
        }
    }
}

void CompilerDriver::processFile(const std::string& filename) {
    processFileInternal(filename, false); // Default to false for backward compatibility
}

void CompilerDriver::processFileForImport(const std::string& filename) {
    processFileInternal(filename, true); // true for symbol collection during import
}

bool CompilerDriver::hasProcessed(const std::string& filename) const {
    return symbolCollectionProcessedFiles.count(filename) || codeGenerationProcessedFiles.count(filename);
}
