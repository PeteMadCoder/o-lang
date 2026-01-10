#include "CompilerDriver.h"
#include "Parser.h"
#include "Lexer.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <filesystem>

namespace fs = std::filesystem;

void CompilerDriver::processFile(const std::string& filename) {
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

    // 2. Check if already processed
    if (processedFiles.count(path)) {
        return; // Already imported
    }
    processedFiles.insert(path);

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

    // 4. Parse
    Lexer lexer(source_code);
    Parser parser(lexer, *this, path);

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

bool CompilerDriver::hasProcessed(const std::string& filename) const {
    return processedFiles.count(filename);
}
