#pragma once
#include <string>
#include <vector>
#include <unordered_set>
#include <memory>

// Forward declarations
class Parser;
class SymbolTable;

class CompilerDriver {
private:
    std::vector<std::string> includePaths;
    std::unordered_set<std::string> processedFiles;
    std::unique_ptr<SymbolTable> symbolTable;

public:
    CompilerDriver();
    ~CompilerDriver();  // Declare destructor to handle unique_ptr<SymbolTable>
    void processFile(const std::string& filename);
    bool hasProcessed(const std::string& filename) const;
    void addIncludePath(const std::string& path) { includePaths.push_back(path); }

    SymbolTable* getSymbolTable() { return symbolTable.get(); }

    // Three-phase compilation methods
    void symbolCollectionPhase(const std::string& filename);
    void semanticResolutionPhase();
    void codeGenerationPhase();

    // Public method for internal file processing during imports
    void processFileForImport(const std::string& filename);

    // Public method to clear processed files to allow re-processing
    void clearProcessedFiles() { processedFiles.clear(); }

private:
    void processFileInternal(const std::string& filename, bool forSymbolCollection);
};
