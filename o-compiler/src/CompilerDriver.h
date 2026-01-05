#pragma once
#include <string>
#include <set>
#include <vector>

class CompilerDriver {
    std::set<std::string> processedFiles;
    std::vector<std::string> includePaths;

public:
    void processFile(const std::string& filename);
    bool hasProcessed(const std::string& filename) const;
    void addIncludePath(const std::string& path) { includePaths.push_back(path); }
};
