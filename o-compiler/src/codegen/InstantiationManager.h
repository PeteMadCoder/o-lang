#pragma once

#include "../AST.h"
#include <queue>
#include <unordered_set>
#include <string>
#include <vector>

struct InstantiationRequest {
    enum Kind {
        Struct,
        Method
    } kind;

    std::string baseName;
    std::vector<OType> typeArgs;
    
    // For method instantiation
    std::string methodName;
    
    // Equality operator for checking if already instantiated
    bool operator==(const InstantiationRequest& other) const {
        return kind == other.kind && 
               baseName == other.baseName && 
               typeArgs.size() == other.typeArgs.size() &&
               methodName == other.methodName;
    }
};

// Hash function for InstantiationRequest
struct InstantiationRequestHash {
    std::size_t operator()(const InstantiationRequest& req) const {
        std::size_t h1 = std::hash<int>{}(static_cast<int>(req.kind));
        std::size_t h2 = std::hash<std::string>{}(req.baseName);
        std::size_t h3 = std::hash<std::string>{}(req.methodName);
        return h1 ^ (h2 << 1) ^ (h3 << 2);
    }
};

class InstantiationManager {
private:
    std::queue<InstantiationRequest> pending;
    std::unordered_set<InstantiationRequest, InstantiationRequestHash> instantiated;
    bool frozen = false;

public:
    bool isFrozen() const { return frozen; }
    void freeze() { frozen = true; }
    
    void enqueue(const InstantiationRequest& req);
    bool hasPending() const { return !pending.empty(); }
    InstantiationRequest dequeue();
    bool isAlreadyInstantiated(const InstantiationRequest& req) const;
    
    void markAsInstantiated(const InstantiationRequest& req);
};