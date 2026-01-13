#include "InstantiationManager.h"

void InstantiationManager::enqueue(const InstantiationRequest& req) {
    if (frozen) {
        fprintf(stderr, "Error: Attempting to enqueue instantiation after freeze!\n");
        return;
    }
    
    // Only add if not already instantiated
    if (!isAlreadyInstantiated(req)) {
        pending.push(req);
    }
}

InstantiationRequest InstantiationManager::dequeue() {
    if (pending.empty()) {
        throw std::runtime_error("Attempted to dequeue from empty instantiation queue");
    }
    
    InstantiationRequest req = pending.front();
    pending.pop();
    return req;
}

bool InstantiationManager::isAlreadyInstantiated(const InstantiationRequest& req) const {
    return instantiated.find(req) != instantiated.end();
}

void InstantiationManager::markAsInstantiated(const InstantiationRequest& req) {
    instantiated.insert(req);
}