#include "SemanticWalker.h"
#include "UtilityCodeGen.h"
#include "InstantiationManager.h"

void SemanticWalker::walk(ExprAST &E) {
    // Dispatch to the appropriate expression walker method based on dynamic type
    if (dynamic_cast<BoolExprAST*>(&E)) {
        walk(static_cast<BoolExprAST&>(E));
    } else if (dynamic_cast<StringExprAST*>(&E)) {
        walk(static_cast<StringExprAST&>(E));
    } else if (dynamic_cast<CastExprAST*>(&E)) {
        walk(static_cast<CastExprAST&>(E));
    } else if (dynamic_cast<NumberExprAST*>(&E)) {
        walk(static_cast<NumberExprAST&>(E));
    } else if (dynamic_cast<VariableExprAST*>(&E)) {
        walk(static_cast<VariableExprAST&>(E));
    } else if (dynamic_cast<VarDeclExprAST*>(&E)) {
        walk(static_cast<VarDeclExprAST&>(E));
    } else if (dynamic_cast<AssignmentExprAST*>(&E)) {
        walk(static_cast<AssignmentExprAST&>(E));
    } else if (dynamic_cast<ReturnExprAST*>(&E)) {
        walk(static_cast<ReturnExprAST&>(E));
    } else if (dynamic_cast<DeleteExprAST*>(&E)) {
        walk(static_cast<DeleteExprAST&>(E));
    } else if (dynamic_cast<NegateExprAST*>(&E)) {
        walk(static_cast<NegateExprAST&>(E));
    } else if (dynamic_cast<NotExprAST*>(&E)) {
        walk(static_cast<NotExprAST&>(E));
    } else if (dynamic_cast<BinaryExprAST*>(&E)) {
        walk(static_cast<BinaryExprAST&>(E));
    } else if (dynamic_cast<CallExprAST*>(&E)) {
        walk(static_cast<CallExprAST&>(E));
    } else if (dynamic_cast<MethodCallExprAST*>(&E)) {
        walk(static_cast<MethodCallExprAST&>(E));
    } else if (dynamic_cast<BlockExprAST*>(&E)) {
        walk(static_cast<BlockExprAST&>(E));
    } else if (dynamic_cast<IfExprAST*>(&E)) {
        walk(static_cast<IfExprAST&>(E));
    } else if (dynamic_cast<WhileExprAST*>(&E)) {
        walk(static_cast<WhileExprAST&>(E));
    } else if (dynamic_cast<ForExprAST*>(&E)) {
        walk(static_cast<ForExprAST&>(E));
    } else if (dynamic_cast<AddressOfExprAST*>(&E)) {
        walk(static_cast<AddressOfExprAST&>(E));
    } else if (dynamic_cast<DerefExprAST*>(&E)) {
        walk(static_cast<DerefExprAST&>(E));
    } else if (dynamic_cast<NewExprAST*>(&E)) {
        walk(static_cast<NewExprAST&>(E));
    } else if (dynamic_cast<NewArrayExprAST*>(&E)) {
        walk(static_cast<NewArrayExprAST&>(E));
    } else if (dynamic_cast<MatchExprAST*>(&E)) {
        walk(static_cast<MatchExprAST&>(E));
    } else if (dynamic_cast<IndexExprAST*>(&E)) {
        walk(static_cast<IndexExprAST&>(E));
    } else if (dynamic_cast<MemberAccessAST*>(&E)) {
        walk(static_cast<MemberAccessAST&>(E));
    } else if (dynamic_cast<ArrayInitExprAST*>(&E)) {
        walk(static_cast<ArrayInitExprAST&>(E));
    } else if (dynamic_cast<ArrayLiteralExprAST*>(&E)) {
        walk(static_cast<ArrayLiteralExprAST&>(E));
    } else {
        codeGen.logError("Unknown expression type in semantic walker");
    }
}

void SemanticWalker::walk(BoolExprAST &E) {
    // Boolean expressions don't require semantic discovery
}

void SemanticWalker::walk(StringExprAST &E) {
    // String expressions don't require semantic discovery
}

void SemanticWalker::walk(CastExprAST &E) {
    // Cast expressions might involve type conversions that need discovery
    if (E.getOperand()) {
        walk(*E.getOperand());
    }
}

void SemanticWalker::walk(NumberExprAST &E) {
    // Number expressions don't require semantic discovery
}

void SemanticWalker::walk(VariableExprAST &E) {
    // Variable expressions don't require semantic discovery
}

void SemanticWalker::walk(VarDeclExprAST &E) {
    // Variable declarations might involve generic types
    if (E.getInit()) {
        walk(*E.getInit());
    }
}

void SemanticWalker::walk(AssignmentExprAST &E) {
    if (E.getLHS()) {
        walk(*E.getLHS());
    }
    if (E.getRHS()) {
        walk(*E.getRHS());
    }
}

void SemanticWalker::walk(ReturnExprAST &E) {
    if (E.getRetVal()) {
        walk(*E.getRetVal());
    }
}

void SemanticWalker::walk(DeleteExprAST &E) {
    if (E.getOperand()) {
        walk(*E.getOperand());
    }
}

void SemanticWalker::walk(NegateExprAST &E) {
    if (E.getOperand()) {
        walk(*E.getOperand());
    }
}

void SemanticWalker::walk(BinaryExprAST &E) {
    if (E.getLHS()) {
        walk(*E.getLHS());
    }
    if (E.getRHS()) {
        walk(*E.getRHS());
    }
}

void SemanticWalker::walk(NotExprAST &E) {
    if (E.getOperand()) {
        walk(*E.getOperand());
    }
}

void SemanticWalker::walk(CallExprAST &E) {
    // Function calls might require discovering function implementations
    for (auto& arg : E.getArgs()) {
        if (arg) {
            walk(*arg);
        }
    }
}

void SemanticWalker::walk(MethodCallExprAST &E) {
    // Method calls require discovering the method implementation
    if (E.getObject()) {
        walk(*E.getObject());
    }
    
    for (auto& arg : E.getArgs()) {
        if (arg) {
            walk(*arg);
        }
    }
    
    // Check if this is a call to a generic method
    OType objType = E.getObject()->getOType();
    if (objType.base == BaseType::Struct && !objType.genericArgs.empty()) {
        // This is a call on a generic instance, enqueue instantiation
        enqueueStructInstantiation(objType.structName, objType.genericArgs);
    }
}

void SemanticWalker::walk(BlockExprAST &E) {
    // BlockExprAST stores body as a vector of unique_ptr<ExprAST>
    for (auto& expr : E.getExpressions()) {
        if (expr) {
            walk(*expr);
        }
    }
}

void SemanticWalker::walk(IfExprAST &E) {
    if (E.getCond()) {
        walk(*E.getCond());
    }
    if (E.getThen()) {
        walk(*E.getThen());
    }
    if (E.getElse()) {
        walk(*E.getElse());
    }
}

void SemanticWalker::walk(WhileExprAST &E) {
    if (E.getCond()) {
        walk(*E.getCond());
    }
    if (E.getBody()) {
        walk(*E.getBody());
    }
}

void SemanticWalker::walk(ForExprAST &E) {
    if (E.getInit()) {
        walk(*E.getInit());
    }
    if (E.getCond()) {
        walk(*E.getCond());
    }
    if (E.getStep()) {  // Changed from getIncr to getStep
        walk(*E.getStep());
    }
    if (E.getBody()) {
        walk(*E.getBody());
    }
}

void SemanticWalker::walk(AddressOfExprAST &E) {
    if (E.getOperand()) {
        walk(*E.getOperand());
    }
}

void SemanticWalker::walk(DerefExprAST &E) {
    if (E.getOperand()) {
        walk(*E.getOperand());
    }
}

void SemanticWalker::walk(NewExprAST &E) {
    // New expressions might involve generic types
    // Check if this is creating a generic type instance
    if (E.getOType().base == BaseType::Struct && !E.getOType().genericArgs.empty()) {
        enqueueStructInstantiation(E.getOType().structName, E.getOType().genericArgs);
    }

    // Walk all the argument expressions
    for (auto& arg : E.getArgs()) {
        if (arg) {
            walk(*arg);
        }
    }
}

void SemanticWalker::walk(NewArrayExprAST &E) {
    if (E.getSize()) {
        walk(*E.getSize());
    }
    
    // Check if this is creating a generic array type
    if (E.getElementType().base == BaseType::Struct && !E.getElementType().genericArgs.empty()) {
        enqueueStructInstantiation(E.getElementType().structName, E.getElementType().genericArgs);
    }
}

void SemanticWalker::walk(MatchExprAST &E) {
    if (E.getCond()) {
        walk(*E.getCond());
    }

    for (auto& casePair : E.getCases()) {
        if (casePair.Body) { // casePair.Body is the expression for this case
            walk(*casePair.Body);
        }
    }
}

void SemanticWalker::walk(IndexExprAST &E) {
    if (E.getArray()) {
        walk(*E.getArray());
    }
    if (E.getIndex()) {
        walk(*E.getIndex());
    }
}

void SemanticWalker::walk(MemberAccessAST &E) {
    if (E.getObject()) {
        walk(*E.getObject());
    }
    
    // Check if this is accessing a member of a generic type
    OType objType = E.getObject()->getOType();
    if (objType.base == BaseType::Struct && !objType.genericArgs.empty()) {
        enqueueStructInstantiation(objType.structName, objType.genericArgs);
    }
}

void SemanticWalker::walk(ArrayInitExprAST &E) {
    for (auto& element : E.getElements()) {
        if (element) {
            walk(*element);
        }
    }
}

void SemanticWalker::walk(ArrayLiteralExprAST &E) {
    // ArrayLiteralExprAST doesn't have individual elements to walk, just the type
    // For now, we don't need to do anything specific for semantic discovery
}

void SemanticWalker::walk(FunctionAST &F) {
    // Walk the function body to discover semantic requirements
    if (F.getBody()) {
        walk(*F.getBody());
    }
}

void SemanticWalker::walk(StructDeclAST &S) {
    // For generic structs, we need to walk the template to discover instantiations
    if (S.isGeneric()) {
        // Store the generic struct for later instantiation
        codeGen.GenericStructRegistry[S.getName()] = S.clone();

        // Walk methods and constructors to discover their requirements
        for (auto& method : S.getMethods()) {
            if (method->getBody()) {
                walk(*method->getBody());
            }
        }

        for (auto& constructor : S.getConstructors()) {
            if (constructor->getBody()) {
                walk(*constructor->getBody());
            }
        }
    } else {
        // For concrete structs, walk the bodies to discover any generic instantiations needed
        for (auto& method : S.getMethods()) {
            if (method->getBody()) {
                walk(*method->getBody());
            }
        }

        for (auto& constructor : S.getConstructors()) {
            if (constructor->getBody()) {
                walk(*constructor->getBody());
            }
        }
    }
}

void SemanticWalker::walk(ClassDeclAST &C) {
    // For classes, walk the methods and constructors to discover requirements
    for (auto& method : C.getMethods()) {
        if (method->getBody()) {
            walk(*method->getBody());
        }
    }
    
    for (auto& constructor : C.getConstructors()) {
        if (constructor->getBody()) {
            walk(*constructor->getBody());
        }
    }
}

void SemanticWalker::enqueueStructInstantiation(const std::string& baseName, const std::vector<OType>& typeArgs) {
    InstantiationRequest req;
    req.kind = InstantiationRequest::Struct;
    req.baseName = baseName;
    req.typeArgs = typeArgs;
    req.methodName = ""; // Not applicable for struct instantiation
    
    codeGen.instantiationManager->enqueue(req);
}

void SemanticWalker::walk(UnresolvedNewExprAST &E) {
    // This is where we resolve the unresolved new expression to a proper NewExprAST
    // Check if the struct exists and the constructor can be resolved

    // Check if this is creating a generic type instance
    if (E.getOType().base == BaseType::Struct && !E.getOType().genericArgs.empty()) {
        enqueueStructInstantiation(E.getOType().structName, E.getOType().genericArgs);
    }

    // Walk all the argument expressions
    for (auto& arg : E.getArgs()) {
        if (arg) {
            walk(*arg);
        }
    }

    // Now resolve the constructor for this new expression
    // Find the appropriate constructor based on argument types
    std::string className = E.getClassName();
    std::vector<OType> argTypes;

    // Get types of arguments
    for (auto& arg : E.getArgs()) {
        if (arg) {
            argTypes.push_back(arg->getOType());
        }
    }

    // Create constructor name based on class name and argument types
    std::string constructorName = className + "_new";

    if (!argTypes.empty()) {
        // Mangle the constructor name with argument types
        constructorName = codeGen.utilCodeGen->mangleGenericName(constructorName, argTypes);
    }

    // Check if the constructor exists in the module
    llvm::Function *constructorFunc = codeGen.TheModule->getFunction(constructorName);
    if (constructorFunc) {
        // Mark the expression as resolved with the constructor name
        E.markResolved(constructorName);
    } else {
        // If constructor doesn't exist yet, it might be defined later or in another file
        // We'll defer this resolution until all imports are complete
        codeGen.logError(("Constructor not found: " + constructorName).c_str());
    }
}

void SemanticWalker::enqueueMethodInstantiation(const std::string& baseName, const std::string& methodName, const std::vector<OType>& typeArgs) {
    InstantiationRequest req;
    req.kind = InstantiationRequest::Method;
    req.baseName = baseName;
    req.typeArgs = typeArgs;
    req.methodName = methodName;

    codeGen.instantiationManager->enqueue(req);
}