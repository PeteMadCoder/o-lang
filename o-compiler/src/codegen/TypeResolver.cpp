#include "TypeResolver.h"
#include "UtilityCodeGen.h"

void TypeResolver::resolve(StructDeclAST &S) {
    // Skip semantic resolution for generic structs - they need to be instantiated first
    if (S.isGeneric()) {
        codeGen.GenericStructRegistry[S.getName()] = S.clone();
        return;
    }

    // For concrete structs, register their layout and fields
    std::vector<FieldInfo> fieldInfos;

    size_t offset = 0;
    for (const auto& field : S.getFields()) {
        FieldInfo info;
        info.name = field.first;
        info.type = field.second;
        info.offset = offset;
        fieldInfos.push_back(info);

        // Simple offset calculation (no padding for now)
        if (field.second.base == BaseType::Int || field.second.base == BaseType::Float) offset += 4;
        else if (field.second.base == BaseType::Char || field.second.base == BaseType::Byte || field.second.base == BaseType::Bool) offset += 1;
        else offset += 8; // pointers and other types
    }

    // Register in type registry with correct size
    TypeRegistry::getInstance().registerStruct(S.getName(), fieldInfos, offset);

    // Resolve method signatures (but don't generate bodies)
    resolveMethods(S);

    // Resolve constructor signatures (but don't generate bodies)
    resolveConstructors(S);
}

void TypeResolver::resolve(ClassDeclAST &C) {
    // For classes, register their layout and fields
    std::vector<FieldInfo> fieldInfos;

    size_t offset = 0;

    // Check inheritance logic
    if (C.hasParent()) {
         if (TypeRegistry::getInstance().hasStruct(C.getParentName())) {
             const StructInfo& parentInfo = TypeRegistry::getInstance().getStruct(C.getParentName());
             // Inherit Virtual Methods from Parent
             std::vector<std::string> parentVMethods = parentInfo.virtualMethods;
         }
    }

    // Add hidden vptr field at index 0 if class has virtual methods or inherits from class with virtual methods
    bool needsVTable = !C.getVirtualMethods().empty() || (C.hasParent() && C.isOpen());

    if (needsVTable) {
        FieldInfo vptrInfo;
        vptrInfo.name = "__vptr";
        vptrInfo.type = OType(BaseType::Void, 2); // void**
        vptrInfo.offset = offset;
        fieldInfos.push_back(vptrInfo);

        offset += 8; // vptr is 8 bytes
    }

    // Struct Prefixing: If this class has a parent, include parent fields
    if (C.hasParent()) {
        if (TypeRegistry::getInstance().hasStruct(C.getParentName())) {
            const StructInfo& parentInfo = TypeRegistry::getInstance().getStruct(C.getParentName());

            // Copy parent fields (skip vptr if already added)
            for (const auto& parentField : parentInfo.fields) {
                if (needsVTable && parentField.name == "__vptr") continue; // Skip parent vptr

                FieldInfo info;
                info.name = parentField.name;
                info.type = parentField.type;
                info.offset = offset;
                fieldInfos.push_back(info);

                offset += TypeRegistry::getInstance().getTypeSize(parentField.type);
            }
        }
    }

    // Add child-specific fields
    for (const auto& field : C.getFields()) {
        FieldInfo info;
        info.name = field.first;
        info.type = field.second;
        info.offset = offset;
        fieldInfos.push_back(info);

        offset += TypeRegistry::getInstance().getTypeSize(field.second);
    }

    // Register in type registry
    TypeRegistry::getInstance().registerStruct(C.getName(), fieldInfos, C.getVirtualMethods());

    // Resolve method signatures (but don't generate bodies)
    resolveMethods(C);

    // Resolve constructor signatures (but don't generate bodies)
    resolveConstructors(C);
}

void TypeResolver::resolveMethods(StructDeclAST &S) {
    for (auto& method : S.getMethods()) {
        // Mangle method name: StructName_methodName
        std::string originalName = method->getPrototype()->getName();
        std::string mangledName = S.getName() + "_" + originalName;

        // Update the method's prototype name
        method->getPrototype()->setName(mangledName);

        // Inject 'this' parameter as first argument
        method->getPrototype()->injectThisParameter(S.getName());

        // Register the prototype in the global registry for external functions
        // This is critical for deferred method resolution
        if (codeGen.GlobalFunctionProtos.find(mangledName) == codeGen.GlobalFunctionProtos.end()) {
            // Create a copy of the prototype to store in the registry using shared_ptr
            codeGen.GlobalFunctionProtos[mangledName] = std::shared_ptr<PrototypeAST>(method->getPrototype()->clone());
        }
    }
}

void TypeResolver::resolveConstructors(StructDeclAST &S) {
    for (auto& constructor : S.getConstructors()) {
        // Create constructor function name: StructName_new[_ArgType...]
        std::string funcName = S.getName() + "_new";

        // Mangle name if parameters exist (simple mangling)
        std::vector<OType> paramOTypes;
        for (const auto& param : constructor->getParams()) {
            paramOTypes.push_back(param.second);
        }

        if (!paramOTypes.empty()) {
            funcName = codeGen.utilCodeGen->mangleGenericName(funcName, paramOTypes);
        }

        // Params: this, args...
        std::vector<std::pair<std::string, OType>> args;
        args.push_back({"this", OType(BaseType::Struct, 1, S.getName())}); // this*
        for (const auto& param : constructor->getParams()) {
            args.push_back(param);
        }

        PrototypeAST proto(funcName, args, OType(BaseType::Void));
        
        // Register the prototype in the global registry
        if (codeGen.GlobalFunctionProtos.find(funcName) == codeGen.GlobalFunctionProtos.end()) {
            codeGen.GlobalFunctionProtos[funcName] = std::shared_ptr<PrototypeAST>(proto.clone());
        }
    }
}

void TypeResolver::resolveMethods(ClassDeclAST &C) {
    for (auto& method : C.getMethods()) {
        std::string originalName = method->getPrototype()->getName();
        std::string mangledName = C.getName() + "_" + originalName;
        method->getPrototype()->setName(mangledName);
        method->getPrototype()->injectThisParameter(C.getName());
        
        // Register the prototype in the global registry
        if (codeGen.GlobalFunctionProtos.find(mangledName) == codeGen.GlobalFunctionProtos.end()) {
            codeGen.GlobalFunctionProtos[mangledName] = std::shared_ptr<PrototypeAST>(method->getPrototype()->clone());
        }
    }
}

void TypeResolver::resolveConstructors(ClassDeclAST &C) {
    for (const auto& constructor : C.getConstructors()) {
        // Create mangled name
        std::string funcName = C.getName() + "_new";
        std::vector<OType> paramOTypes;
        for (const auto& param : constructor->getParams()) {
            paramOTypes.push_back(param.second);
        }
        if (!paramOTypes.empty()) {
            funcName = codeGen.utilCodeGen->mangleGenericName(funcName, paramOTypes);
        }

        // Create Prototype
        // Params: this, args...
        std::vector<std::pair<std::string, OType>> args;
        args.push_back({"this", OType(BaseType::Struct, 1, C.getName())}); // this*
        for (const auto& param : constructor->getParams()) {
            args.push_back(param);
        }

        PrototypeAST proto(funcName, args, OType(BaseType::Void));
        
        // Register the prototype in the global registry
        if (codeGen.GlobalFunctionProtos.find(funcName) == codeGen.GlobalFunctionProtos.end()) {
            codeGen.GlobalFunctionProtos[funcName] = std::shared_ptr<PrototypeAST>(proto.clone());
        }
    }
}

void TypeResolver::processSemanticResolution() {
    // Process all pending instantiations semantically
    // This method should be called during the semantic discovery phase
    
    // For now, we'll just process the generic struct registry
    // In a full implementation, this would process the instantiation queue
    // and resolve all pending generic instantiations
    
    // Note: This is a simplified version - in a full implementation,
    // we would iterate through pending instantiations and resolve them
    // without generating any LLVM IR
}