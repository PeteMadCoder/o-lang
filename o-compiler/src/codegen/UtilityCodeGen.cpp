#include "UtilityCodeGen.h"
#include "FunctionCodeGen.h"

llvm::Function *UtilityCodeGen::getFreeFunc() {
    llvm::Function *FreeF = codeGen.TheModule->getFunction("free");
    if (!FreeF) {
        std::vector<llvm::Type*> Args;
        Args.push_back(llvm::PointerType::get(*codeGen.TheContext, 0)); // void* (opaque ptr)
        llvm::FunctionType *FT = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*codeGen.TheContext), Args, false);
        FreeF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "free", codeGen.TheModule);
    }
    return FreeF;
}

llvm::Type* UtilityCodeGen::getLLVMType(const OType& t) {
    // Validate the input OType to prevent invalid array sizes from causing std::bad_array_new_length
    for (int arraySize : t.arraySizes) {
        if (arraySize < -1) {  // -1 is valid for slices
            fprintf(stderr, "Error: Invalid array size %d in OType\n", arraySize);
            return llvm::Type::getVoidTy(*codeGen.TheContext);
        }
        if (arraySize == 0) {
            fprintf(stderr, "Warning: Array size 0 detected, using size 1\n");
        }
        if (arraySize > 100000000) {  // 100 million - conservative limit
            fprintf(stderr, "Error: Array size %d is too large\n", arraySize);
            return llvm::Type::getVoidTy(*codeGen.TheContext);
        }
    }

    llvm::Type* baseType;

    switch (t.base) {
        case BaseType::Int: baseType = llvm::Type::getInt32Ty(*codeGen.TheContext); break;
        case BaseType::Float: baseType = llvm::Type::getDoubleTy(*codeGen.TheContext); break;
        case BaseType::Bool: baseType = llvm::Type::getInt1Ty(*codeGen.TheContext); break;
        case BaseType::Char: baseType = llvm::Type::getInt8Ty(*codeGen.TheContext); break;
        case BaseType::Byte: baseType = llvm::Type::getInt8Ty(*codeGen.TheContext); break;
        case BaseType::Struct:
            if (codeGen.StructTypes.find(t.structName) != codeGen.StructTypes.end()) {
                baseType = codeGen.StructTypes[t.structName];
            } else if (!t.genericArgs.empty()) {
                // Validate generic args to prevent infinite recursion or invalid instantiation
                if (t.structName.length() > 1000) {
                    fprintf(stderr, "Error: Struct name too long: %s\n", t.structName.c_str());
                    return llvm::Type::getVoidTy(*codeGen.TheContext);
                }

                // Check if generic args are valid
                for (const auto& genArg : t.genericArgs) {
                    if (genArg.base == BaseType::Void) {
                        fprintf(stderr, "Error: Invalid generic argument (void type) for struct %s\n", t.structName.c_str());
                        return llvm::Type::getVoidTy(*codeGen.TheContext);
                    }
                }

                instantiateStruct(t.structName, t.genericArgs);
                std::string mangled = mangleGenericName(t.structName, t.genericArgs);
                if (codeGen.StructTypes.count(mangled)) {
                    baseType = codeGen.StructTypes[mangled];
                } else {
                    baseType = llvm::Type::getVoidTy(*codeGen.TheContext);
                }
            } else {
                // Check if this is a mangled generic type name
                if (codeGen.StructTypes.count(t.structName)) {
                    baseType = codeGen.StructTypes[t.structName];
                } else {
                    baseType = llvm::Type::getVoidTy(*codeGen.TheContext);
                }
            }
            break;
        default: baseType = llvm::Type::getVoidTy(*codeGen.TheContext); break;
    }

    // Handle Arrays (Inner to Outer)
    for (auto it = t.arraySizes.rbegin(); it != t.arraySizes.rend(); ++it) {
        int size = *it;
        if (size == -1) {
            // Slice
            baseType = getSliceType(baseType);
        } else {
            // Fixed Array - validate size to prevent std::bad_array_new_length
            if (size < 0) {
                fprintf(stderr, "Error: Invalid array size %d\n", size);
                baseType = llvm::Type::getVoidTy(*codeGen.TheContext);
                break;
            }
            // Prevent creating arrays with size 0 or extremely large sizes which can cause issues
            if (size == 0) {
                fprintf(stderr, "Warning: Creating array with size 0, using size 1\n");
                size = 1; // Use minimum size to prevent std::bad_array_new_length
            }
            // Additional safety check: make sure size is not too large to cause allocation issues
            if (size > 100000000) {  // More conservative limit
                fprintf(stderr, "Error: Array size %d is too large (> 100M), using void type\n", size);
                baseType = llvm::Type::getVoidTy(*codeGen.TheContext);
                break;
            }

            // Check if baseType is valid before creating array
            if (!baseType) {
                fprintf(stderr, "Error: Invalid base type when creating array\n");
                baseType = llvm::Type::getVoidTy(*codeGen.TheContext);
                break;
            }

            // Fixed Array
            try {
                baseType = llvm::ArrayType::get(baseType, size);
            } catch (const std::exception& e) {
                fprintf(stderr, "Error creating array type with size %d: %s\n", size, e.what());
                baseType = llvm::Type::getVoidTy(*codeGen.TheContext);
                break;
            }
        }
    }

    // Handle Pointers
    for (int i = 0; i < t.pointerDepth; ++i) {
        baseType = llvm::PointerType::get(baseType, 0);
    }

    return baseType;
}

std::string UtilityCodeGen::mangleGenericName(const std::string& baseName, const std::vector<OType>& args) {
    std::string name = baseName;
    for (const auto& arg : args) {
        name += "_";
        for (int i=0; i<arg.pointerDepth; ++i) name += "ptr_"; // Add ptr prefix

        if (arg.base == BaseType::Int) name += "int";
        else if (arg.base == BaseType::Float) name += "float";
        else if (arg.base == BaseType::Bool) name += "bool";
        else if (arg.base == BaseType::Char) name += "char";
        else if (arg.base == BaseType::Byte) name += "byte";
        else if (arg.base == BaseType::Struct) {
             name += arg.structName;
        }
        else name += "void";
    }
    return name;
}

llvm::Type* UtilityCodeGen::instantiateStruct(const std::string& genericName, const std::vector<OType>& typeArgs) {
    if (codeGen.GenericStructRegistry.count(genericName) == 0) return nullptr;

    std::string mangledName = mangleGenericName(genericName, typeArgs);

    // Check if we're already in the process of instantiating this type
    if (InProgressInstantiations.count(mangledName)) {
        std::cerr << "Warning: Recursive instantiation detected for " << mangledName << ", returning existing type if available\n";
        if (codeGen.StructTypes.count(mangledName)) {
            return codeGen.StructTypes[mangledName];
        }
        return nullptr;
    }

    if (codeGen.StructTypes.count(mangledName)) {
        // Already instantiated, return the existing type
        return codeGen.StructTypes[mangledName];
    }

    // Mark this instantiation as in-progress
    InProgressInstantiations.insert(mangledName);

    const auto& genericAST = codeGen.GenericStructRegistry[genericName];
    if (genericAST->getGenericParams().size() != typeArgs.size()) {
        codeGen.logError(("Incorrect number of type arguments for " + genericName).c_str());
        InProgressInstantiations.erase(mangledName);
        return nullptr;
    }

    std::map<std::string, OType> typeMap;
    for (size_t i = 0; i < genericAST->getGenericParams().size(); ++i) {
        typeMap[genericAST->getGenericParams()[i]] = typeArgs[i];
    }

    auto newAST = genericAST->clone(typeMap);
    if (!newAST) {
        fprintf(stderr, "Error: Failed to clone generic AST for %s\n", mangledName.c_str());
        InProgressInstantiations.erase(mangledName);
        return nullptr;
    }
    newAST->setName(mangledName);
    newAST->makeConcrete();

    // GENERATE TYPE LAYOUT (We need the llvm::StructType immediately)
    std::vector<llvm::Type*> fieldTypes;
    std::vector<FieldInfo> fieldInfos;

    size_t offset = 0;
    for (const auto& field : newAST->getFields()) {
        // Explicitly substitute field types to ensure proper generic instantiation
        OType substitutedFieldType = field.second.substitute(typeMap);

        // Validate the substituted field type before processing
        bool hasInvalidArraySize = false;
        for (int arraySize : substitutedFieldType.arraySizes) {
            if (arraySize < 0 || arraySize == 0) {  // Both negative and zero are invalid for fixed arrays
                fprintf(stderr, "Warning: Invalid array size %d in field type, skipping field\n", arraySize);
                hasInvalidArraySize = true;
                break;
            }
            // Also check for extremely large array sizes
            if (arraySize > 100000000) {  // 100 million - very conservative limit
                fprintf(stderr, "Warning: Array size %d is extremely large, skipping field\n", arraySize);
                hasInvalidArraySize = true;
                break;
            }
        }

        if (hasInvalidArraySize) {
            continue; // Skip this field
        }

        // Validate the substitutedFieldType before calling getLLVMType
        // Check for any invalid array sizes that might cause std::bad_array_new_length
        bool isValidType = true;
        for (int arraySize : substitutedFieldType.arraySizes) {
            if (arraySize < -1 || arraySize == 0) {  // -1 is valid for slices, positive numbers are valid for fixed arrays
                fprintf(stderr, "Error: Invalid array size %d in substituted field type for field %s in struct %s\n",
                        arraySize, field.first.c_str(), mangledName.c_str());
                isValidType = false;
                break;
            }
            // Check for extremely large array sizes that could cause allocation issues
            if (arraySize > 100000000) {  // 100 million - conservative limit
                fprintf(stderr, "Error: Array size %d is too large in substituted field type for field %s in struct %s\n",
                        arraySize, field.first.c_str(), mangledName.c_str());
                isValidType = false;
                break;
            }
        }

        if (!isValidType) {
            fprintf(stderr, "Skipping field %s due to invalid type in struct %s\n", field.first.c_str(), mangledName.c_str());
            continue;
        }

        llvm::Type* fieldType = getLLVMType(substitutedFieldType);
        if (!fieldType) {
            // If we can't get the LLVM type, try instantiating dependent generic types first
            if (substitutedFieldType.base == BaseType::Struct && !substitutedFieldType.genericArgs.empty()) {
                // Check for potential infinite recursion
                std::string depMangledName = mangleGenericName(substitutedFieldType.structName, substitutedFieldType.genericArgs);
                if (depMangledName.length() > 1000) {  // Prevent extremely long names that might cause issues
                    fprintf(stderr, "Error: Mangled name too long, potential infinite recursion: %s\n", depMangledName.c_str());
                    InProgressInstantiations.erase(mangledName);
                    return nullptr;
                }

                if (InProgressInstantiations.count(depMangledName)) {
                    fprintf(stderr, "Error: Circular dependency detected in generic instantiation: %s\n", depMangledName.c_str());
                    InProgressInstantiations.erase(mangledName);
                    return nullptr;
                }

                llvm::Type* depType = instantiateStruct(substitutedFieldType.structName, substitutedFieldType.genericArgs);
                if (depType) {
                    fieldType = getLLVMType(substitutedFieldType);
                }
            }
            if (!fieldType) {
                fprintf(stderr, "Error: Could not get LLVM type for field %s in struct %s, skipping\n", field.first.c_str(), mangledName.c_str());
                continue; // Error
            }
        }

        // Additional validation: make sure fieldType is valid before adding to vector
        if (!fieldType) {
            fprintf(stderr, "Error: fieldType is null for field %s in struct %s, skipping\n", field.first.c_str(), mangledName.c_str());
            continue;
        }

        fieldTypes.push_back(fieldType);

        FieldInfo info;
        info.name = field.first;
        info.type = substitutedFieldType;  // Use the substituted type
        info.offset = offset;
        fieldInfos.push_back(info);

        // Simple offset calculation (no padding for now)
        if (substitutedFieldType.base == BaseType::Int || substitutedFieldType.base == BaseType::Float) offset += 4;
        else if (substitutedFieldType.base == BaseType::Char || substitutedFieldType.base == BaseType::Byte || substitutedFieldType.base == BaseType::Bool) offset += 1;
        else offset += 8; // pointers and other types
    }

    // Create LLVM struct type
    llvm::StructType* structType = llvm::StructType::create(*codeGen.TheContext, mangledName);
    codeGen.StructTypes[mangledName] = structType;

    // Validate field types before setting the body
    bool hasInvalidType = false;
    for (auto* fieldType : fieldTypes) {
        if (!fieldType) {
            fprintf(stderr, "Error: Null field type found in struct %s\n", mangledName.c_str());
            hasInvalidType = true;
            break;
        }
    }

    if (hasInvalidType) {
        fprintf(stderr, "Error: Invalid field types found, skipping struct body for %s\n", mangledName.c_str());
        InProgressInstantiations.erase(mangledName);
        return nullptr;
    }

    // Additional validation: check for extremely large number of fields
    if (fieldTypes.size() > 10000) {
        fprintf(stderr, "Error: Too many fields (%zu) in struct %s, likely a recursion issue\n", fieldTypes.size(), mangledName.c_str());
        InProgressInstantiations.erase(mangledName);
        return nullptr;
    }

    // Additional validation before setting struct body
    if (fieldTypes.empty()) {
        fprintf(stderr, "Warning: No fields found for struct %s, using empty struct\n", mangledName.c_str());
        structType->setBody({});
    } else {
        // Validate all field types before setting body
        bool allValid = true;
        for (size_t i = 0; i < fieldTypes.size(); ++i) {
            if (!fieldTypes[i]) {
                fprintf(stderr, "Error: Field %zu in struct %s has null type\n", i, mangledName.c_str());
                allValid = false;
                break;
            }
        }

        if (!allValid) {
            fprintf(stderr, "Error: Invalid field types in struct %s, using empty struct\n", mangledName.c_str());
            structType->setBody({});
        } else {
            // Check for potential size issues that might cause std::bad_array_new_length
            if (fieldTypes.size() > 1000000) {  // Very conservative limit
                fprintf(stderr, "Error: Too many fields (%zu) in struct %s\n", fieldTypes.size(), mangledName.c_str());
                structType->setBody({});
            } else {
                // Additional safety check: validate each field type more thoroughly
                bool hasProblematicType = false;
                for (size_t i = 0; i < fieldTypes.size(); ++i) {
                    if (fieldTypes[i]->isFunctionTy() || fieldTypes[i]->isVoidTy()) {
                        fprintf(stderr, "Warning: Skipping problematic field type at index %zu in struct %s\n", i, mangledName.c_str());
                        hasProblematicType = true;
                        break;
                    }
                }

                if (hasProblematicType) {
                    // Create a struct with only valid field types
                    std::vector<llvm::Type*> validFieldTypes;
                    for (auto* ft : fieldTypes) {
                        if (ft && !ft->isFunctionTy() && !ft->isVoidTy()) {
                            validFieldTypes.push_back(ft);
                        }
                    }

                    if (validFieldTypes.empty()) {
                        structType->setBody({});
                    } else {
                        structType->setBody(validFieldTypes);
                    }
                } else {
                    // Set the body of the struct after all dependencies are resolved
                    try {
                        structType->setBody(fieldTypes);
                    } catch (...) {
                        fprintf(stderr, "Error: Exception during struct body creation for %s, using empty struct\n", mangledName.c_str());
                        structType->setBody({});
                    }
                }
            }
        }
    }

    // Get Layout for accurate offsets and size
    const llvm::StructLayout *Layout = codeGen.TheModule->getDataLayout().getStructLayout(structType);
    if (!Layout) {
        fprintf(stderr, "Error: Failed to get layout for struct\n");
        InProgressInstantiations.erase(mangledName);
        return nullptr;
    }

    // Update field offsets
    for(size_t i = 0; i < fieldInfos.size(); ++i) {
        fieldInfos[i].offset = Layout->getElementOffset(i);
    }

    // Register in type registry with correct size
    TypeRegistry::getInstance().registerStruct(mangledName, fieldInfos, Layout->getSizeInBytes());

    // GENERATE PROTOTYPES (Declarations only, no bodies)
    // Generate prototypes for methods of the instantiated struct
    // Add safety check for newAST validity
    if (!newAST) {
        fprintf(stderr, "Error: newAST is null in instantiateStruct for %s\n", mangledName.c_str());
        InProgressInstantiations.erase(mangledName);
        return nullptr;
    }

    // GENERATE PROTOTYPES (Declarations only, no bodies)
    // Generate prototypes for methods of the instantiated struct
    // Add safety check for newAST validity
    if (!newAST) {
        fprintf(stderr, "Error: newAST is null in instantiateStruct for %s\n", mangledName.c_str());
        InProgressInstantiations.erase(mangledName);
        return nullptr;
    }

    // Iterate directly over the methods without copying
    for (auto& method : newAST->getMethods()) {
        if (!method) {
            fprintf(stderr, "Error: null method in instantiateStruct for %s\n", mangledName.c_str());
            continue;
        }

        auto proto = method->getPrototype();
        if (!proto) {
            fprintf(stderr, "Error: null prototype for method in instantiateStruct for %s\n", mangledName.c_str());
            continue;
        }

        // Mangle method name: StructName_methodName
        std::string originalName = proto->getName();
        std::string mangledMethodName = mangledName + "_" + originalName;

        // Create a new prototype with the mangled name
        auto clonedProto = proto->clone();
        if (!clonedProto) {
            fprintf(stderr, "Error: failed to clone prototype for method %s in instantiateStruct for %s\n", originalName.c_str(), mangledName.c_str());
            continue;
        }

        clonedProto->setName(mangledMethodName);

        // Inject 'this' parameter as first argument with the instantiated struct type
        clonedProto->injectThisParameter(mangledName);

        // Generate the function declaration (prototype only)
        llvm::Function *TheFunction = codeGen.funcCodeGen->codegen(*clonedProto);
        if (!TheFunction) {
            fprintf(stderr, "Error: failed to generate function for method %s in instantiateStruct for %s\n", originalName.c_str(), mangledName.c_str());
            continue;
        }

        // Register the prototype in the global registry
        std::string MangledName = clonedProto->getName();
        RegisterFunctionProto(std::move(clonedProto)); // Register in Global Map
    }

    // Generate prototypes for constructors of the instantiated struct
    // Iterate directly over the constructors without copying
    for (auto& constructor : newAST->getConstructors()) {
        if (!constructor) {
            fprintf(stderr, "Error: null constructor in instantiateStruct for %s\n", mangledName.c_str());
            continue;
        }

        // Create constructor function name: StructName_new[_ArgType...]
        std::string funcName = mangledName + "_new";

        // Mangle name if parameters exist (simple mangling)
        std::vector<OType> paramOTypes;
        for (const auto& param : constructor->getParams()) {
            paramOTypes.push_back(param.second);
        }

        if (!paramOTypes.empty()) {
            funcName = mangleGenericName(funcName, paramOTypes);
        }

        // Params: this, args...
        std::vector<llvm::Type*> paramTypes;
        std::vector<std::string> paramNames;

        // 1. Add 'this' parameter
        if (codeGen.StructTypes.find(mangledName) == codeGen.StructTypes.end()) {
            codeGen.logError("Unknown struct type for constructor");
            continue;
        }
        llvm::Type* structType = codeGen.StructTypes[mangledName];
        if (!structType) {
            fprintf(stderr, "Error: struct type is null for constructor in instantiateStruct for %s\n", mangledName.c_str());
            continue;
        }
        paramTypes.push_back(llvm::PointerType::get(structType, 0));
        paramNames.push_back("this");

        // 2. Add user parameters
        for (const auto& param : constructor->getParams()) {
            llvm::Type* paramType = getLLVMType(param.second);
            if (!paramType) {
                fprintf(stderr, "Error: failed to get LLVM type for parameter in constructor for %s\n", mangledName.c_str());
                continue;
            }
            paramTypes.push_back(paramType);
            paramNames.push_back(param.first);
        }

        // Constructor returns void (it initializes the memory passed in 'this')
        llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(*codeGen.TheContext), paramTypes, false);

        // Check if function already exists
        llvm::Function* func = codeGen.TheModule->getFunction(funcName);
        if (!func) {
            func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, funcName, codeGen.TheModule);
        }

        // Register the constructor prototype
        std::vector<std::pair<std::string, OType>> args;

        // 1. Add 'this' (always first)
        args.push_back({"this", OType(BaseType::Struct, 1, mangledName)});

        // 2. Add user parameters
        for (size_t i = 0; i < paramOTypes.size(); ++i) {
            // paramNames[0] is 'this', so user params start at 1
            if (i + 1 < paramNames.size()) {
                args.push_back({paramNames[i+1], paramOTypes[i]});
            }
        }

        // Create a temporary prototype just for registration purposes
        auto tempProto = std::make_unique<PrototypeAST>(funcName, args, OType(BaseType::Void));
        if (tempProto) {
            RegisterFunctionProto(std::move(tempProto)); // Register in Global Map
        }
    }

    // QUEUE FOR LATER (Add to the instantiation queue for deferred body generation)
    codeGen.utilCodeGen->getInstantiationQueue().push_back({std::move(newAST), mangledName});

    // Remove from in-progress set
    InProgressInstantiations.erase(mangledName);

    return structType;
}

llvm::Function *UtilityCodeGen::getFunctionFromPrototype(std::string Name) {
    auto it = codeGen.GlobalFunctionProtos.find(Name);
    if (it != codeGen.GlobalFunctionProtos.end() && it->second) {
        // Just return the function if it already exists in the current module
        llvm::Function *F = codeGen.TheModule->getFunction(Name);
        if (F) return F;

        // Otherwise, we need to create it from the prototype
        // But be very careful here to avoid segfaults
        auto protoSharedPtr = it->second;

        // Check if the shared pointer is valid before dereferencing
        if (!protoSharedPtr) {
            fprintf(stderr, "Error: Null prototype found for function: %s\n", Name.c_str());
            return nullptr;
        }

        // Generate code for the function
        return codeGen.funcCodeGen->codegen(*protoSharedPtr);
    }
    return nullptr;
}

void UtilityCodeGen::RegisterFunctionProto(std::unique_ptr<PrototypeAST> Proto) {
    std::string Name = Proto->getName();
    codeGen.GlobalFunctionProtos[Name] = std::shared_ptr<PrototypeAST>(Proto->clone());
}

llvm::StructType* UtilityCodeGen::getSliceType(llvm::Type* ElementType) {
    std::vector<llvm::Type*> Elements;
    Elements.push_back(llvm::Type::getInt32Ty(*codeGen.TheContext)); // Size
    Elements.push_back(llvm::PointerType::get(ElementType, 0)); // Pointer
    return llvm::StructType::get(*codeGen.TheContext, Elements);
}

// Helper to get or create the llvm.trap intrinsic
llvm::Function *UtilityCodeGen::getTrapFunc() {
    return llvm::Intrinsic::getDeclaration(codeGen.TheModule, llvm::Intrinsic::trap);
}

// Helper to create bounds check
void UtilityCodeGen::createBoundsCheck(llvm::Value *IndexVal, llvm::Value *ArraySizeVal) {
    llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *CheckBlock = llvm::BasicBlock::Create(*codeGen.TheContext, "bounds_check", TheFunction);
    llvm::BasicBlock *ContinueBlock = llvm::BasicBlock::Create(*codeGen.TheContext, "bounds_ok", TheFunction);
    llvm::BasicBlock *ErrorBlock = llvm::BasicBlock::Create(*codeGen.TheContext, "bounds_fail", TheFunction);

    codeGen.Builder->CreateBr(CheckBlock);
    codeGen.Builder->SetInsertPoint(CheckBlock);

    // Check if Index < 0 || Index >= Size
    // Using unsigned comparison (uge) handles both cases:
    // If Index is negative (e.g. -1), it wraps to a large unsigned number, which is >= Size.
    llvm::Value *OutOfBounds = codeGen.Builder->CreateICmpUGE(IndexVal, ArraySizeVal, "out_of_bounds");

    codeGen.Builder->CreateCondBr(OutOfBounds, ErrorBlock, ContinueBlock);

    // Error Block
    codeGen.Builder->SetInsertPoint(ErrorBlock);
    codeGen.Builder->CreateCall(getTrapFunc(), {});
    codeGen.Builder->CreateUnreachable();

    // Continue Block
    codeGen.Builder->SetInsertPoint(ContinueBlock);
}

std::vector<PendingInstantiation>& UtilityCodeGen::getInstantiationQueue() {
    return codeGen.InstantiationQueue;
}