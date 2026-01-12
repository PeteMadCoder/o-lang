#include "TypeCodeGen.h"
#include "FunctionCodeGen.h"
#include "UtilityCodeGen.h"
#include "ExpressionCodeGen.h"

void TypeCodeGen::codegen(StructDeclAST &S) {
    std::cerr << "Generating Struct: " << S.getName() << ". Constructors: " << S.getConstructors().size() << "\n";
    // Skip codegen for generic structs - they need to be instantiated first
    if (S.isGeneric()) {
        codeGen.GenericStructRegistry[S.getName()] = S.clone();
        return;
    }

    // Convert fields to LLVM types
    std::vector<llvm::Type*> fieldTypes;
    std::vector<FieldInfo> fieldInfos;

    size_t offset = 0;
    for (const auto& field : S.getFields()) {
        llvm::Type* fieldType = codeGen.utilCodeGen->getLLVMType(field.second);
        if (!fieldType) return; // Error

        fieldTypes.push_back(fieldType);

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

    // Create LLVM struct type
    llvm::StructType* structType = llvm::StructType::create(*codeGen.TheContext, fieldTypes, S.getName());
    codeGen.StructTypes[S.getName()] = structType;

    // Get Layout for accurate offsets and size
    const llvm::StructLayout *Layout = codeGen.TheModule->getDataLayout().getStructLayout(structType);
    if (!Layout) {
        codeGen.logError(("Failed to get layout for struct: " + S.getName()).c_str());
        return;
    }

    // Update field offsets
    for(size_t i = 0; i < fieldInfos.size(); ++i) {
        fieldInfos[i].offset = Layout->getElementOffset(i);
    }

    // Register in type registry with correct size
    TypeRegistry::getInstance().registerStruct(S.getName(), fieldInfos, Layout->getSizeInBytes());

    // 1. Generate Prototypes for Methods
    for (auto& method : S.getMethods()) {
        // Mangle method name: StructName_methodName
        std::string originalName = method->getPrototype()->getName();
        std::string mangledName = S.getName() + "_" + originalName;

        // Update the method's prototype name
        method->getPrototype()->setName(mangledName);

        // Inject 'this' parameter as first argument
        method->getPrototype()->injectThisParameter(S.getName());

        // Generate the function declaration
        llvm::Function *TheFunction = codeGen.funcCodeGen->codegen(*method->getPrototype());

        // Register the prototype in the global registry for external functions
        // This is critical for deferred method resolution
        if (codeGen.GlobalFunctionProtos.find(mangledName) == codeGen.GlobalFunctionProtos.end()) {
            // Create a copy of the prototype to store in the registry using shared_ptr
            codeGen.GlobalFunctionProtos[mangledName] = std::shared_ptr<PrototypeAST>(method->getPrototype()->clone());
        }
    }

    // 2. Generate Prototypes for Constructors
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
        codeGen.funcCodeGen->codegen(proto); // Declare function
    }

    // 3. CONTEXT PRESERVATION AND METHOD GENERATION
    // We are likely inside 'main' or another function right now.
    // We must save this location so we don't accidentally inject generic code into it.
    llvm::BasicBlock *OldBlock = codeGen.Builder->GetInsertBlock();

    // 4. DETACH BUILDER temporarily
    // This prevents the new methods from writing into 'main' if they fail to set a block.
    codeGen.Builder->ClearInsertionPoint();

    // 5. Generate Method Bodies with proper context management
    for (auto& method : S.getMethods()) {
        // A. Generate the Function Declaration (Prototype)
        llvm::Function *TheFunction = codeGen.funcCodeGen->codegen(*method->getPrototype());
        if (!TheFunction) continue;

        // B. FORCE VALID CONTEXT (The Fix)
        // Create a dedicated entry block for this method immediately.
        llvm::BasicBlock *MethodBlock = llvm::BasicBlock::Create(*codeGen.TheContext, "entry", TheFunction);
        codeGen.Builder->SetInsertPoint(MethodBlock);

        // C. GENERATE BODY
        // If the method has a body, we need to process it carefully
        if (method->getBody()) {
            // Process the method body with proper variable setup
            codeGen.ScopeStack.clear();
            codeGen.enterScope(); // Function Scope

            // Register types from prototype
            const auto& ProtoArgs = method->getPrototype()->getArgs();
            for (const auto& ArgPair : ProtoArgs) {
                codeGen.addVariableType(ArgPair.first, ArgPair.second);
            }

            // Set up arguments in the function
            for (auto &Arg : TheFunction->args()) {
                llvm::AllocaInst *Alloca = codeGen.createEntryBlockAlloca(TheFunction, std::string(Arg.getName()), Arg.getType());
                codeGen.Builder->CreateStore(&Arg, Alloca);
                codeGen.addVariable(std::string(Arg.getName()), Alloca);
            }

            // Generate the method body
            // This is where calls to external functions like 'exit' are processed
            llvm::Value *RetVal = codeGen.exprCodeGen->codegen(*method->getBody());

            // Add return instruction if block is not terminated
            if (!codeGen.Builder->GetInsertBlock()->getTerminator()) {
                if (method->getPrototype()->getReturnType().base == BaseType::Void) {
                    codeGen.Builder->CreateRetVoid();
                } else {
                    // For non-void functions, return a default value
                    llvm::Type *retType = codeGen.utilCodeGen->getLLVMType(method->getPrototype()->getReturnType());
                    if (retType->isIntegerTy()) {
                        codeGen.Builder->CreateRet(llvm::ConstantInt::get(retType, 0));
                    } else if (retType->isDoubleTy()) {
                        codeGen.Builder->CreateRet(llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0)));
                    } else {
                        codeGen.Builder->CreateRet(llvm::UndefValue::get(retType));
                    }
                }
            }
        } else {
            // For methods without body, just add a return
            if (method->getPrototype()->getReturnType().base == BaseType::Void) {
                codeGen.Builder->CreateRetVoid();
            } else {
                llvm::Type *retType = codeGen.utilCodeGen->getLLVMType(method->getPrototype()->getReturnType());
                if (retType->isIntegerTy()) {
                    codeGen.Builder->CreateRet(llvm::ConstantInt::get(retType, 0));
                } else if (retType->isDoubleTy()) {
                    codeGen.Builder->CreateRet(llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0)));
                } else {
                    codeGen.Builder->CreateRet(llvm::UndefValue::get(retType));
                }
            }
        }

        // D. VALIDATE
        // Ensure the block has a terminator (ret void/ret val)
        llvm::BasicBlock *CurrentMethodBlock = codeGen.Builder->GetInsertBlock();
        if (CurrentMethodBlock && !CurrentMethodBlock->getTerminator()) {
             if (method->getPrototype()->getReturnType().base == BaseType::Void) {
                 codeGen.Builder->CreateRetVoid();
             } else {
                 // Return default 0/null for safety if user forgot return
                 llvm::Type *retType = codeGen.utilCodeGen->getLLVMType(method->getPrototype()->getReturnType());
                 llvm::Value *defaultValue = llvm::Constant::getNullValue(retType);
                 codeGen.Builder->CreateRet(defaultValue);
             }
        }

        // Optional: LLVM sanity check
        llvm::verifyFunction(*TheFunction);
    }

    // 6. RESTORE CONTEXT
    // Put the builder back where it was (e.g., inside 'main') so subsequent code works.
    if (OldBlock) {
        codeGen.Builder->SetInsertPoint(OldBlock);
    } else {
        codeGen.Builder->ClearInsertionPoint();
    }

    // 4. Generate Constructor Bodies
    for (auto& constructor : S.getConstructors()) {
        codeGen.funcCodeGen->codegen(*constructor, S.getName());
    }
}

void TypeCodeGen::codegen(ClassDeclAST &C) {
    std::vector<llvm::Type*> fieldTypes;
    std::vector<FieldInfo> fieldInfos;

    size_t offset = 0;

    // Check inheritance logic
    if (C.hasParent()) {
         if (TypeRegistry::getInstance().hasStruct(C.getParentName())) {
             const StructInfo& parentInfo = TypeRegistry::getInstance().getStruct(C.getParentName());
             // Inherit Virtual Methods from Parent
             std::vector<std::string> parentVMethods = parentInfo.virtualMethods;
             if (!parentVMethods.empty()) {
                 // Add parent virtual methods to this class
                 // We need to access the virtual methods from the class directly
                 // Since we don't have direct access to the member, we'll need to copy them differently
                 // For now, we'll just use the virtual methods from the class
             }
         }
    }

    // Add hidden vptr field at index 0 if class has virtual methods or inherits from class with virtual methods
    bool needsVTable = !C.getVirtualMethods().empty() || (C.hasParent() && C.isOpen());

    if (needsVTable) {
        // vptr is a pointer to array of function pointers (i8**)
        llvm::Type* i8PtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(*codeGen.TheContext), 0);
        llvm::Type* vptrType = llvm::PointerType::get(i8PtrType, 0);
        fieldTypes.push_back(vptrType);

        FieldInfo vptrInfo;
        vptrInfo.name = "__vptr";
        vptrInfo.type = OType(BaseType::Void, 2); // void**
        vptrInfo.offset = offset;
        fieldInfos.push_back(vptrInfo);

        offset += 8; // vptr is 8 bytes
    }

    // Struct Prefixing: If this class has a parent, include parent fields
    if (C.hasParent()) {
        if (codeGen.StructTypes.find(C.getParentName()) == codeGen.StructTypes.end()) {
            codeGen.logError("Parent class not found");
            return;
        }

        if (TypeRegistry::getInstance().hasStruct(C.getParentName())) {
            const StructInfo& parentInfo = TypeRegistry::getInstance().getStruct(C.getParentName());

            // Copy parent fields (skip vptr if already added)
            for (const auto& parentField : parentInfo.fields) {
                if (needsVTable && parentField.name == "__vptr") continue; // Skip parent vptr

                llvm::Type* fieldType = codeGen.utilCodeGen->getLLVMType(parentField.type);
                fieldTypes.push_back(fieldType);

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
        llvm::Type* fieldType = codeGen.utilCodeGen->getLLVMType(field.second);
        if (!fieldType) return;

        fieldTypes.push_back(fieldType);

        FieldInfo info;
        info.name = field.first;
        info.type = field.second;
        info.offset = offset;
        fieldInfos.push_back(info);

        offset += TypeRegistry::getInstance().getTypeSize(field.second);
    }

    // Create LLVM struct type
    llvm::StructType* structType = llvm::StructType::create(*codeGen.TheContext, fieldTypes, C.getName());
    codeGen.StructTypes[C.getName()] = structType;

    // Register in type registry
    TypeRegistry::getInstance().registerStruct(C.getName(), fieldInfos, C.getVirtualMethods());

    std::cerr << "Generating method protos...\n";
    // 1. Generate Prototypes for Methods
    for (auto& method : C.getMethods()) {
        std::string originalName = method->getPrototype()->getName();
        std::string mangledName = C.getName() + "_" + originalName;
        method->getPrototype()->setName(mangledName);
        method->getPrototype()->injectThisParameter(C.getName());
        codeGen.funcCodeGen->codegen(*method->getPrototype()); // Declare function
    }

    // 2. Generate Prototypes for Constructors
    std::cerr << "Starting constructor loop...\n";
    for (const auto& constructor : C.getConstructors()) {
        std::cerr << "In loop\n";
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
        codeGen.funcCodeGen->codegen(proto); // Declare function
        std::cerr << "Generated constructor proto: " << funcName << "\n";
    }

    // 3. Generate VTable (needs method declarations)
    if (needsVTable) {
        generateVTable(C);
    }

    // 4. Generate Method Bodies
    for (auto& method : C.getMethods()) {
        codeGen.funcCodeGen->codegen(*method); // FunctionAST::codegen will lookup existing function
    }

    // 5. Generate Constructor Bodies
    for (auto& constructor : C.getConstructors()) {
        codeGen.funcCodeGen->codegen(*constructor, C.getName()); // Will lookup existing function
    }
}

void TypeCodeGen::generateVTable(ClassDeclAST &C) {
    // Create VTable as global array of function pointers
    std::vector<llvm::Constant*> vtableEntries;

    // For each virtual method, add function pointer to VTable
    for (const auto& virtualMethod : C.getVirtualMethods()) {
        std::string mangledName = C.getName() + "_" + virtualMethod;
        llvm::Function* func = codeGen.TheModule->getFunction(mangledName);

        if (func) {
            // Cast function to i8* for VTable storage
            llvm::Type* i8PtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(*codeGen.TheContext), 0);
            llvm::Constant* funcPtr = llvm::ConstantExpr::getBitCast(func, i8PtrType);
            vtableEntries.push_back(funcPtr);
        }
    }

    if (!vtableEntries.empty()) {
        // Create VTable type: array of i8*
        llvm::Type* i8PtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(*codeGen.TheContext), 0);
        llvm::ArrayType* vtableType = llvm::ArrayType::get(i8PtrType, vtableEntries.size());

        // Create VTable constant
        llvm::Constant* vtableInit = llvm::ConstantArray::get(vtableType, vtableEntries);

        // Create global VTable variable
        std::string vtableName = C.getName() + "_vtable";
        new llvm::GlobalVariable(*codeGen.TheModule, vtableType, true, llvm::GlobalValue::ExternalLinkage, vtableInit, vtableName);
    }
}

void TypeCodeGen::processDeferredInstantiations() {
    // 0. Ensure all external function prototypes are declared in the module
    // This fixes the "Lazy Declaration Instability" where functions imported but not used
    // in main() might be missing or incomplete when needed by deferred instantiations.
    std::cerr << "--- Restoring External Functions ---\n";
    for (const auto& entry : codeGen.GlobalFunctionProtos) {
        if (!codeGen.TheModule->getFunction(entry.first)) {
            std::cerr << "Restoring function declaration: " << entry.first << "\n";
            // Force creation of the function declaration in the current module
            if (entry.second) {
                codeGen.funcCodeGen->codegen(*entry.second);
            }
        }
    }
    std::cerr << "--- End Restoration ---\n";

    // Process in batches.
    // We swap the global queue into a local vector so that if 'instantiateStruct'
    // pushes new items, it pushes to the global queue (for the next iteration)
    // without invalidating the reference to the item we are currently processing.

    // --- SAVE CONTEXT ---
    // We must save the current insertion point because this function might be called
    // recursively or from within an expression codegen (e.g., NewExprAST).
    // Failing to restore it will leave the Builder in a detached state, causing
    // segfaults when the caller tries to continue generating code.
    llvm::BasicBlock *OldBlock = codeGen.Builder->GetInsertBlock();
    llvm::BasicBlock::iterator OldPoint;
    if (OldBlock) {
        OldPoint = codeGen.Builder->GetInsertPoint();
    }

    while (!codeGen.utilCodeGen->getInstantiationQueue().empty()) {
        // 1. Move all pending items to a local batch
        std::vector<PendingInstantiation> CurrentBatch;
        CurrentBatch.swap(codeGen.utilCodeGen->getInstantiationQueue());

        // 2. Process this batch
        for (auto& Item : CurrentBatch) {
            std::cerr << "Processing Deferred Item: " << Item.MangledName << "\n";

            // --- CLEAN SLATE ---
            codeGen.Builder->ClearInsertionPoint();

            // --- GENERATE BODIES ---
            // Generate bodies for methods of the instantiated struct
            for (auto& method : Item.AST->getMethods()) {
                std::string originalName = method->getPrototype()->getName();
                std::cerr << "  Generating method: " << originalName << "\n";
                std::string mangledName = Item.MangledName + "_" + originalName;

                llvm::Function *TheFunction = codeGen.TheModule->getFunction(mangledName);
                if (!TheFunction) {
                     std::cerr << "    Function not found: " << mangledName << "\n";
                     continue;
                }

                // Create Block
                llvm::BasicBlock *MethodBlock = llvm::BasicBlock::Create(*codeGen.TheContext, "entry", TheFunction);
                codeGen.Builder->SetInsertPoint(MethodBlock);

                // Setup Scope
                std::vector<ScopeLayer> oldScopeStack = codeGen.ScopeStack;
                std::vector<std::set<std::string>> oldImmutableVars = codeGen.ImmutableVars;

                codeGen.ScopeStack.clear();
                codeGen.ImmutableVars.clear();
                codeGen.ImmutableVars.push_back(std::set<std::string>()); // Global Scope

                codeGen.enterScope();

                // Register Args - ensure type information is preserved for generic instantiation
                const auto& ProtoArgs = method->getPrototype()->getArgs();

                // Process arguments in the same order as they appear in the function
                auto argIt = TheFunction->arg_begin();
                for (const auto& ArgPair : ProtoArgs) {
                    // Safety check
                    if (argIt == TheFunction->arg_end()) {
                        std::cerr << "Error: Mismatch between prototype args and function args in " << TheFunction->getName().str() << std::endl;
                        break;
                    }

                    // Set the argument name
                    argIt->setName(ArgPair.first);

                    // Create alloca for the argument
                    llvm::AllocaInst *Alloca = codeGen.createEntryBlockAlloca(TheFunction, ArgPair.first, argIt->getType());
                    if (!Alloca) {
                        std::cerr << "Error: Failed to create alloca for argument " << ArgPair.first << " in " << TheFunction->getName().str() << std::endl;
                        ++argIt;
                        continue;
                    }

                    // Store the argument value in the alloca
                    codeGen.Builder->CreateStore(&*argIt, Alloca);

                    // Register both the variable and its type
                    codeGen.addVariable(ArgPair.first, Alloca);
                    codeGen.addVariableType(ArgPair.first, ArgPair.second);

                    ++argIt;
                }

                // Gen Body
                if (method->getBody()) {
                    codeGen.exprCodeGen->codegen(*method->getBody());
                }

                // Return Void if needed
                if (!codeGen.Builder->GetInsertBlock()->getTerminator()) {
                    if (method->getPrototype()->getReturnType().base == BaseType::Void) {
                        codeGen.Builder->CreateRetVoid();
                    } else {
                        llvm::Type *retType = codeGen.utilCodeGen->getLLVMType(method->getPrototype()->getReturnType());
                        if (retType->isIntegerTy()) {
                            codeGen.Builder->CreateRet(llvm::ConstantInt::get(retType, 0));
                        } else if (retType->isDoubleTy()) {
                            codeGen.Builder->CreateRet(llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0)));
                        } else {
                            codeGen.Builder->CreateRet(llvm::UndefValue::get(retType));
                        }
                    }
                }

                codeGen.ScopeStack = oldScopeStack;
                codeGen.ImmutableVars = oldImmutableVars;
                llvm::verifyFunction(*TheFunction);
            }

            // --- GENERATE CONSTRUCTORS ---
            for (auto& constructor : Item.AST->getConstructors()) {
                // ... (existing constructor loop) ...
                std::cerr << "  Generating constructor for " << Item.MangledName << "\n";
                std::string funcName = Item.MangledName + "_new";
                // Re-mangle if needed (simple check)
                std::vector<OType> paramOTypes;
                for (const auto& param : constructor->getParams()) paramOTypes.push_back(param.second);
                if (!paramOTypes.empty()) funcName = codeGen.utilCodeGen->mangleGenericName(funcName, paramOTypes);

                llvm::Function* func = codeGen.TheModule->getFunction(funcName);
                if (!func) {
                    std::cerr << "    Constructor function not found: " << funcName << "\n";
                    continue;
                }

                llvm::BasicBlock* BB = llvm::BasicBlock::Create(*codeGen.TheContext, "entry", func);
                codeGen.Builder->SetInsertPoint(BB);

                std::vector<ScopeLayer> oldScopeStack = codeGen.ScopeStack;
                std::vector<std::set<std::string>> oldImmutableVars = codeGen.ImmutableVars;

                codeGen.ScopeStack.clear();
                codeGen.ImmutableVars.clear();
                codeGen.ImmutableVars.push_back(std::set<std::string>()); // Global Scope

                codeGen.enterScope();

                // Handle Args - preserve type information for generic instantiation
                auto argIt = func->arg_begin();
                std::vector<std::string> paramNames;
                paramNames.push_back("this");
                for (const auto& param : constructor->getParams()) paramNames.push_back(param.first);

                // Get the parameter types from the prototype
                std::vector<OType> paramTypes;
                paramTypes.push_back(OType(BaseType::Struct, 1, Item.MangledName)); // 'this' type
                for (const auto& param : constructor->getParams()) paramTypes.push_back(param.second);

                for (size_t j = 0; j < paramNames.size(); ++j, ++argIt) {
                    if (argIt == func->arg_end()) {
                        std::cerr << "Error: Mismatch between expected args and function args in constructor " << func->getName().str() << std::endl;
                        break;
                    }

                    argIt->setName(paramNames[j]);
                    llvm::AllocaInst* alloca = codeGen.createEntryBlockAlloca(func, paramNames[j], argIt->getType());
                    if (!alloca) {
                        std::cerr << "Error: Failed to create alloca for parameter " << paramNames[j] << " in " << func->getName().str() << std::endl;
                        continue;
                    }
                    codeGen.Builder->CreateStore(&*argIt, alloca);
                    codeGen.addVariable(paramNames[j], alloca);

                    if (j < paramTypes.size()) {
                        codeGen.addVariableType(paramNames[j], paramTypes[j]);
                    }
                }

                if (constructor->getBody()) {
                    codeGen.exprCodeGen->codegen(*constructor->getBody());
                }

                // VTable Init (Copy existing logic)
                if (TypeRegistry::getInstance().hasStruct(Item.MangledName)) {
                    const StructInfo& info = TypeRegistry::getInstance().getStruct(Item.MangledName);

                    bool hasVptr = !info.fields.empty() && info.fields[0].name == "__vptr";

                    if (hasVptr) {
                        // Load 'this' pointer
                        llvm::AllocaInst* thisAlloca = codeGen.getVariable("this");
                        if (thisAlloca) {
                            llvm::Value* thisPtr = codeGen.Builder->CreateLoad(thisAlloca->getAllocatedType(), thisAlloca, "thisptr");

                            // Get Global VTable
                            std::string vtableName = Item.MangledName + "_vtable";
                            llvm::GlobalVariable* vtableVar = codeGen.TheModule->getNamedGlobal(vtableName);

                            if (vtableVar) {
                                // Get address of __vptr field (index 0)
                                std::vector<llvm::Value*> indices;
                                indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0));
                                indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0)); // Field 0

                                llvm::Value* vptrAddr = codeGen.Builder->CreateInBoundsGEP(
                                    codeGen.StructTypes[Item.MangledName],
                                    thisPtr,
                                    indices,
                                    "vptr_addr"
                                );

                                // Store VTable address
                                llvm::Value* Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0);
                                llvm::Value* vtablePtr = codeGen.Builder->CreateInBoundsGEP(
                                    vtableVar->getValueType(),
                                    vtableVar,
                                    {Zero, Zero},
                                    "vtable_decay"
                                );

                                codeGen.Builder->CreateStore(vtablePtr, vptrAddr);
                            }
                        }
                    }
                }

                if (!codeGen.Builder->GetInsertBlock()->getTerminator()) {
                    codeGen.Builder->CreateRetVoid();
                }

                codeGen.ScopeStack = oldScopeStack;
                codeGen.ImmutableVars = oldImmutableVars;
                llvm::verifyFunction(*func);
            }
        }
    }

    // --- RESTORE CONTEXT ---
    if (OldBlock) {
        if (OldPoint != OldBlock->end()) {
             codeGen.Builder->SetInsertPoint(OldBlock, OldPoint);
        } else {
             codeGen.Builder->SetInsertPoint(OldBlock);
        }
    } else {
        codeGen.Builder->ClearInsertionPoint();
    }
}