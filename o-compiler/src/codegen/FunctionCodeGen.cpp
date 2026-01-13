#include "FunctionCodeGen.h"
#include "UtilityCodeGen.h"
#include "ExpressionCodeGen.h"

llvm::Function *FunctionCodeGen::codegen(PrototypeAST &P) {
    std::vector<llvm::Type *> LLVMArgs;
    for (auto &ArgPair : P.getArgs()) {
        LLVMArgs.push_back(codeGen.utilCodeGen->getLLVMType(ArgPair.second));
    }

    llvm::FunctionType *FT = llvm::FunctionType::get(
        codeGen.utilCodeGen->getLLVMType(P.getReturnType()), LLVMArgs, false);

    // Check if function already exists (could be an external declaration from forward reference)
    llvm::Function *F = codeGen.TheModule->getFunction(P.getName());
    if (!F) {
        // Function doesn't exist, create it
        F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, P.getName(), codeGen.TheModule);
    } else {
        // Function exists (likely as external declaration from forward reference)
        // Verify the function type matches
        if (F->getFunctionType() != FT) {
            codeGen.logError(("Function " + P.getName() + " has mismatched signature").c_str());
            return nullptr;
        }
    }

    codeGen.FunctionReturnTypes[P.getName()] = P.getReturnType();

    // Register the prototype in the global registry for lazy symbol resolution
    // This allows generic instantiations to find external function declarations
    if (codeGen.GlobalFunctionProtos.find(P.getName()) == codeGen.GlobalFunctionProtos.end()) {
        // We need to create a copy of the prototype to store in the registry
        // Using shared_ptr to allow sharing across parsers
        codeGen.GlobalFunctionProtos[P.getName()] = std::shared_ptr<PrototypeAST>(P.clone());
    }

    unsigned Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(P.getArgs()[Idx++].first);

    return F;
}

llvm::Function *FunctionCodeGen::codegen(FunctionAST &F) {
    // Enter codegen phase
    codeGen.utilCodeGen->enterCodegenPhase();

    std::string functionName = F.getPrototype()->getName();

    // Register the prototype in the global registry for external functions
    // This is critical for deferred generic instantiation to find external declarations
    // Make sure the prototype is in the global registry
    if (codeGen.GlobalFunctionProtos.find(functionName) == codeGen.GlobalFunctionProtos.end()) {
        // Create a copy of the prototype to store in the registry using shared_ptr
        codeGen.GlobalFunctionProtos[functionName] = std::shared_ptr<PrototypeAST>(F.getPrototype()->clone());
    }

    // Ensure the function is created in the module during import
    // This is critical for external function declarations to be available during instantiation
    llvm::Function *TheFunction = codeGen.TheModule->getFunction(functionName);
    if (!TheFunction) {
        // If the function doesn't exist in the module, create it from the prototype
        TheFunction = codegen(*F.getPrototype());
    }
    if (!TheFunction) {
        // Exit codegen phase before returning
        codeGen.utilCodeGen->exitCodegenPhase();
        return nullptr;
    }

    // For external function declarations (no body), just return the function
    if (!F.getBody()) {
        // Exit codegen phase before returning
        codeGen.utilCodeGen->exitCodegenPhase();
        return TheFunction;
    }

    // If we already have a valid insertion point in the right function, use it!
    // This happens during generic instantiation when StructDeclAST sets the insertion point
    llvm::BasicBlock *CurrentBlock = codeGen.Builder->GetInsertBlock();
    if (CurrentBlock && CurrentBlock->getParent() == TheFunction) {
        // We're already in the right function, just process the body
        codeGen.ScopeStack.clear();
        codeGen.ImmutableVars.clear(); // Clear immutable vars stack
        codeGen.ImmutableVars.push_back(std::set<std::string>()); // Initialize with global scope
        codeGen.enterScope(); // Function Scope

        // Register types from prototype
        const auto& ProtoArgs = F.getPrototype()->getArgs();
        for (const auto& ArgPair : ProtoArgs) {
            codeGen.addVariableType(ArgPair.first, ArgPair.second);
        }

        for (auto &Arg : TheFunction->args()) {
            llvm::AllocaInst *Alloca = codeGen.createEntryBlockAlloca(TheFunction, std::string(Arg.getName()), Arg.getType());
            codeGen.Builder->CreateStore(&Arg, Alloca);
            codeGen.addVariable(std::string(Arg.getName()), Alloca);
        }

        if (llvm::Value *RetVal = codeGen.exprCodeGen->codegen(*F.getBody())) {
            // Only insert return if the block is not terminated
            if (!codeGen.Builder->GetInsertBlock()->getTerminator()) {
                if (F.getPrototype()->getReturnType().base == BaseType::Void) {
                    codeGen.Builder->CreateRetVoid();
                } else {
                    codeGen.Builder->CreateRet(RetVal);
                }
            }
            llvm::verifyFunction(*TheFunction);
            // Exit codegen phase before returning
            codeGen.utilCodeGen->exitCodegenPhase();
            return TheFunction;
        } else {
            // Body->codegen() returned nullptr, check if we need to add a return for void functions
            if (!codeGen.Builder->GetInsertBlock()->getTerminator() && F.getPrototype()->getReturnType().base == BaseType::Void) {
                codeGen.Builder->CreateRetVoid();
            }
            llvm::verifyFunction(*TheFunction);
            // Exit codegen phase before returning
            codeGen.utilCodeGen->exitCodegenPhase();
            return TheFunction;
        }
    } else {
        // Standard function compilation - create new block
        llvm::BasicBlock *BB = llvm::BasicBlock::Create(*codeGen.TheContext, "entry", TheFunction);
        codeGen.Builder->SetInsertPoint(BB);

        codeGen.ScopeStack.clear();
        codeGen.ImmutableVars.clear(); // Clear immutable vars stack
        codeGen.ImmutableVars.push_back(std::set<std::string>()); // Initialize with global scope
        codeGen.enterScope(); // Function Scope

        // Register types from prototype
        const auto& ProtoArgs = F.getPrototype()->getArgs();
        for (const auto& ArgPair : ProtoArgs) {
            codeGen.addVariableType(ArgPair.first, ArgPair.second);
        }

        for (auto &Arg : TheFunction->args()) {
            llvm::AllocaInst *Alloca = codeGen.createEntryBlockAlloca(TheFunction, std::string(Arg.getName()), Arg.getType());
            codeGen.Builder->CreateStore(&Arg, Alloca);
            codeGen.addVariable(std::string(Arg.getName()), Alloca);
        }


        if (llvm::Value *RetVal = codeGen.exprCodeGen->codegen(*F.getBody())) {
            // Only insert return if the block is not terminated
            if (!codeGen.Builder->GetInsertBlock()->getTerminator()) {
                if (F.getPrototype()->getReturnType().base == BaseType::Void) {
                    codeGen.Builder->CreateRetVoid();
                } else {
                    codeGen.Builder->CreateRet(RetVal);
                }
            }
            llvm::verifyFunction(*TheFunction);
            // Exit codegen phase before returning
            codeGen.utilCodeGen->exitCodegenPhase();
            return TheFunction;
        } else {
            // Body->codegen() returned nullptr, check if we need to add a return for void functions
            if (!codeGen.Builder->GetInsertBlock()->getTerminator() && F.getPrototype()->getReturnType().base == BaseType::Void) {
                codeGen.Builder->CreateRetVoid();
            }
            llvm::verifyFunction(*TheFunction);
            // Exit codegen phase before returning
            codeGen.utilCodeGen->exitCodegenPhase();
            return TheFunction;
        }
    }

    TheFunction->eraseFromParent();
    // Exit codegen phase before returning
    codeGen.utilCodeGen->exitCodegenPhase();
    return nullptr;
}

llvm::Function *FunctionCodeGen::codegen(ConstructorAST &C, const std::string& structName) {
    // Create constructor function name: StructName_new[_ArgType...]
    std::string funcName = structName + "_new";

    // Mangle name if parameters exist (simple mangling)
    std::vector<OType> paramOTypes;
    for (const auto& param : C.getParams()) {
        paramOTypes.push_back(param.second);
    }

    if (!paramOTypes.empty()) {
        funcName = codeGen.utilCodeGen->mangleGenericName(funcName, paramOTypes);
    }

    // Params: this, args...
    std::vector<llvm::Type*> paramTypes;
    std::vector<std::string> paramNames;

    // 1. Add 'this' parameter
    if (codeGen.StructTypes.find(structName) == codeGen.StructTypes.end()) {
        codeGen.logError("Unknown struct type for constructor");
        return nullptr;
    }
    llvm::Type* structType = codeGen.StructTypes[structName];
    paramTypes.push_back(llvm::PointerType::get(structType, 0));
    paramNames.push_back("this");

    // 2. Add user parameters
    for (const auto& param : C.getParams()) {
        paramTypes.push_back(codeGen.utilCodeGen->getLLVMType(param.second));
        paramNames.push_back(param.first);
    }

    // Constructor returns void (it initializes the memory passed in 'this')
    llvm::FunctionType* funcType = llvm::FunctionType::get(llvm::Type::getVoidTy(*codeGen.TheContext), paramTypes, false);

    // Check if function already exists
    llvm::Function* func = codeGen.TheModule->getFunction(funcName);
    if (!func) {
        func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, funcName, codeGen.TheModule);
    }

    // Create basic block
    llvm::BasicBlock* BB = llvm::BasicBlock::Create(*codeGen.TheContext, "entry", func);
    codeGen.Builder->SetInsertPoint(BB);

    codeGen.enterScope();

    // Handle Arguments
    auto argIt = func->arg_begin();
    for (size_t i = 0; i < paramNames.size(); ++i, ++argIt) {
        argIt->setName(paramNames[i]);

        if (paramNames[i] == "this") {
            // Store 'this' pointer in an alloca so we can access it via load/store
            llvm::AllocaInst* alloca = codeGen.createEntryBlockAlloca(func, "this", argIt->getType());
            codeGen.Builder->CreateStore(&*argIt, alloca);

            codeGen.addVariable("this", alloca);
            codeGen.addVariableType("this", OType(BaseType::Struct, 1, structName));
        } else {
            // User arguments
            llvm::AllocaInst* alloca = codeGen.createEntryBlockAlloca(func, paramNames[i], argIt->getType());
            codeGen.Builder->CreateStore(&*argIt, alloca);

            codeGen.addVariable(paramNames[i], alloca);
            // We should register the type here if we had it easily available matching the param index
            // But for now, we rely on the parser having checked types?
            // Or we iterate Params again.
        }
    }

    if (C.getBody()) {
        codeGen.exprCodeGen->codegen(*C.getBody());
    }

    // Initialize VTable Pointer if it exists
    if (TypeRegistry::getInstance().hasStruct(structName)) {
        const StructInfo& info = TypeRegistry::getInstance().getStruct(structName);

        bool hasVptr = !info.fields.empty() && info.fields[0].name == "__vptr";

        if (hasVptr) {
            // Load 'this' pointer
            llvm::AllocaInst* thisAlloca = codeGen.getVariable("this");
            if (thisAlloca) {
                llvm::Value* thisPtr = codeGen.Builder->CreateLoad(thisAlloca->getAllocatedType(), thisAlloca, "thisptr");

                // Get Global VTable
                std::string vtableName = structName + "_vtable";
                llvm::GlobalVariable* vtableVar = codeGen.TheModule->getNamedGlobal(vtableName);

                if (vtableVar) {
                    // Get address of __vptr field (index 0)
                    std::vector<llvm::Value*> indices;
                    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0));
                    indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0)); // Field 0

                    llvm::Value* vptrAddr = codeGen.Builder->CreateInBoundsGEP(
                        codeGen.StructTypes[structName],
                        thisPtr,
                        indices,
                        "vptr_addr"
                    );

                    // Store VTable address
                    // VTable var is [N x i8*]*. Decay to i8**.
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

    codeGen.Builder->CreateRetVoid();

    codeGen.exitScope();

    return func;
}