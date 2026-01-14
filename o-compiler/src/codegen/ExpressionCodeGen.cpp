#include "ExpressionCodeGen.h"
#include "UtilityCodeGen.h"
#include <unordered_set>

static thread_local std::unordered_set<const ExprAST*> activeCodegen;

llvm::Value *ExpressionCodeGen::codegen(ExprAST &E) {
    // Enter codegen phase
    codeGen.utilCodeGen->enterCodegenPhase();

    if (!activeCodegen.insert(&E).second) {
        llvm::errs() << "FATAL: AST cycle detected during codegen\n";
        E.dump();
        abort();
    }

    llvm::Value *result = nullptr;

    // Dispatch to the appropriate expression codegen method based on dynamic type
    if (dynamic_cast<BoolExprAST*>(&E)) {
        result = codegen(static_cast<BoolExprAST&>(E));
    } else if (dynamic_cast<StringExprAST*>(&E)) {
        result = codegen(static_cast<StringExprAST&>(E));
    } else if (dynamic_cast<CastExprAST*>(&E)) {
        result = codegen(static_cast<CastExprAST&>(E));
    } else if (dynamic_cast<NumberExprAST*>(&E)) {
        result = codegen(static_cast<NumberExprAST&>(E));
    } else if (dynamic_cast<VariableExprAST*>(&E)) {
        result = codegen(static_cast<VariableExprAST&>(E));
    } else if (dynamic_cast<VarDeclExprAST*>(&E)) {
        result = codegen(static_cast<VarDeclExprAST&>(E));
    } else if (dynamic_cast<AssignmentExprAST*>(&E)) {
        result = codegen(static_cast<AssignmentExprAST&>(E));
    } else if (dynamic_cast<ReturnExprAST*>(&E)) {
        result = codegen(static_cast<ReturnExprAST&>(E));
    } else if (dynamic_cast<DeleteExprAST*>(&E)) {
        result = codegen(static_cast<DeleteExprAST&>(E));
    } else if (dynamic_cast<NegateExprAST*>(&E)) {
        result = codegen(static_cast<NegateExprAST&>(E));
    } else if (dynamic_cast<NotExprAST*>(&E)) {
        result = codegen(static_cast<NotExprAST&>(E));
    } else if (dynamic_cast<BinaryExprAST*>(&E)) {
        result = codegen(static_cast<BinaryExprAST&>(E));
    } else if (dynamic_cast<CallExprAST*>(&E)) {
        result = codegen(static_cast<CallExprAST&>(E));
    } else if (dynamic_cast<MethodCallExprAST*>(&E)) {
        result = codegen(static_cast<MethodCallExprAST&>(E));
    } else if (dynamic_cast<BlockExprAST*>(&E)) {
        result = codegen(static_cast<BlockExprAST&>(E));
    } else if (dynamic_cast<IfExprAST*>(&E)) {
        result = codegen(static_cast<IfExprAST&>(E));
    } else if (dynamic_cast<WhileExprAST*>(&E)) {
        result = codegen(static_cast<WhileExprAST&>(E));
    } else if (dynamic_cast<ForExprAST*>(&E)) {
        result = codegen(static_cast<ForExprAST&>(E));
    } else if (dynamic_cast<AddressOfExprAST*>(&E)) {
        result = codegen(static_cast<AddressOfExprAST&>(E));
    } else if (dynamic_cast<DerefExprAST*>(&E)) {
        result = codegen(static_cast<DerefExprAST&>(E));
    } else if (dynamic_cast<NewExprAST*>(&E)) {
        result = codegen(static_cast<NewExprAST&>(E));
    } else if (dynamic_cast<UnresolvedNewExprAST*>(&E)) {
        result = codegen(static_cast<UnresolvedNewExprAST&>(E));
    } else if (dynamic_cast<NewArrayExprAST*>(&E)) {
        result = codegen(static_cast<NewArrayExprAST&>(E));
    } else if (dynamic_cast<MatchExprAST*>(&E)) {
        result = codegen(static_cast<MatchExprAST&>(E));
    } else if (dynamic_cast<IndexExprAST*>(&E)) {
        result = codegen(static_cast<IndexExprAST&>(E));
    } else if (dynamic_cast<MemberAccessAST*>(&E)) {
        result = codegen(static_cast<MemberAccessAST&>(E));
    } else if (dynamic_cast<ArrayInitExprAST*>(&E)) {
        result = codegen(static_cast<ArrayInitExprAST&>(E));
    } else if (dynamic_cast<ArrayLiteralExprAST*>(&E)) {
        result = codegen(static_cast<ArrayLiteralExprAST&>(E));
    } else {
        codeGen.logError("Unknown expression type in codegen");
        result = nullptr;
    }

    activeCodegen.erase(&E);
    // Exit codegen phase
    codeGen.utilCodeGen->exitCodegenPhase();
    return result;
}

llvm::Value *ExpressionCodeGen::codegen(BoolExprAST &E) {
    return llvm::ConstantInt::get(*codeGen.TheContext, llvm::APInt(1, E.getVal() ? 1 : 0, false));
}

llvm::Value *ExpressionCodeGen::codegen(StringExprAST &E) {
    // Create global constant array for string
    std::string content = E.getVal() + '\0'; // Null-terminate
    llvm::Constant *strConstant = llvm::ConstantDataArray::getString(*codeGen.TheContext, content, false);

    // Create global variable
    llvm::GlobalVariable *globalStr = new llvm::GlobalVariable(
        *codeGen.TheModule,
        strConstant->getType(),
        true, // isConstant
        llvm::GlobalValue::PrivateLinkage,
        strConstant,
        "str"
    );

    // Return pointer to first character (i8*)
    llvm::Value *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0);
    return codeGen.Builder->CreateInBoundsGEP(
        strConstant->getType(),
        globalStr,
        {zero, zero},
        "strptr"
    );
}

llvm::Value *ExpressionCodeGen::codegen(CastExprAST &E) {
    llvm::Value *Val = codegen(*E.getOperand());
    if (!Val) return nullptr;

    OType SrcType = E.getOperand()->getOType();

    // Get LLVM Types
    llvm::Type *DestLLVMType = codeGen.utilCodeGen->getLLVMType(E.getTargetType());
    llvm::Type *SrcLLVMType = Val->getType();

    // 1. Same Type? No-op.
    if (SrcLLVMType == DestLLVMType) return Val;

    // 2. Int <-> Float
    if (SrcLLVMType->isIntegerTy() && DestLLVMType->isFloatingPointTy()) {
        return codeGen.Builder->CreateSIToFP(Val, DestLLVMType, "cast_si_fp");
    }
    if (SrcLLVMType->isFloatingPointTy() && DestLLVMType->isIntegerTy()) {
        return codeGen.Builder->CreateFPToSI(Val, DestLLVMType, "cast_fp_si");
    }

    // 3. Pointer <-> Pointer (Bitcast)
    if (SrcLLVMType->isPointerTy() && DestLLVMType->isPointerTy()) {
        // In Opaque Pointers, this is a no-op instruction, but necessary for semantic correctness
        return codeGen.Builder->CreateBitCast(Val, DestLLVMType, "cast_ptr");
    }

    // 4. Pointer <-> Int (Address Manipulation)
    if (SrcLLVMType->isPointerTy() && DestLLVMType->isIntegerTy()) {
        return codeGen.Builder->CreatePtrToInt(Val, DestLLVMType, "cast_ptr_int");
    }
    if (SrcLLVMType->isIntegerTy() && DestLLVMType->isPointerTy()) {
        return codeGen.Builder->CreateIntToPtr(Val, DestLLVMType, "cast_int_ptr");
    }

    // 5. Int <-> Int (Resize)
    if (SrcLLVMType->isIntegerTy() && DestLLVMType->isIntegerTy()) {
        if (SrcLLVMType->getIntegerBitWidth() < DestLLVMType->getIntegerBitWidth()) {
            return codeGen.Builder->CreateZExt(Val, DestLLVMType, "cast_zext"); // or SExt
        } else {
            return codeGen.Builder->CreateTrunc(Val, DestLLVMType, "cast_trunc");
        }
    }

    codeGen.logError("Unsupported cast operation");
    return nullptr;
}

llvm::Value *ExpressionCodeGen::codegen(NumberExprAST &E) {
    OType Type = E.getType();
    if (Type.base == BaseType::Float) {
        return llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(E.getVal()));
    } else if (Type.base == BaseType::Char || Type.base == BaseType::Byte) {
        return llvm::ConstantInt::get(*codeGen.TheContext, llvm::APInt(8, (int)E.getVal(), false));
    } else {
        return llvm::ConstantInt::get(*codeGen.TheContext, llvm::APInt(32, (int)E.getVal(), true));
    }
}

llvm::Value *ExpressionCodeGen::codegen(VariableExprAST &E) {
    llvm::AllocaInst *A = codeGen.getVariable(E.getName());
    if (!A) {
        codeGen.logError("Unknown variable name");
        return nullptr;
    }
    return codeGen.Builder->CreateLoad(A->getAllocatedType(), A, E.getName().c_str());
}

llvm::Value *ExpressionCodeGen::codegenAddress(VariableExprAST &E) {
    llvm::AllocaInst *A = codeGen.getVariable(E.getName());
    if (!A) {
        codeGen.logError("Unknown variable name");
        return nullptr;
    }
    return A; // Return the alloca directly (address) without loading
}

llvm::Value *ExpressionCodeGen::codegen(VarDeclExprAST &E) {
    llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();

    llvm::Type *AllocaType = nullptr;
    llvm::Value *InitVal = nullptr;

    if (E.getInit()) {
        InitVal = codegen(*E.getInit());
        if (!InitVal) return nullptr;
        AllocaType = InitVal->getType();
    }

    // If explicit type is provided, use it
    if (E.hasExplicitType()) {
        llvm::Type *ExpectedType = codeGen.utilCodeGen->getLLVMType(E.getExplicitType());
        if (InitVal && ExpectedType && ExpectedType != AllocaType) {
            // Check if we can implicitly cast
            if (InitVal->getType()->isPointerTy() && ExpectedType->isArrayTy()) {
                // This might be the ArrayInit returning pointer case?
                // But generally types should match
            } else {
                 codeGen.logError("Type mismatch in variable declaration");
                 return nullptr;
            }
        }
        if (ExpectedType) AllocaType = ExpectedType;
    }

    if (!AllocaType) {
        codeGen.logError("Cannot infer type for variable declaration");
        return nullptr;
    }

    llvm::AllocaInst *Alloca = codeGen.createEntryBlockAlloca(TheFunction, E.getName(), AllocaType);

    if (InitVal) {
        codeGen.Builder->CreateStore(InitVal, Alloca);
    } else {
        // Zero initialization for safety
        llvm::Value *Zero = llvm::Constant::getNullValue(AllocaType);
        codeGen.Builder->CreateStore(Zero, Alloca);
    }

    codeGen.addVariable(E.getName(), Alloca);

    OType VarType;
    if (E.hasExplicitType()) VarType = E.getExplicitType();
    else if (E.getInit()) {
        VarType = E.getInit()->getOType();

        // Special handling for NewExprAST with generic types
        // If this is a generic instantiation, update the type to reflect the instantiated type
        if (auto* newExpr = dynamic_cast<NewExprAST*>(E.getInit())) {
            if (!newExpr->getGenericArgs().empty() && !newExpr->getClassName().empty()) {
                // Check if this is a generic struct that needs instantiation
                if (codeGen.GenericStructRegistry.count(newExpr->getClassName()) > 0) {
                    // Trigger instantiation to ensure the instantiated type exists
                    llvm::Type* instantiatedType = codeGen.utilCodeGen->instantiateStruct(newExpr->getClassName(), newExpr->getGenericArgs());
                    if (instantiatedType) {
                        // Update the VarType to reflect the instantiated type name
                        // IMPORTANT: We must clear genericArgs because the structName is now the concrete mangled name
                        // Keeping genericArgs would cause "double mangling" (e.g., Box_int + <int> -> Box_int_int)
                        std::string instantiatedName = codeGen.utilCodeGen->mangleGenericName(newExpr->getClassName(), newExpr->getGenericArgs());
                        VarType = OType(BaseType::Struct, 1, instantiatedName, {}, {});
                    }
                }
            }
        }
    }

    codeGen.addVariableType(E.getName(), VarType);

    // RAII Registration: If type is Slice (Dynamic Array), register for cleanup
    if (VarType.isArray() && VarType.getArrayNumElements() == -1) {
        codeGen.registerCleanup(Alloca);
    }

    // Track immutable variables
    if (E.getIsConst() && !codeGen.ImmutableVars.empty()) {
        codeGen.ImmutableVars.back().insert(E.getName());
    }

    return Alloca; // Return the address
}

llvm::Value *ExpressionCodeGen::codegen(AssignmentExprAST &E) {
    llvm::Value *Val = codegen(*E.getRHS());
    if (!Val) return nullptr;

    llvm::Value *Ptr = codegenAddress(*E.getLHS());
    if (!Ptr) {
        codeGen.logError("Left-hand side of assignment must be an l-value (variable, array index, dereference)");
        return nullptr;
    }

    // Check if we're trying to assign to a constant variable
    // First, check if the left-hand side is a simple variable reference
    if (auto *VarExpr = dynamic_cast<VariableExprAST*>(E.getLHS())) {
        std::string VarName = VarExpr->getName();

        // Check all scopes from innermost to outermost for immutable variables
        for (auto it = codeGen.ImmutableVars.rbegin(); it != codeGen.ImmutableVars.rend(); ++it) {
            if (it->count(VarName)) {
                codeGen.logError(("Cannot reassign constant '" + VarName + "'").c_str());
                return nullptr;
            }
        }
    }

    // Check types and cast if needed (e.g. char to int promotion in some cases, or int to byte)
    // For now, assume types match or rely on LLVM to complain/cast implicitly if compatible

    codeGen.Builder->CreateStore(Val, Ptr);
    return Val;
}

llvm::Value *ExpressionCodeGen::codegen(ReturnExprAST &E) {
    llvm::Value *V = nullptr;
    if (E.getRetVal()) {
        V = codegen(*E.getRetVal());
        if (!V) return nullptr;
    }

    // RAII: Cleanup all scopes before returning
    // Iterate in reverse (innermost to outermost)
    for (auto it = codeGen.ScopeStack.rbegin(); it != codeGen.ScopeStack.rend(); ++it) {
        codeGen.emitCleanup(*it);
    }

    if (E.getRetVal()) {
        codeGen.Builder->CreateRet(V);
    } else {
        codeGen.Builder->CreateRetVoid();
        V = llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*codeGen.TheContext)); // Dummy
    }
    return V;
}

llvm::Value *ExpressionCodeGen::codegen(DeleteExprAST &E) {
    llvm::Value *Val = codegen(*E.getOperand());
    if (!Val) return nullptr;

    // If we are deleting a variable, we must remove it from the RAII cleanup list to avoid double-free
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(E.getOperand())) {
        llvm::AllocaInst* Alloca = codeGen.getVariable(VarExpr->getName());
        if (Alloca) {
            // Search from top of stack down
            for (auto it = codeGen.ScopeStack.rbegin(); it != codeGen.ScopeStack.rend(); ++it) {
                auto& cleanups = it->cleanupVars;
                auto found = std::find(cleanups.begin(), cleanups.end(), Alloca);
                if (found != cleanups.end()) {
                    cleanups.erase(found);
                    break; // Found and removed
                }
            }
        }
    }

    // Val is likely a pointer (or a Slice struct if it was a variable load)
    // Wait, VariableExprAST::codegen() loads the value.
    // If it's a slice (struct), we need the pointer inside.

    llvm::Value* PtrToDelete = Val;
    if (Val->getType()->isStructTy()) {
        // Assume Slice { size, ptr }
        if (Val->getType()->getStructNumElements() == 2) {
             PtrToDelete = codeGen.Builder->CreateExtractValue(Val, 1, "delete_ptr");
        }
    }

    if (!PtrToDelete->getType()->isPointerTy()) {
        codeGen.logError("Cannot delete non-pointer type");
        return nullptr;
    }

    // Cast to opaque ptr if needed (LLVM 18 uses ptr)
    if (PtrToDelete->getType() != llvm::PointerType::get(*codeGen.TheContext, 0)) {
        PtrToDelete = codeGen.Builder->CreateBitCast(PtrToDelete, llvm::PointerType::get(*codeGen.TheContext, 0), "delete_cast");
    }

    codeGen.Builder->CreateCall(codeGen.utilCodeGen->getFreeFunc(), {PtrToDelete});

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*codeGen.TheContext));
}

llvm::Value *ExpressionCodeGen::codegen(NegateExprAST &E) {
    llvm::Value *Val = codegen(*E.getOperand());
    if (!Val) return nullptr;

    if (Val->getType()->isDoubleTy()) {
        return codeGen.Builder->CreateFNeg(Val, "negtmp");
    } else if (Val->getType()->isIntegerTy()) {
        return codeGen.Builder->CreateNeg(Val, "negtmp");
    }

    codeGen.logError("Invalid type for negation");
    return nullptr;
}

llvm::Value *ExpressionCodeGen::codegen(NotExprAST &E) {
    llvm::Value *Val = codegen(*E.getOperand());
    if (!Val) return nullptr;

    if (Val->getType()->isIntegerTy(1)) {
        return codeGen.Builder->CreateNot(Val, "nottmp");
    }

    codeGen.logError("Invalid type for logical NOT (!). Expected boolean.");
    return nullptr;
}

llvm::Value *ExpressionCodeGen::codegen(BinaryExprAST &E) {
    // Handle short-circuiting logical operators
    if (E.getOp() == "&&" || E.getOp() == "||") {
        llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();

        llvm::BasicBlock *LHSBlock = codeGen.Builder->GetInsertBlock();
        llvm::BasicBlock *RHSBlock = llvm::BasicBlock::Create(*codeGen.TheContext, "rhs", TheFunction);
        llvm::BasicBlock *MergeBlock = llvm::BasicBlock::Create(*codeGen.TheContext, "merge", TheFunction);

        // Evaluate LHS
        llvm::Value *L = codegen(*E.getLHS());
        if (!L) return nullptr;

        // Convert to boolean if needed
        if (!L->getType()->isIntegerTy(1)) {
            if (L->getType()->isDoubleTy()) {
                L = codeGen.Builder->CreateFCmpONE(L, llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0)), "tobool");
            } else {
                L = codeGen.Builder->CreateICmpNE(L, llvm::ConstantInt::get(L->getType(), 0), "tobool");
            }
        }

        if (E.getOp() == "&&") {
            // For &&: if LHS is false, skip RHS
            codeGen.Builder->CreateCondBr(L, RHSBlock, MergeBlock);
        } else { // ||
            // For ||: if LHS is true, skip RHS
            codeGen.Builder->CreateCondBr(L, MergeBlock, RHSBlock);
        }

        // RHS Block
        codeGen.Builder->SetInsertPoint(RHSBlock);
        llvm::Value *R = codegen(*E.getRHS());
        if (!R) return nullptr;

        // Convert RHS to boolean if needed
        if (!R->getType()->isIntegerTy(1)) {
            if (R->getType()->isDoubleTy()) {
                R = codeGen.Builder->CreateFCmpONE(R, llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0)), "tobool");
            } else {
                R = codeGen.Builder->CreateICmpNE(R, llvm::ConstantInt::get(R->getType(), 0), "tobool");
            }
        }

        codeGen.Builder->CreateBr(MergeBlock);
        RHSBlock = codeGen.Builder->GetInsertBlock(); // Update in case RHS created new blocks

        // Merge Block
        codeGen.Builder->SetInsertPoint(MergeBlock);
        llvm::PHINode *PHI = codeGen.Builder->CreatePHI(llvm::Type::getInt1Ty(*codeGen.TheContext), 2, "logictmp");

        if (E.getOp() == "&&") {
            PHI->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*codeGen.TheContext), 0), LHSBlock); // false from LHS
            PHI->addIncoming(R, RHSBlock); // RHS result
        } else { // ||
            PHI->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*codeGen.TheContext), 1), LHSBlock); // true from LHS
            PHI->addIncoming(R, RHSBlock); // RHS result
        }

        return PHI;
    }

    // Regular binary operators
    llvm::Value *L = codegen(*E.getLHS());
    llvm::Value *R = codegen(*E.getRHS());
    if (!L || !R) return nullptr;

    // Check for Operator Overloading (Structs)
    OType LType = E.getLHS()->getOType();
    if (LType.base == BaseType::Struct) {
        std::string OpMethod;
        if (E.getOp() == "+") OpMethod = "op_add";
        else if (E.getOp() == "-") OpMethod = "op_sub";
        else if (E.getOp() == "*") OpMethod = "op_mul";
        else if (E.getOp() == "/") OpMethod = "op_div";
        else if (E.getOp() == "%") OpMethod = "op_mod";
        else if (E.getOp() == "==") OpMethod = "op_eq";
        else if (E.getOp() == "!=") OpMethod = "op_neq";
        else if (E.getOp() == "<") OpMethod = "op_lt";
        else if (E.getOp() == ">") OpMethod = "op_gt";
        else if (E.getOp() == "<=") OpMethod = "op_le";
        else if (E.getOp() == ">=") OpMethod = "op_ge";

        if (!OpMethod.empty()) {
            std::string StructName = LType.structName;
            if (!LType.genericArgs.empty()) {
                StructName = codeGen.utilCodeGen->mangleGenericName(StructName, LType.genericArgs);
            }
            std::string MangledName = StructName + "_" + OpMethod;

            llvm::Function *OpFunc = codeGen.TheModule->getFunction(MangledName);
            if (OpFunc) {
                // Prepare Args: L (this), R
                std::vector<llvm::Value*> ArgsV;
                ArgsV.push_back(L); // this
                ArgsV.push_back(R); // other

                return codeGen.Builder->CreateCall(OpFunc, ArgsV, "optmp");
            }
        }
    }

    // Pointer Arithmetic
    if (L->getType()->isPointerTy() && R->getType()->isIntegerTy()) {
        if (E.getOp() == "+") {
            // Assume byte-wise arithmetic for void* or byte*
            return codeGen.Builder->CreateInBoundsGEP(llvm::Type::getInt8Ty(*codeGen.TheContext), L, R, "ptradd");
        } else if (E.getOp() == "-") {
            llvm::Value *NegR = codeGen.Builder->CreateNeg(R);
            return codeGen.Builder->CreateInBoundsGEP(llvm::Type::getInt8Ty(*codeGen.TheContext), L, NegR, "ptrsub");
        }
    }

    // Handle type promotion: char/byte to int for arithmetic
    if (L->getType()->isIntegerTy(8) && R->getType()->isIntegerTy(32)) {
        L = codeGen.Builder->CreateZExt(L, llvm::Type::getInt32Ty(*codeGen.TheContext), "promote");
    } else if (L->getType()->isIntegerTy(32) && R->getType()->isIntegerTy(8)) {
        R = codeGen.Builder->CreateZExt(R, llvm::Type::getInt32Ty(*codeGen.TheContext), "promote");
    } else if (L->getType()->isIntegerTy(8) && R->getType()->isIntegerTy(8)) {
        L = codeGen.Builder->CreateZExt(L, llvm::Type::getInt32Ty(*codeGen.TheContext), "promote");
        R = codeGen.Builder->CreateZExt(R, llvm::Type::getInt32Ty(*codeGen.TheContext), "promote");
    }

    if (L->getType() != R->getType()) {
        codeGen.logError("Type mismatch in binary expression");
        return nullptr;
    }

    bool isFloat = L->getType()->isDoubleTy();

    if (E.getOp() == "+") {
        return isFloat ? codeGen.Builder->CreateFAdd(L, R, "addtmp") : codeGen.Builder->CreateAdd(L, R, "addtmp");
    } else if (E.getOp() == "-") {
        return isFloat ? codeGen.Builder->CreateFSub(L, R, "subtmp") : codeGen.Builder->CreateSub(L, R, "subtmp");
    } else if (E.getOp() == "*") {
        return isFloat ? codeGen.Builder->CreateFMul(L, R, "multmp") : codeGen.Builder->CreateMul(L, R, "multmp");
    } else if (E.getOp() == "/") {
        return isFloat ? codeGen.Builder->CreateFDiv(L, R, "divtmp") : codeGen.Builder->CreateSDiv(L, R, "divtmp");
    } else if (E.getOp() == "%") {
        return isFloat ? codeGen.Builder->CreateFRem(L, R, "remtmp") : codeGen.Builder->CreateSRem(L, R, "remtmp");
    } else if (E.getOp() == "<") {
        return isFloat ? codeGen.Builder->CreateFCmpOLT(L, R, "cmptmp") : codeGen.Builder->CreateICmpSLT(L, R, "cmptmp");
    } else if (E.getOp() == ">") {
        return isFloat ? codeGen.Builder->CreateFCmpOGT(L, R, "cmptmp") : codeGen.Builder->CreateICmpSGT(L, R, "cmptmp");
    } else if (E.getOp() == "<=") {
        return isFloat ? codeGen.Builder->CreateFCmpOLE(L, R, "cmptmp") : codeGen.Builder->CreateICmpSLE(L, R, "cmptmp");
    } else if (E.getOp() == ">=") {
        return isFloat ? codeGen.Builder->CreateFCmpOGE(L, R, "cmptmp") : codeGen.Builder->CreateICmpSGE(L, R, "cmptmp");
    } else if (E.getOp() == "==") {
        return isFloat ? codeGen.Builder->CreateFCmpOEQ(L, R, "cmptmp") : codeGen.Builder->CreateICmpEQ(L, R, "cmptmp");
    } else if (E.getOp() == "!=") {
        return isFloat ? codeGen.Builder->CreateFCmpONE(L, R, "cmptmp") : codeGen.Builder->CreateICmpNE(L, R, "cmptmp");
    }

    return nullptr;
}

llvm::Value *ExpressionCodeGen::codegen(CallExprAST &E) {
    //fprintf(stderr, "DEBUG: CallExprAST::codegen for %p\n", &E);
    //fprintf(stderr, "DEBUG: Callee: %s\n", E.getCallee().c_str());
    fflush(stderr);

    if (!codeGen.TheModule) {
        fprintf(stderr, "FATAL: codeGen.TheModule is null!\n");
        return nullptr;
    }

    llvm::Function *CalleeF = codeGen.TheModule->getFunction(E.getCallee());
    
    if (!CalleeF) {
        //fprintf(stderr, "DEBUG: Function not found in module, checking well-knowns\n");
        // Check if this is a well-known external C function that should be pre-declared
        if (E.getCallee() == "exit") {
            //fprintf(stderr, "DEBUG: Creating declaration for exit\n");
            // Declare exit function: void exit(int)
            std::vector<llvm::Type*> Args;
            Args.push_back(llvm::Type::getInt32Ty(*codeGen.TheContext));
            llvm::FunctionType *FT = llvm::FunctionType::get(
                llvm::Type::getVoidTy(*codeGen.TheContext), Args, false);
            CalleeF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "exit", codeGen.TheModule);
            //fprintf(stderr, "DEBUG: Created exit decl at %p\n", (void*)CalleeF);
        }
        else if (E.getCallee() == "malloc") {
            //fprintf(stderr, "DEBUG: Creating declaration for malloc\n");
            // Declare malloc function: void* malloc(size_t)
            std::vector<llvm::Type*> Args;
            Args.push_back(llvm::Type::getInt32Ty(*codeGen.TheContext)); // size_t as int for simplicity
            llvm::FunctionType *FT = llvm::FunctionType::get(
                llvm::PointerType::get(*codeGen.TheContext, 0), Args, false); // void*
            CalleeF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "malloc", codeGen.TheModule);
        }
        else if (E.getCallee() == "free") {
            //fprintf(stderr, "DEBUG: Creating declaration for free\n");
            // Declare free function: void free(void*)
            std::vector<llvm::Type*> Args;
            Args.push_back(llvm::PointerType::get(*codeGen.TheContext, 0)); // void*
            llvm::FunctionType *FT = llvm::FunctionType::get(
                llvm::Type::getVoidTy(*codeGen.TheContext), Args, false);
            CalleeF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "free", codeGen.TheModule);
        }
        // Add other well-known C functions as needed
        else {
            //fprintf(stderr, "DEBUG: Checking global registry for prototype\n");
            // Attempt to recover from missing function during generic instantiation
            // by looking up the prototype in the global registry
            CalleeF = codeGen.utilCodeGen->getFunctionFromPrototype(E.getCallee());
        }
    }

    // 3. --- SEGFAULT PREVENTION ---
    if (!CalleeF) {
        //fprintf(stderr, "DEBUG: Function still null after lookup attempts\n");
        // This stops the compiler from crashing when Builder->CreateCall(NULL) happens
        codeGen.logError(("Linker Error: Function '" + E.getCallee() + "' is declared but not found during instantiation.").c_str());
        return nullptr;
    }
    // ------------------------------

    /*
    fprintf(stderr, "DEBUG: CalleeF found: %p\n", (void*)CalleeF);
    llvm::errs() << "DEBUG: CalleeF Dump: ";
    CalleeF->print(llvm::errs());
    llvm::errs() << "\n";
    */

    // 4. --- INSERTION POINT CHECK ---
    // Make sure the builder has a valid insertion point before creating the call
    if (!codeGen.Builder->GetInsertBlock()) {
        //fprintf(stderr, "DEBUG: No insertion point!\n");
        codeGen.logError(("Call to function '" + E.getCallee() + "' cannot be generated: no insertion point available during instantiation.").c_str());
        return nullptr;
    }
    // -------------------------------
    // Additional null check for function
    if (!CalleeF || !CalleeF->getFunctionType()) {
        //fprintf(stderr, "DEBUG: Invalid function or function type\n");
        codeGen.logError(("Invalid function or function type for: " + E.getCallee()).c_str());
        return nullptr;
    }

    //fprintf(stderr, "DEBUG: Checking arg count. Expected: %u, Got: %zu\n", CalleeF->arg_size(), E.getArgs().size());
    if (CalleeF->arg_size() != E.getArgs().size()) {
        codeGen.logError("Incorrect # arguments passed");
        return nullptr;
    }

    std::vector<llvm::Value *> ArgsV;
    for (unsigned i = 0, e = E.getArgs().size(); i != e; ++i) {
        //fprintf(stderr, "DEBUG: Processing arg %u\n", i);
        llvm::Value *ArgVal = codegen(*E.getArgs()[i]);
        if (!ArgVal) {
            fprintf(stderr, "DEBUG: Arg codegen returned null\n");
            return nullptr;
        }

        //llvm::errs() << "DEBUG: ArgVal " << i << " Dump: ";
        ArgVal->print(llvm::errs());
        llvm::errs() << "\n";

        // Explicitly cast argument to match parameter type
        llvm::Type *ParamType = CalleeF->getArg(i)->getType();
        llvm::Type *ArgType = ArgVal->getType();

        //fprintf(stderr, "DEBUG: Arg Type: %p, Param Type: %p\n", ArgType, ParamType);

        if (ArgType != ParamType) {
            //fprintf(stderr, "DEBUG: Attempting cast\n");
            if (ParamType->isIntegerTy() && ArgType->isIntegerTy()) {
                // Integer Cast
                if (ArgType->getIntegerBitWidth() < ParamType->getIntegerBitWidth()) {
                    ArgVal = codeGen.Builder->CreateZExt(ArgVal, ParamType, "arg_zext");
                } else {
                    ArgVal = codeGen.Builder->CreateTrunc(ArgVal, ParamType, "arg_trunc");
                }
            } else if (ParamType->isPointerTy() && ArgType->isPointerTy()) {
                // Pointer Cast
                ArgVal = codeGen.Builder->CreateBitCast(ArgVal, ParamType, "arg_bitcast");
            } else if (ParamType->isFloatingPointTy() && ArgType->isIntegerTy()) {
                ArgVal = codeGen.Builder->CreateSIToFP(ArgVal, ParamType, "arg_sitofp");
            } else if (ParamType->isIntegerTy() && ArgType->isFloatingPointTy()) {
                ArgVal = codeGen.Builder->CreateFPToSI(ArgVal, ParamType, "arg_fptosi");
            }
            // Add implicit Slice conversion logic here if needed, but keeping it simple for now
        }

        // Implicit Cast: Fixed Array -> Slice
        // Param Type
        // Check if Param is Slice ({i32, T*})
        bool IsParamSlice = false;
        if (ParamType->isStructTy() && !ParamType->getStructName().starts_with("class.") && !ParamType->getStructName().starts_with("struct.")) {
            if (ParamType->getStructNumElements() == 2 &&
                ParamType->getStructElementType(0)->isIntegerTy(32) &&
                ParamType->getStructElementType(1)->isPointerTy()) {
                IsParamSlice = true;
            }
        }

        // Check if Arg is Fixed Array Value (Load from Alloca<[N x T]>) -> [N x T]
        // Wait, VariableExprAST loads the value. So ArgVal is [N x T].
        if (IsParamSlice && ArgType->isArrayTy()) {
             //fprintf(stderr, "DEBUG: Handling slice cast\n");
             // We need the address of the array to create the pointer.
             // But ArgVal is the *value* (loaded). We can't take address of value easily without Alloca.
             // However, VariableExprAST::codegen() does CreateLoad.
             // If we could get the address...

             // Trick: If we have the value, we can store it to a temp alloca, then GEP.
             // Or, better: CallExprAST should ask for address if needed? No, too complex refactor.

             // Store the array value to a temp stack slot to get an address
             llvm::BasicBlock *CurrentBlock = codeGen.Builder->GetInsertBlock();
             if (CurrentBlock) {
                 llvm::Function *TheFunction = CurrentBlock->getParent();
                 llvm::AllocaInst *TempAlloca = codeGen.createEntryBlockAlloca(TheFunction, "tmparray", ArgType);
                 codeGen.Builder->CreateStore(ArgVal, TempAlloca);

                 // Now decay: Get pointer to first element
                 uint64_t ArraySize = ArgType->getArrayNumElements();

                 llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0);
                 std::vector<llvm::Value*> Indices = {Zero, Zero};

                 llvm::Value *DecayedPtr = codeGen.Builder->CreateInBoundsGEP(
                     ArgType,
                     TempAlloca,
                     Indices,
                     "decayed_ptr"
                 );

                 // Create Slice
                 llvm::Value *Slice = llvm::UndefValue::get(ParamType);

                 llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), ArraySize);
                 Slice = codeGen.Builder->CreateInsertValue(Slice, SizeVal, 0, "cast_slice_len");

                 // Bitcast ptr if needed (e.g. if Slice expects i8* but we have i32*)
                 // Ideally types match T.
                 llvm::Value *TypedPtr = codeGen.Builder->CreateBitCast(DecayedPtr, ParamType->getStructElementType(1), "cast_slice_ptr");
                 Slice = codeGen.Builder->CreateInsertValue(Slice, TypedPtr, 1, "cast_slice_ptr");

                 ArgVal = Slice;
             } else {
                 // If there's no insertion block, we can't create the slice conversion
                 codeGen.logError("Cannot create slice conversion: no insertion point available during instantiation.");
                 return nullptr;
             }
        }

        ArgsV.push_back(ArgVal);
    }

    //fprintf(stderr, "DEBUG: Creating Call instruction. Builder=%p, Context=%p\n", (void*)codeGen.Builder, (void*)codeGen.TheContext);
    
    if (CalleeF->getParent() != codeGen.TheModule) {
        fprintf(stderr, "FATAL: CalleeF belongs to different module! CalleeF Module: %p, TheModule: %p\n", (void*)CalleeF->getParent(), (void*)codeGen.TheModule);
    } else {
        //fprintf(stderr, "DEBUG: CalleeF belongs to TheModule.\n");
    }

    if (codeGen.Builder->GetInsertBlock()->getParent() == nullptr) {
         fprintf(stderr, "FATAL: BasicBlock has no parent function!\n");
    } else {
         //fprintf(stderr, "DEBUG: Inserting into function: %s\n", codeGen.Builder->GetInsertBlock()->getParent()->getName().str().c_str());
    }

    // Attempt to verify the function we are building (might fail if incomplete, but worth a try)
    // llvm::verifyFunction(*codeGen.Builder->GetInsertBlock()->getParent(), &llvm::errs());
    
    std::string callName = "calltmp";
    if (CalleeF->getReturnType()->isVoidTy()) {
        callName = ""; // Void calls cannot have names
    }

    //fprintf(stderr, "DEBUG: About to call CreateCall. ArgsV size: %zu\n", ArgsV.size());
    fflush(stderr);

    llvm::CallInst* CI = codeGen.Builder->CreateCall(CalleeF->getFunctionType(), CalleeF, ArgsV, callName);
    //fprintf(stderr, "DEBUG: CreateCall returned %p\n", (void*)CI);
    return CI;
}

llvm::Value *ExpressionCodeGen::codegen(MethodCallExprAST &E) {
    // 1. Resolve Object
    OType ObjType = E.getObject()->getOType();
    std::string StructName = ObjType.structName;

    // Handle generic types - during code generation phase, we should not trigger new instantiations
    // Instead, we should expect that all necessary instantiations have been processed
    if (!ObjType.genericArgs.empty()) {
        StructName = codeGen.utilCodeGen->mangleGenericName(ObjType.structName, ObjType.genericArgs);
    } else if (StructName.empty()) {
        // Check if this might be a variable whose type we need to look up
        if (auto* varExpr = dynamic_cast<VariableExprAST*>(E.getObject())) {
            // Get the stored type for this variable
            OType storedType = codeGen.getVariableType(varExpr->getName());
            if (storedType.base == BaseType::Struct) {
                StructName = storedType.structName;
                if (!storedType.genericArgs.empty()) {
                    // During code generation phase, we should not trigger new instantiations
                    // Instead, we should expect that all necessary instantiations have been processed
                    StructName = codeGen.utilCodeGen->mangleGenericName(storedType.structName, storedType.genericArgs);
                }
            }
        }

        // If still empty, check if this is a static method call (struct name used as namespace)
        if (StructName.empty()) {
            if (auto* varExpr = dynamic_cast<VariableExprAST*>(E.getObject())) {
                std::string varName = varExpr->getName();

                // Check if this name corresponds to a struct type
                if (TypeRegistry::getInstance().hasStruct(varName)) {
                    StructName = varName;
                }
            }
        }

        if (StructName.empty()) {
            codeGen.logError("Method call on non-struct type");
            return nullptr;
        }
    }

    // Ensure the struct name is properly mangled for generic types
    if (!ObjType.genericArgs.empty()) {
        StructName = codeGen.utilCodeGen->mangleGenericName(ObjType.structName, ObjType.genericArgs);
    }

    std::string MangledName = StructName + "_" + E.getMethodName();
    llvm::Function *CalleeF = codeGen.TheModule->getFunction(MangledName);

    // If function not found, try to look it up in the global registry
    if (!CalleeF) {
        CalleeF = codeGen.utilCodeGen->getFunctionFromPrototype(MangledName);
    }

    // If still not found and we have generic args, we should have already processed them during instantiation phase
    if (!CalleeF && !ObjType.genericArgs.empty()) {
        // During code generation phase, we should not trigger new instantiations
        // Instead, we should expect that all necessary instantiations have been processed
        // Log an error if the function is still not found
        codeGen.logError(("Method not found after instantiation phase: " + MangledName).c_str());
        return nullptr;
    }

    // If still not found, try a workaround for the method name mangling issue
    // where method names might be incorrectly set to include return type info
    if (!CalleeF) {
        std::string originalMethodName = E.getMethodName();

        // Check if the method name looks like it includes type info (e.g., "int_get" instead of "get")
        // Try to extract the actual method name by removing potential type prefixes
        std::string correctedMethodName = originalMethodName;
        if (originalMethodName.length() > 4) { // At least "int_"
            // Check if it starts with a common type name followed by "_"
            if (originalMethodName.substr(0, 4) == "int_") {
                correctedMethodName = originalMethodName.substr(4); // Remove "int_"
            } else if (originalMethodName.substr(0, 5) == "bool_") {
                correctedMethodName = originalMethodName.substr(5); // Remove "bool_"
            } else if (originalMethodName.substr(0, 6) == "float_") {
                correctedMethodName = originalMethodName.substr(6); // Remove "float_"
            } else if (originalMethodName.substr(0, 5) == "char_") {
                correctedMethodName = originalMethodName.substr(5); // Remove "char_"
            } else if (originalMethodName.substr(0, 5) == "byte_") {
                correctedMethodName = originalMethodName.substr(5); // Remove "byte_"
            }
        }

        // If we corrected the method name, try looking up with the corrected name
        if (correctedMethodName != originalMethodName) {
            std::string correctedMangledName = StructName + "_" + correctedMethodName;
            CalleeF = codeGen.TheModule->getFunction(correctedMangledName);

            if (!CalleeF) {
                CalleeF = codeGen.utilCodeGen->getFunctionFromPrototype(correctedMangledName);
            }

            // If found with corrected name, use the corrected mangled name
            if (CalleeF) {
                MangledName = correctedMangledName;
            }
        }
    }

    // If still not found, try additional fallbacks for common name mangling issues
    if (!CalleeF) {
        // Try looking for common patterns where return type might have been prepended to method name
        std::vector<std::string> possibleMethodNames = {E.getMethodName()};

        // Add corrected versions of the method name
        std::string originalMethodName = E.getMethodName();
        if (originalMethodName.length() > 4) {
            std::string correctedName = originalMethodName;
            if (originalMethodName.substr(0, 4) == "int_") {
                correctedName = originalMethodName.substr(4);
            } else if (originalMethodName.substr(0, 5) == "bool_") {
                correctedName = originalMethodName.substr(5);
            } else if (originalMethodName.substr(0, 6) == "float_") {
                correctedName = originalMethodName.substr(6);
            } else if (originalMethodName.substr(0, 5) == "char_") {
                correctedName = originalMethodName.substr(5);
            } else if (originalMethodName.substr(0, 5) == "byte_") {
                correctedName = originalMethodName.substr(5);
            }

            if (correctedName != originalMethodName) {
                possibleMethodNames.push_back(correctedName);
            }
        }

        // Try each possible method name
        for (const auto& methodName : possibleMethodNames) {
            std::string alternativeMangledName = StructName + "_" + methodName;
            CalleeF = codeGen.TheModule->getFunction(alternativeMangledName);
            if (!CalleeF) {
                CalleeF = codeGen.utilCodeGen->getFunctionFromPrototype(alternativeMangledName);
            }
            if (CalleeF) {
                MangledName = alternativeMangledName;
                break;
            }
        }
    }

    if (!CalleeF) {
        std::string err = "Unknown method: " + MangledName;
        codeGen.logError(err.c_str());
        return nullptr;
    }

    // Additional safety check for function
    if (!CalleeF || !CalleeF->getFunctionType()) {
        std::string err = "Invalid method function: " + MangledName;
        codeGen.logError(err.c_str());
        return nullptr;
    }

    // Check if this is a static method call (no 'this' parameter)
    // Static methods would not have 'this' as the first parameter
    bool isStaticMethod = true;
    if (!CalleeF->arg_empty()) {
        // For instance methods, the first argument should be a pointer to the struct
        // In LLVM 18 with opaque pointers, we can't directly check the pointee type
        // So we'll use the naming convention: if the function name contains the struct name
        // and the first argument is a pointer, assume it's an instance method
        std::string funcName = CalleeF->getName().str();
        if (funcName.find(StructName + "_") == 0 && CalleeF->getArg(0)->getType()->isPointerTy()) {
            // This looks like an instance method (e.g., "IO_print_int" where first arg is IO*)
            // Check if the struct type exists and matches
            auto it = codeGen.StructTypes.find(StructName);
            if (it != codeGen.StructTypes.end()) {
                // For opaque pointers, we can't directly compare pointee types
                // So we'll assume if the naming matches and it's a pointer, it's an instance method
                isStaticMethod = false;
            }
        }
    }

    // 3. Prepare Arguments
    std::vector<llvm::Value *> ArgsV;

    llvm::Value *ThisPtr = nullptr;
    if (!isStaticMethod) {
        // 2. Prepare 'this' pointer for instance methods
        if (ObjType.base == BaseType::Void) {
             // This is a static invocation of an instance method (Struct.method())
             // Pass a NULL pointer as 'this'
             if (codeGen.StructTypes.find(StructName) != codeGen.StructTypes.end()) {
                 llvm::Type* structType = codeGen.StructTypes[StructName];
                 // Create opaque pointer to null
                 ThisPtr = llvm::ConstantPointerNull::get(llvm::PointerType::get(*codeGen.TheContext, 0));
             } else {
                 codeGen.logError(("Unknown struct type: " + StructName).c_str());
                 return nullptr;
             }
        } else if (ObjType.isPointer()) {
            ThisPtr = codegen(*E.getObject());
        } else {
            ThisPtr = codegenAddress(*E.getObject());
            if (!ThisPtr) {
                // R-value struct (returned from function, etc). Store to temp.
                llvm::Value *Val = codegen(*E.getObject());
                if (!Val) return nullptr;
                 llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();
                 llvm::AllocaInst *TempAlloca = codeGen.createEntryBlockAlloca(TheFunction, "tmp_this", Val->getType());
                 codeGen.Builder->CreateStore(Val, TempAlloca);
                 ThisPtr = TempAlloca;
            }
        }

        if (!ThisPtr) return nullptr;

        ArgsV.push_back(ThisPtr); // Inject 'this'
    }

    if (CalleeF->arg_size() != E.getArgs().size() + (isStaticMethod ? 0 : 1)) {
        codeGen.logError("Incorrect # arguments passed to method");
        return nullptr;
    }

    for (unsigned i = 0, e = E.getArgs().size(); i != e; ++i) {
        llvm::Value *ArgVal = codegen(*E.getArgs()[i]);
        if (!ArgVal) return nullptr;

        // Implicit Cast: Fixed Array -> Slice
        // Param Type (adjusted for static methods - no 'this' offset)
        unsigned paramIndex = i + (isStaticMethod ? 0 : 1); // Skip 'this' for instance methods
        llvm::Type *ParamType = CalleeF->getArg(paramIndex)->getType();
        llvm::Type *ArgType = ArgVal->getType();

        // Check if Param is Slice ({i32, T*})
        bool IsParamSlice = false;
        if (ParamType->isStructTy() && !ParamType->getStructName().starts_with("class.") && !ParamType->getStructName().starts_with("struct.")) {
            if (ParamType->getStructNumElements() == 2 &&
                ParamType->getStructElementType(0)->isIntegerTy(32) &&
                ParamType->getStructElementType(1)->isPointerTy()) {
                IsParamSlice = true;
            }
        }

        if (IsParamSlice && ArgType->isArrayTy()) {
             llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();
             llvm::AllocaInst *TempAlloca = codeGen.createEntryBlockAlloca(TheFunction, "tmparray", ArgType);
             codeGen.Builder->CreateStore(ArgVal, TempAlloca);

             uint64_t ArraySize = ArgType->getArrayNumElements();

             llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0);
             std::vector<llvm::Value*> Indices = {Zero, Zero};

             llvm::Value *DecayedPtr = codeGen.Builder->CreateInBoundsGEP(
                 ArgType,
                 TempAlloca,
                 Indices,
                 "decayed_ptr"
             );

             llvm::Value *Slice = llvm::UndefValue::get(ParamType);

             llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), ArraySize);
             Slice = codeGen.Builder->CreateInsertValue(Slice, SizeVal, 0, "cast_slice_len");

             llvm::Value *TypedPtr = codeGen.Builder->CreateBitCast(DecayedPtr, ParamType->getStructElementType(1), "cast_slice_ptr");
             Slice = codeGen.Builder->CreateInsertValue(Slice, TypedPtr, 1, "cast_slice_ptr");

             ArgVal = Slice;
        }

        ArgsV.push_back(ArgVal);
    }

    // 4. Dynamic Dispatch Check
    int virtualIndex = -1;
    if (TypeRegistry::getInstance().hasStruct(StructName)) {
        const StructInfo& info = TypeRegistry::getInstance().getStruct(StructName);
        for (size_t i = 0; i < info.virtualMethods.size(); ++i) {
            if (info.virtualMethods[i] == E.getMethodName()) {
                virtualIndex = i;
                break;
            }
        }
    }

    if (virtualIndex != -1) {
        // Dynamic Dispatch
        llvm::Value* Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0);

        // __vptr is at index 0
        // Safety check: ensure the struct type exists in StructTypes
        if (codeGen.StructTypes.find(StructName) == codeGen.StructTypes.end()) {
            codeGen.logError(("Struct type not found in StructTypes map for vtable access: " + StructName).c_str());
            return nullptr;
        }

        llvm::Type* structType = codeGen.StructTypes[StructName];
        if (!structType || !structType->isStructTy()) {
            codeGen.logError(("Invalid struct type for vtable access: " + StructName).c_str());
            return nullptr;
        }

        llvm::Value* vptrAddr = codeGen.Builder->CreateInBoundsGEP(
             structType,
             ThisPtr,
             {Zero, Zero},
             "vptr_addr"
        );

        llvm::Type* i8PtrType = llvm::PointerType::get(llvm::Type::getInt8Ty(*codeGen.TheContext), 0); // i8*
        llvm::Type* vtableType = llvm::PointerType::get(i8PtrType, 0); // i8**

        llvm::Value* vtable = codeGen.Builder->CreateLoad(vtableType, vptrAddr, "vtable");

        llvm::Value* funcPtrAddr = codeGen.Builder->CreateInBoundsGEP(
            i8PtrType,
            vtable,
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), virtualIndex),
            "func_ptr_addr"
        );

        llvm::Value* funcVoidPtr = codeGen.Builder->CreateLoad(i8PtrType, funcPtrAddr, "func_void_ptr");

        llvm::FunctionType* funcType = CalleeF->getFunctionType();
        llvm::Value* funcPtr = codeGen.Builder->CreateBitCast(funcVoidPtr, llvm::PointerType::get(funcType, 0), "func_ptr");

        return codeGen.Builder->CreateCall(funcType, funcPtr, ArgsV, "vcalltmp");
    }

    return codeGen.Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Value *ExpressionCodeGen::codegen(BlockExprAST &E) {
    //fprintf(stderr, "DEBUG: BlockExprAST::codegen for %p\n", &E);
    codeGen.enterScope();
    llvm::Value *LastVal = nullptr;
    for (auto &Expr : E.getExpressions()) {
        //fprintf(stderr, "DEBUG: BlockExprAST iterating expr %p\n", Expr.get());
        if (!Expr) {
            //fprintf(stderr, "DEBUG: BlockExprAST found null expr!\n");
            continue;
        }
        LastVal = codegen(*Expr);
        if (!LastVal) {
             //fprintf(stderr, "DEBUG: BlockExprAST codegen failed for expr %p\n", Expr.get());
             codeGen.exitScope();
             return nullptr;
        }

        // If the block is already terminated (e.g. by return), stop emitting
        if (codeGen.Builder->GetInsertBlock()->getTerminator()) {
             break;
        }
    }
    codeGen.exitScope();
    return LastVal;
}

llvm::Value *ExpressionCodeGen::codegen(IfExprAST &E) {
    //fprintf(stderr, "DEBUG: IfExprAST::codegen for %p\n", &E);
    //fprintf(stderr, "DEBUG: IfExprAST Cond=%p, Then=%p, Else=%p\n", E.getCond(), E.getThen(), E.getElse());
    
    llvm::Value *CondV = codegen(*E.getCond());
    if (!CondV) return nullptr;

    if (CondV->getType()->isDoubleTy()) {
        CondV = codeGen.Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0)), "ifcond");
    } else if (CondV->getType()->isIntegerTy()) {
         CondV = codeGen.Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(CondV->getType(), 0), "ifcond");
    }

    llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*codeGen.TheContext, "then", TheFunction);
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*codeGen.TheContext, "else");
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*codeGen.TheContext, "ifcont");

    codeGen.Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    // Then
    codeGen.Builder->SetInsertPoint(ThenBB);
    llvm::Value *ThenV = codegen(*E.getThen());
    if (!ThenV) return nullptr;

    //fprintf(stderr, "DEBUG: IfExprAST Then block generated. ThenV=%p\n", (void*)ThenV);
    if (ThenV->getType()->isVoidTy()) {
        //fprintf(stderr, "DEBUG: ThenV is Void type\n");
    } else {
        //fprintf(stderr, "DEBUG: ThenV is NOT Void type\n");
    }
    fflush(stderr);

    bool ThenTerminated = (codeGen.Builder->GetInsertBlock()->getTerminator() != nullptr);
    //fprintf(stderr, "DEBUG: ThenTerminated=%d\n", ThenTerminated);
    fflush(stderr);

    if (!ThenTerminated) {
        //fprintf(stderr, "DEBUG: Creating Branch to MergeBB\n");
        codeGen.Builder->CreateBr(MergeBB);
    }
    ThenBB = codeGen.Builder->GetInsertBlock();

    // Else
    TheFunction->insert(TheFunction->end(), ElseBB);
    codeGen.Builder->SetInsertPoint(ElseBB);
    llvm::Value *ElseV = nullptr;
    if (E.getElse()) {
        ElseV = codegen(*E.getElse());
        if (!ElseV) return nullptr;
    } else {
        //fprintf(stderr, "DEBUG: Else branch (implicit). ThenV type is void: %d\n", ThenV->getType()->isVoidTy());
        fflush(stderr);
        if (ThenV->getType()->isVoidTy()) {
             //fprintf(stderr, "DEBUG: AVOIDED CRASH: skipping getNullValue(VoidTy)\n");
             fflush(stderr);
             ElseV = nullptr; // Won't be used anyway
        } else {
             ElseV = llvm::Constant::getNullValue(ThenV->getType());
        }
    }

    bool ElseTerminated = (codeGen.Builder->GetInsertBlock()->getTerminator() != nullptr);
    if (!ElseTerminated) codeGen.Builder->CreateBr(MergeBB);
    ElseBB = codeGen.Builder->GetInsertBlock();

    // Merge
    TheFunction->insert(TheFunction->end(), MergeBB);
    codeGen.Builder->SetInsertPoint(MergeBB);

    // Check if both branches are terminated (both return/exit)
    if (ThenTerminated && ElseTerminated) {
        // Both branches return, so MergeBB is unreachable.
        // We can create a dummy instruction or Unreachable.
        codeGen.Builder->CreateUnreachable();
        return llvm::Constant::getNullValue(ThenV->getType());
    }

    // FIX: Handle VOID types (prevent PHI creation for void)
    if (ThenV->getType()->isVoidTy()) {
        // If the type is void, we can't create a PHI node, so just return a dummy value
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*codeGen.TheContext)); // Return dummy void value
    }

    // Only create PHI if type is NOT void
    llvm::PHINode *PN = codeGen.Builder->CreatePHI(ThenV->getType(), 2, "iftmp");
    if (!ThenTerminated) PN->addIncoming(ThenV, ThenBB);

    // Handle ElseV generation carefully
    llvm::Value *FinalElseV = ElseV;
    if (!E.getElse()) {
         // If no else provided, we need a default value.
         //fprintf(stderr, "DEBUG: IfExprAST has no Else block. ThenV Type is Void: %d\n", ThenV->getType()->isVoidTy());
         fflush(stderr);

         // For non-void types, this is actually semantic error (missing else in expression), but to prevent crash:
         if (ThenV->getType()->isVoidTy()) {
             //fprintf(stderr, "DEBUG: ThenV is void. About to call getNullValue(VoidTy) which might crash!\n");
             fflush(stderr);
         }
         FinalElseV = llvm::Constant::getNullValue(ThenV->getType());
         //fprintf(stderr, "DEBUG: getNullValue returned %p\n", (void*)FinalElseV);
    }

    if (!ElseTerminated) PN->addIncoming(FinalElseV, ElseBB);

    return PN;
}

llvm::Value *ExpressionCodeGen::codegen(WhileExprAST &E) {
    llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *LoopCondBB = llvm::BasicBlock::Create(*codeGen.TheContext, "loopcond", TheFunction);
    llvm::BasicBlock *LoopBodyBB = llvm::BasicBlock::Create(*codeGen.TheContext, "loopbody");
    llvm::BasicBlock *LoopEndBB = llvm::BasicBlock::Create(*codeGen.TheContext, "loopend");

    codeGen.Builder->CreateBr(LoopCondBB);

    codeGen.Builder->SetInsertPoint(LoopCondBB);
    llvm::Value *CondV = codegen(*E.getCond());
    if (!CondV) return nullptr;

    // Convert to bool (i1) if needed
    if (CondV->getType()->isDoubleTy()) {
        CondV = codeGen.Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0)), "loopcond");
    } else if (CondV->getType()->isIntegerTy() && !CondV->getType()->isIntegerTy(1)) {
        CondV = codeGen.Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(CondV->getType(), 0), "loopcond");
    }

    codeGen.Builder->CreateCondBr(CondV, LoopBodyBB, LoopEndBB);

    TheFunction->insert(TheFunction->end(), LoopBodyBB);
    codeGen.Builder->SetInsertPoint(LoopBodyBB);

    if (!codegen(*E.getBody())) return nullptr;

    // Jump back to start
    if (!codeGen.Builder->GetInsertBlock()->getTerminator())
        codeGen.Builder->CreateBr(LoopCondBB);

    TheFunction->insert(TheFunction->end(), LoopEndBB);
    codeGen.Builder->SetInsertPoint(LoopEndBB);

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*codeGen.TheContext));
}

llvm::Value *ExpressionCodeGen::codegen(ForExprAST &E) {
    llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();

    codeGen.enterScope(); // Scope for Init variables

    // 1. Emit Init
    if (E.getInit()) {
        if (!codegen(*E.getInit())) {
            codeGen.exitScope();
            return nullptr;
        }
    }

    // Prepare Blocks
    llvm::BasicBlock *LoopCondBB = llvm::BasicBlock::Create(*codeGen.TheContext, "loopcond", TheFunction);
    llvm::BasicBlock *LoopBodyBB = llvm::BasicBlock::Create(*codeGen.TheContext, "loopbody");
    llvm::BasicBlock *LoopEndBB = llvm::BasicBlock::Create(*codeGen.TheContext, "looplatch");
    llvm::BasicBlock *AfterLoopBB = llvm::BasicBlock::Create(*codeGen.TheContext, "afterloop");

    // Jump to Condition
    codeGen.Builder->CreateBr(LoopCondBB);

    // 2. Loop Condition
    codeGen.Builder->SetInsertPoint(LoopCondBB);
    llvm::Value *CondV = nullptr;
    if (E.getCond()) {
        CondV = codegen(*E.getCond());
        if (!CondV) {
            codeGen.exitScope();
            return nullptr;
        }

        // Convert to bool
        if (CondV->getType()->isDoubleTy()) {
            CondV = codeGen.Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0)), "loopcond");
        } else if (CondV->getType()->isIntegerTy() && !CondV->getType()->isIntegerTy(1)) {
            CondV = codeGen.Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(CondV->getType(), 0), "loopcond");
        }
    } else {
        // Infinite loop
        CondV = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*codeGen.TheContext), 1);
    }

    codeGen.Builder->CreateCondBr(CondV, LoopBodyBB, AfterLoopBB);

    // 3. Loop Body
    TheFunction->insert(TheFunction->end(), LoopBodyBB);
    codeGen.Builder->SetInsertPoint(LoopBodyBB);

    // Note: Body (BlockExprAST) will create its own inner scope.
    if (!codegen(*E.getBody())) {
        codeGen.exitScope();
        return nullptr;
    }

    // Jump to Latch (Step) if not terminated
    if (!codeGen.Builder->GetInsertBlock()->getTerminator())
        codeGen.Builder->CreateBr(LoopEndBB);

    // 4. Latch (Step)
    TheFunction->insert(TheFunction->end(), LoopEndBB);
    codeGen.Builder->SetInsertPoint(LoopEndBB);

    if (E.getStep()) {
        if (!codegen(*E.getStep())) {
            codeGen.exitScope();
            return nullptr;
        }
    }

    // Back to Cond
    codeGen.Builder->CreateBr(LoopCondBB);

    // 5. After Loop
    TheFunction->insert(TheFunction->end(), AfterLoopBB);
    codeGen.Builder->SetInsertPoint(AfterLoopBB);

    codeGen.exitScope(); // Exit Init scope

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*codeGen.TheContext));
}

llvm::Value *ExpressionCodeGen::codegen(AddressOfExprAST &E) {
    // Try to cast operand to VariableExprAST to get address
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(E.getOperand())) {
        return codegenAddress(*VarExpr);
    }

    // Support address of array elements
    if (auto IndexExpr = dynamic_cast<IndexExprAST*>(E.getOperand())) {
        return codegenAddress(*IndexExpr);
    }

    // For now, only support address-of variables and array elements
    codeGen.logError("Address-of operator only supports variables and array elements currently");
    return nullptr;
}

llvm::Value *ExpressionCodeGen::codegen(DerefExprAST &E) {
    // RHS (Reading): Generate a load instruction on the pointer address
    llvm::Value *PtrValue = codegen(*E.getOperand());
    if (!PtrValue) return nullptr;

    // Load the value that the pointer points to
    if (!PtrValue->getType()->isPointerTy()) {
        codeGen.logError("Cannot dereference non-pointer type");
        return nullptr;
    }

    // Resolve pointee type
    OType PtrType = E.getOperand()->getOType();
    OType PointeeType = PtrType.getPointeeType();
    llvm::Type *LLVMType = codeGen.utilCodeGen->getLLVMType(PointeeType);

    return codeGen.Builder->CreateLoad(LLVMType, PtrValue, "deref");
}

llvm::Value *ExpressionCodeGen::codegenAddress(DerefExprAST &E) {
    // LHS (Assignment): Return the pointer address for storing
    llvm::Value *PtrValue = codegen(*E.getOperand());
    if (!PtrValue) return nullptr;

    if (!PtrValue->getType()->isPointerTy()) {
        codeGen.logError("Cannot dereference non-pointer type");
        return nullptr;
    }

    return PtrValue; // Return the pointer itself for assignment target
}

llvm::Value *ExpressionCodeGen::codegen(UnresolvedNewExprAST &E) {
    // During import phase, unresolved expressions should not be codegen'd
    // This indicates a problem with the three-phase architecture
    codeGen.logError("UnresolvedNewExprAST should not reach codegen phase - semantic resolution incomplete");
    return nullptr;
}

llvm::Value *ExpressionCodeGen::codegen(NewExprAST &E) {
    fprintf(stderr, "DEBUG: Entering NewExprAST::codegen for class '%s'\n", E.getClassName().c_str());
    std::string LookupName = E.getClassName();

    // Handle Generics - only instantiate if the struct is actually generic
    if (!E.getGenericArgs().empty()) {
        fprintf(stderr, "DEBUG: NewExprAST has generic args, checking if struct is generic\n");
        // Check if this is a generic struct first
        if (codeGen.GenericStructRegistry.count(E.getClassName()) > 0) {
            // Trigger instantiation (type and prototypes only)
            llvm::Type* instantiatedType = codeGen.utilCodeGen->instantiateStruct(E.getClassName(), E.getGenericArgs());
            LookupName = codeGen.utilCodeGen->mangleGenericName(E.getClassName(), E.getGenericArgs());
            fprintf(stderr, "DEBUG: Instantiated generic struct, new name: %s\n", LookupName.c_str());
        } else {
            // If not a generic struct but has generic args, this is an error
            codeGen.logError(("Trying to instantiate non-generic struct with generic arguments: " + E.getClassName()).c_str());
            return nullptr;
        }
    }

    // Check if the class/struct exists
    fprintf(stderr, "DEBUG: Looking for struct type '%s' in StructTypes map\n", LookupName.c_str());
    if (codeGen.StructTypes.find(LookupName) == codeGen.StructTypes.end()) {
        codeGen.logError(("Unknown class/struct name in 'new' expression: " + LookupName).c_str());
        return nullptr;
    }

    llvm::StructType *StructType = codeGen.StructTypes[LookupName];
    fprintf(stderr, "DEBUG: Retrieved struct type: %p\n", StructType);

    // Check if StructType is valid
    if (!StructType) {
        codeGen.logError(("Invalid struct type for: " + LookupName).c_str());
        return nullptr;
    }

    // 1. Heap Allocation (malloc)
    llvm::Function *MallocF = codeGen.TheModule->getFunction("malloc");
    if (!MallocF) {
        fprintf(stderr, "DEBUG: Malloc function not found, creating declaration\n");
        std::vector<llvm::Type*> Args;
        Args.push_back(llvm::Type::getInt64Ty(*codeGen.TheContext)); // size_t
        llvm::FunctionType *FT = llvm::FunctionType::get(
            llvm::PointerType::get(llvm::Type::getInt8Ty(*codeGen.TheContext), 0), Args, false);
        MallocF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "malloc", codeGen.TheModule);
        fprintf(stderr, "DEBUG: Created malloc function: %p\n", MallocF);
    } else {
        fprintf(stderr, "DEBUG: Found existing malloc function: %p\n", MallocF);
    }

    // Calculate size
    size_t size = 0;
    if (TypeRegistry::getInstance().hasStruct(LookupName)) {
        size = TypeRegistry::getInstance().getStruct(LookupName).totalSize;
        fprintf(stderr, "DEBUG: Calculated struct size: %zu\n", size);
    }
    if (size == 0) {
        size = 1; // Minimum allocation
        fprintf(stderr, "DEBUG: Using minimum allocation size: %zu\n", size);
    }

    llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*codeGen.TheContext), size);
    
    // Convert SizeVal to match malloc's argument type
    llvm::Value *MallocArg = SizeVal;
    if (MallocF->arg_size() > 0) {
        llvm::Type *MallocArgType = MallocF->getArg(0)->getType();
        if (MallocArgType != MallocArg->getType()) {
             MallocArg = codeGen.Builder->CreateZExtOrTrunc(MallocArg, MallocArgType, "malloc_size_cast");
        }
    }
    
    llvm::Value *VoidPtr = codeGen.Builder->CreateCall(MallocF, {MallocArg}, "mallocptr");

    // Cast to Struct*
    llvm::Value *ObjPtr = codeGen.Builder->CreateBitCast(VoidPtr, llvm::PointerType::get(StructType, 0), "objptr");

    // 2. Call Constructor: LookupName_new(this, args...)
    std::string ConstructorName = LookupName + "_new";

    // Attempt mangled name resolution if args exist
    std::vector<OType> argTypes;
    for (auto &Arg : E.getArgs()) {
        argTypes.push_back(Arg->getOType());
    }

    std::string MangledName = ConstructorName;
    if (!argTypes.empty()) {
        // Use the same mangling logic as in UtilityCodeGen.cpp for constructor names
        MangledName = codeGen.utilCodeGen->mangleGenericName(ConstructorName, argTypes);
    }

    llvm::Function *Constructor = codeGen.TheModule->getFunction(MangledName);
    fprintf(stderr, "DEBUG: Looking for constructor function: %s, found: %p\n", MangledName.c_str(), Constructor);

    // Fallback to base name if mangled not found (backward compatibility or void args)
    if (!Constructor) {
        Constructor = codeGen.TheModule->getFunction(ConstructorName);
        fprintf(stderr, "DEBUG: Fallback to base name: %s, found: %p\n", ConstructorName.c_str(), Constructor);
    }

    // Try to look up in global registry if still not found
    if (!Constructor) {
        Constructor = codeGen.utilCodeGen->getFunctionFromPrototype(MangledName);
        fprintf(stderr, "DEBUG: Looking in global registry for mangled: %s, found: %p\n", MangledName.c_str(), Constructor);
    }

    // Also try base name in global registry
    if (!Constructor) {
        Constructor = codeGen.utilCodeGen->getFunctionFromPrototype(ConstructorName);
        fprintf(stderr, "DEBUG: Looking in global registry for base: %s, found: %p\n", ConstructorName.c_str(), Constructor);
    }

    // CRITICAL FIX: If constructor is still not found, trigger instantiation request
    // This is especially important when called from within struct methods during instantiation
    if (!Constructor) {
        fprintf(stderr, "DEBUG: Constructor not found, checking for generic instantiation\n");
        // If this is a generic struct that should have been instantiated but wasn't,
        // trigger instantiation (but do not process queue recursively)
        if (!E.getGenericArgs().empty() && codeGen.GenericStructRegistry.count(E.getClassName()) > 0) {
            // This creates the Struct Type and the Constructor PROTOTYPE
            llvm::Type* instantiatedType = codeGen.utilCodeGen->instantiateStruct(E.getClassName(), E.getGenericArgs());

            // Look again for the constructor after instantiation (the prototype should exist now)
            Constructor = codeGen.TheModule->getFunction(MangledName);
            if (!Constructor) {
                Constructor = codeGen.TheModule->getFunction(ConstructorName);
            }

            // If still not found, check mangling (do not call processDeferredInstantiations here!)
            if (!Constructor) {
                 Constructor = codeGen.TheModule->getFunction(ConstructorName);
            }
            fprintf(stderr, "DEBUG: After instantiation, constructor found: %p\n", Constructor);
        } else {
            // For non-generic structs, try to trigger any pending instantiations that might define this constructor
            codeGen.processDeferredInstantiations();
            // Try one more time after processing instantiations
            Constructor = codeGen.TheModule->getFunction(MangledName);
            if (!Constructor) {
                Constructor = codeGen.TheModule->getFunction(ConstructorName);
            }
            fprintf(stderr, "DEBUG: After processing instantiations, constructor found: %p\n", Constructor);
        }
    }

    if (Constructor) {
        // Additional safety check for constructor
        if (!Constructor || !Constructor->getFunctionType()) {
            codeGen.logError(("Invalid constructor function type for " + LookupName).c_str());
            return nullptr;
        }

        // Prepare arguments: this pointer + user arguments
        std::vector<llvm::Value*> CallArgs;
        CallArgs.push_back(ObjPtr); // 'this' pointer

        // Add user arguments
        for (auto &Arg : E.getArgs()) {
            llvm::Value *ArgVal = codegen(*Arg);
            if (!ArgVal) return nullptr;
            CallArgs.push_back(ArgVal);
        }

        // Safety check for arguments
        if (!Constructor) {  // Double-check constructor validity before accessing
            codeGen.logError(("Constructor became invalid after argument processing for " + LookupName).c_str());
            return nullptr;
        }

        if (CallArgs.size() != Constructor->arg_size()) {
            codeGen.logError(("Argument count mismatch for constructor " + LookupName + ": expected " +
                     std::to_string(Constructor->arg_size()) + ", got " + std::to_string(CallArgs.size())).c_str());
            return nullptr;
        }

        // Call constructor - add safety checks
        if (!codeGen.Builder->GetInsertBlock()) {
            codeGen.logError(("Cannot call constructor: no insertion point available for " + LookupName).c_str());
            return nullptr;
        }

        // Make sure all arguments are valid
        for (size_t i = 0; i < CallArgs.size(); ++i) {
            if (!CallArgs[i]) {
                codeGen.logError(("Invalid argument at index " + std::to_string(i) + " for constructor " + LookupName).c_str());
                return nullptr;
            }
        }

        // Make sure the function is valid
        if (!Constructor || !Constructor->getFunctionType()) {
            codeGen.logError(("Invalid constructor function for " + LookupName).c_str());
            return nullptr;
        }

        // Final safety check before calling
        if (!Constructor || !Constructor->getFunctionType()) {
            codeGen.logError(("Constructor function became invalid before call for " + LookupName).c_str());
            return nullptr;
        }

        fprintf(stderr, "DEBUG: About to call constructor - function: %p, num args: %zu\n", Constructor, CallArgs.size());

        // Perform detailed validation before the call
        if (!Constructor->getFunctionType()) {
            fprintf(stderr, "ERROR: Constructor function has no type\n");
            return nullptr;
        }

        if (CallArgs.size() != Constructor->arg_size()) {
            fprintf(stderr, "ERROR: Argument count mismatch - expected: %zu, got: %zu\n",
                    (size_t)Constructor->arg_size(), CallArgs.size());
            return nullptr;
        }

        // Validate each argument type matches the expected parameter type
        for (size_t i = 0; i < CallArgs.size(); ++i) {
            if (i >= Constructor->arg_size()) {
                fprintf(stderr, "ERROR: Argument index %zu exceeds function parameter count %zu\n",
                        i, (size_t)Constructor->arg_size());
                return nullptr;
            }

            llvm::Type *expectedType = Constructor->getFunctionType()->getParamType(i);
            llvm::Type *actualType = CallArgs[i]->getType();

            fprintf(stderr, "DEBUG: Validating arg %zu - expected: %p, actual: %p\n", i, expectedType, actualType);

            // Basic type compatibility check - for now just make sure both are valid
            if (!expectedType || !actualType) {
                fprintf(stderr, "ERROR: Invalid types for arg %zu - expected: %p, actual: %p\n", i, expectedType, actualType);
                return nullptr;
            }
        }

        fprintf(stderr, "DEBUG: All validations passed, about to make constructor call\n");

        // Call constructor
        llvm::Value *callResult = codeGen.Builder->CreateCall(Constructor, CallArgs);
        fprintf(stderr, "DEBUG: Constructor call completed successfully, result: %p\n", callResult);
    } else {
        // Only error if we expected a constructor and didn't find one.
        // Default constructor might be implicit (do nothing).
        if (!E.getArgs().empty()) {
             // Try to print useful error
             std::string err = "No matching constructor found for " + LookupName + " with arguments: ";
             for (const auto& t : argTypes) {
                 // Convert OType to string representation?
                 // err += ...
             }
             // For now simple error
             // codeGen.logError(("No matching constructor found for " + LookupName).c_str());
             // But actually, we might just proceed with uninitialized memory if no constructor.
             // But if args provided, it's definitely an error.
             codeGen.logError(("No matching constructor found for " + LookupName + " (tried " + MangledName + ")").c_str());
             return nullptr;
        }
    }

    return ObjPtr; // Return pointer to the allocated object
}

llvm::Value *ExpressionCodeGen::codegen(NewArrayExprAST &E) {
    // 1. Get/Declare malloc
    llvm::Function *MallocF = codeGen.TheModule->getFunction("malloc");
    if (!MallocF) {
        std::vector<llvm::Type*> Args;
        Args.push_back(llvm::Type::getInt64Ty(*codeGen.TheContext)); // size_t
        llvm::FunctionType *FT = llvm::FunctionType::get(
            llvm::PointerType::get(llvm::Type::getInt8Ty(*codeGen.TheContext), 0), Args, false);
        MallocF = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "malloc", codeGen.TheModule);
    }

    // 2. Size
    llvm::Value *SizeVal = codegen(*E.getSize());
    if (!SizeVal) return nullptr;

    // Promote to i64 for malloc
    llvm::Value *Size64 = codeGen.Builder->CreateZExtOrBitCast(SizeVal, llvm::Type::getInt64Ty(*codeGen.TheContext));

    // 3. Calculate Bytes
    size_t ElemBytes = 4; // Default int
    if (E.getElementType().base == BaseType::Char || E.getElementType().base == BaseType::Bool || E.getElementType().base == BaseType::Byte) ElemBytes = 1;
    else if (E.getElementType().base == BaseType::Float) ElemBytes = 8;
    else if (E.getElementType().isPointer()) ElemBytes = 8;
    else if (E.getElementType().base == BaseType::Struct) {
         if (TypeRegistry::getInstance().hasStruct(E.getElementType().structName)) {
             ElemBytes = TypeRegistry::getInstance().getStruct(E.getElementType().structName).totalSize;
         }
    }

    llvm::Value *ElemSizeVal = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*codeGen.TheContext), ElemBytes);
    llvm::Value *TotalBytes = codeGen.Builder->CreateMul(Size64, ElemSizeVal, "mallocsize");

    // Convert TotalBytes to match malloc's argument type
    llvm::Value *MallocArg = TotalBytes;
    if (MallocF->arg_size() > 0) {
        llvm::Type *MallocArgType = MallocF->getArg(0)->getType();
        if (MallocArgType != MallocArg->getType()) {
             MallocArg = codeGen.Builder->CreateZExtOrTrunc(MallocArg, MallocArgType, "malloc_size_cast");
        }
    }

    // 4. Call Malloc
    llvm::Value *VoidPtr = codeGen.Builder->CreateCall(MallocF, {MallocArg}, "mallocptr");

    // Cast to ElementType*
    llvm::Type *ElemLLVMType = codeGen.utilCodeGen->getLLVMType(E.getElementType());
    llvm::Value *TypedPtr = codeGen.Builder->CreateBitCast(VoidPtr, llvm::PointerType::get(ElemLLVMType, 0), "typedptr");

    // 5. Create Slice { Size, Ptr }
    llvm::StructType *SliceType = codeGen.utilCodeGen->getSliceType(ElemLLVMType);
    llvm::Value *Slice = llvm::UndefValue::get(SliceType);

    // Insert Size (cast back to i32 if needed)
    llvm::Value *Size32 = codeGen.Builder->CreateTruncOrBitCast(Size64, llvm::Type::getInt32Ty(*codeGen.TheContext));
    Slice = codeGen.Builder->CreateInsertValue(Slice, Size32, 0, "slice_len");

    // Insert Ptr
    Slice = codeGen.Builder->CreateInsertValue(Slice, TypedPtr, 1, "slice_ptr");

    return Slice;
}

llvm::Value *ExpressionCodeGen::codegen(MatchExprAST &E) {
    llvm::Value *CondVal = codegen(*E.getCond());
    if (!CondVal) return nullptr;

    llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*codeGen.TheContext, "matchcont");
    llvm::BasicBlock *CurrentTestBB = llvm::BasicBlock::Create(*codeGen.TheContext, "matchtest", TheFunction);

    codeGen.Builder->CreateBr(CurrentTestBB);

    // PHI Node tracking
    std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> IncomingValues;
    bool HasDefault = false;

    for (auto &Case : E.getCases()) {
        codeGen.Builder->SetInsertPoint(CurrentTestBB);

        llvm::BasicBlock *BodyBB = llvm::BasicBlock::Create(*codeGen.TheContext, "matchbody", TheFunction);
        llvm::BasicBlock *NextBB = llvm::BasicBlock::Create(*codeGen.TheContext, "nexttest", TheFunction);

        if (Case.Pattern) {
            // Check Pattern
            llvm::Value *PatternVal = codegen(*Case.Pattern);
            if (!PatternVal) return nullptr;

            // Generate Compare
            llvm::Value *Cmp = nullptr;
            if (CondVal->getType()->isFloatingPointTy()) {
                Cmp = codeGen.Builder->CreateFCmpOEQ(CondVal, PatternVal, "matcheq");
            } else {
                // Integer or Pointer
                Cmp = codeGen.Builder->CreateICmpEQ(CondVal, PatternVal, "matcheq");
            }

            codeGen.Builder->CreateCondBr(Cmp, BodyBB, NextBB);
        } else {
            // Wildcard (_)
            codeGen.Builder->CreateBr(BodyBB);
            HasDefault = true;
            // No next test needed as wildcard matches everything
            // Remove NextBB from function since we won't jump to it
            NextBB->eraseFromParent();
        }

        // Generate Body
        codeGen.Builder->SetInsertPoint(BodyBB);
        llvm::Value *BodyVal = codegen(*Case.Body);
        if (!BodyVal) return nullptr;

        // Add to PHI (if not void and not terminated)
        if (!codeGen.Builder->GetInsertBlock()->getTerminator()) {
            codeGen.Builder->CreateBr(MergeBB);
            IncomingValues.push_back({BodyVal, codeGen.Builder->GetInsertBlock()});
        }

        if (HasDefault) {
            break;
        }

        // Move to next
        CurrentTestBB = NextBB;
    }

    // If no default and we ran out of cases:
    if (!HasDefault) {
        codeGen.Builder->SetInsertPoint(CurrentTestBB);
        // Trap (Non-exhaustive match)
        codeGen.Builder->CreateCall(codeGen.utilCodeGen->getTrapFunc(), {});
        codeGen.Builder->CreateUnreachable();
    }

    // Merge Block
    TheFunction->insert(TheFunction->end(), MergeBB);
    codeGen.Builder->SetInsertPoint(MergeBB);

    if (IncomingValues.empty()) {
        // Void or all returns
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*codeGen.TheContext)); // Dummy
    }

    // Create PHI
    llvm::Type *PhiType = IncomingValues[0].first->getType();
    if (PhiType->isVoidTy()) {
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*codeGen.TheContext));
    }

    llvm::PHINode *PN = codeGen.Builder->CreatePHI(PhiType, IncomingValues.size(), "matchphi");
    for (auto &Pair : IncomingValues) {
        // Handle type mismatch? Assume uniform for now.
        PN->addIncoming(Pair.first, Pair.second);
    }

    return PN;
}

llvm::Value *ExpressionCodeGen::codegen(IndexExprAST &E) {
    // 0. Operator Overloading Check (Structs)
    OType ArrayOType = E.getArray()->getOType();
    if (ArrayOType.base == BaseType::Struct) {
        std::string OpMethod = "op_index";
        std::string StructName = ArrayOType.structName;

        if (!ArrayOType.genericArgs.empty()) {
            StructName = codeGen.utilCodeGen->mangleGenericName(StructName, ArrayOType.genericArgs);
        }

        std::string MangledName = StructName + "_" + OpMethod;
        llvm::Function *OpFunc = codeGen.TheModule->getFunction(MangledName);

        if (OpFunc) {
            // Prepare Args: Array (this), Index
            llvm::Value *ArrayVal = nullptr;
            if (ArrayOType.isPointer()) {
                ArrayVal = codegen(*E.getArray());
            } else {
                ArrayVal = codegenAddress(*E.getArray());
            }

            if (!ArrayVal) return nullptr;

            llvm::Value *IndexVal = codegen(*E.getIndex());
            if (!IndexVal) return nullptr;

            std::vector<llvm::Value*> ArgsV;
            ArgsV.push_back(ArrayVal); // this
            ArgsV.push_back(IndexVal); // index

            return codeGen.Builder->CreateCall(OpFunc, ArgsV, "opindextmp");
        }
    }

    // 1. Get Array Address
    llvm::Value *ArrayAddr = nullptr;
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(E.getArray())) {
        ArrayAddr = codegenAddress(*VarExpr);
    } else {
        ArrayAddr = codegen(*E.getArray());
    }
    if (!ArrayAddr) return nullptr;

    // 2. Resolve Type Info from AST
    // OType ArrayOType = E.getArray()->getOType();

    // Check for Slice (Dynamic Array)
    if (ArrayOType.isArray() && ArrayOType.getArrayNumElements() == -1) {
        // Slice Structure Type
        llvm::Type *SliceType = codeGen.utilCodeGen->getLLVMType(ArrayOType); // This should be {i32, T*}

        // 1. Get Size
        llvm::Value *SizePtr = codeGen.Builder->CreateStructGEP(SliceType, ArrayAddr, 0, "slice_len_ptr");
        llvm::Value *SizeVal = codeGen.Builder->CreateLoad(llvm::Type::getInt32Ty(*codeGen.TheContext), SizePtr, "slice_len");

        // 2. Bounds Check
        llvm::Value *IndexVal = codegen(*E.getIndex());
        if (!IndexVal) return nullptr;
        codeGen.utilCodeGen->createBoundsCheck(IndexVal, SizeVal);

        // 3. Get Data Pointer
        llvm::Value *DataPtrPtr = codeGen.Builder->CreateStructGEP(SliceType, ArrayAddr, 1, "slice_data_ptr");
        llvm::Type *DataPtrType = SliceType->getStructElementType(1); // T*
        llvm::Value *DataPtr = codeGen.Builder->CreateLoad(DataPtrType, DataPtrPtr, "slice_data");

        // 4. Indexing GEP
        std::vector<llvm::Value*> Indices;
        Indices.push_back(IndexVal);

        // Element Type
        OType ElemOType = ArrayOType.getElementType();
        llvm::Type *ElemLLVMType = codeGen.utilCodeGen->getLLVMType(ElemOType);

        llvm::Value *ElementAddr = codeGen.Builder->CreateInBoundsGEP(
           ElemLLVMType,
           DataPtr,
           Indices,
           "slice_idx"
        );

        return codeGen.Builder->CreateLoad(ElemLLVMType, ElementAddr, "slice_val_load");
    }

    // Fixed Array
    llvm::Type *ArrayLLVMType = codeGen.utilCodeGen->getLLVMType(ArrayOType);
    if (!ArrayLLVMType->isArrayTy()) {
        // Fallback or Error?
        // Might be a pointer to array?
        if (ArrayLLVMType->isPointerTy()) {
             // Pointer arithmetic
             // ...
        }
    }

    // Get the index value
    llvm::Value *IndexVal = codegen(*E.getIndex());
    if (!IndexVal) return nullptr;

    // Perform Bounds Check
    uint64_t Size = ArrayOType.getArrayNumElements();
    llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), Size);
    codeGen.utilCodeGen->createBoundsCheck(IndexVal, SizeVal);

    // Create indices: {0, Index}
    std::vector<llvm::Value*> Indices;
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0));
    Indices.push_back(IndexVal);

    llvm::Value *ElementAddr = codeGen.Builder->CreateInBoundsGEP(
        ArrayLLVMType,
        ArrayAddr,
        Indices,
        "arrayidx"
    );

    // If the element is an array (multidimensional), return the address (decay to pointer)
    // Otherwise, load the value (primitive)
    if (ArrayOType.getElementType().isArray()) {
        return ElementAddr;
    }

    return codeGen.Builder->CreateLoad(codeGen.utilCodeGen->getLLVMType(ArrayOType.getElementType()), ElementAddr, "arrayload");
}

llvm::Value *ExpressionCodeGen::codegenAddress(IndexExprAST &E) {
    // 1. Get Array Address
    llvm::Value *ArrayAddr = nullptr;
    if (auto VarExpr = dynamic_cast<VariableExprAST*>(E.getArray())) {
        ArrayAddr = codegenAddress(*VarExpr);
    } else {
        ArrayAddr = codegen(*E.getArray());
    }
    if (!ArrayAddr) return nullptr;

    // 2. Resolve Type Info from AST
    OType ArrayOType = E.getArray()->getOType();

    // Check for Slice (Dynamic Array)
    if (ArrayOType.isArray() && ArrayOType.getArrayNumElements() == -1) {
        // Slice Structure Type
        llvm::Type *SliceType = codeGen.utilCodeGen->getLLVMType(ArrayOType);

        // 1. Get Size
        llvm::Value *SizePtr = codeGen.Builder->CreateStructGEP(SliceType, ArrayAddr, 0, "slice_len_ptr");
        llvm::Value *SizeVal = codeGen.Builder->CreateLoad(llvm::Type::getInt32Ty(*codeGen.TheContext), SizePtr, "slice_len");

        // 2. Bounds Check
        llvm::Value *IndexVal = codegen(*E.getIndex());
        if (!IndexVal) return nullptr;
        codeGen.utilCodeGen->createBoundsCheck(IndexVal, SizeVal);

        // 3. Get Data Pointer
        llvm::Value *DataPtrPtr = codeGen.Builder->CreateStructGEP(SliceType, ArrayAddr, 1, "slice_data_ptr");
        llvm::Type *DataPtrType = SliceType->getStructElementType(1); // T*
        llvm::Value *DataPtr = codeGen.Builder->CreateLoad(DataPtrType, DataPtrPtr, "slice_data");

        // 4. Indexing GEP
        std::vector<llvm::Value*> Indices;
        Indices.push_back(IndexVal);

        // Element Type
        OType ElemOType = ArrayOType.getElementType();
        llvm::Type *ElemLLVMType = codeGen.utilCodeGen->getLLVMType(ElemOType);

        return codeGen.Builder->CreateInBoundsGEP(
           ElemLLVMType,
           DataPtr,
           Indices,
           "slice_addr"
        );
    }

    // Check for Pointer Indexing (ptr[i])
    if (ArrayOType.isPointer()) {
         llvm::Value *PtrVal = codegen(*E.getArray());
         if (!PtrVal) return nullptr;

         llvm::Value *IndexVal = codegen(*E.getIndex());
         if (!IndexVal) return nullptr;

         OType ElemOType = ArrayOType.getPointeeType();
         llvm::Type *ElemLLVMType = codeGen.utilCodeGen->getLLVMType(ElemOType);

         std::vector<llvm::Value*> Indices;
         Indices.push_back(IndexVal);

         return codeGen.Builder->CreateInBoundsGEP(ElemLLVMType, PtrVal, Indices, "ptridx");
    }

    // Fixed Array
    llvm::Type *ArrayLLVMType = codeGen.utilCodeGen->getLLVMType(ArrayOType);

    // Get the index value
    llvm::Value *IndexVal = codegen(*E.getIndex());
    if (!IndexVal) return nullptr;

    // Perform Bounds Check
    uint64_t Size = ArrayOType.getArrayNumElements();
    llvm::Value *SizeVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), Size);
    codeGen.utilCodeGen->createBoundsCheck(IndexVal, SizeVal);

    // Create indices: {0, Index}
    std::vector<llvm::Value*> Indices;
    Indices.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0));
    Indices.push_back(IndexVal);

    return codeGen.Builder->CreateInBoundsGEP(
        ArrayLLVMType,
        ArrayAddr,
        Indices,
        "arrayaddr"
    );
}

llvm::Value *ExpressionCodeGen::codegen(MemberAccessAST &E) {
    // 1. Get Address of Field
    llvm::Value *FieldPtr = codegenAddress(E);
    if (!FieldPtr) {
        // Fallback for non-lvalue access (e.g. array.len)
        OType ObjOType = E.getObject()->getOType();
        if (ObjOType.isArray() && (E.getFieldName() == "len" || E.getFieldName() == "length")) {
             return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), ObjOType.getArrayNumElements());
        }

        // Check for Slice Length Property
         if (ObjOType.isArray() && ObjOType.getArrayNumElements() == -1 && (E.getFieldName() == "len" || E.getFieldName() == "length")) {
             llvm::Value *ObjVal = codegen(*E.getObject());
             if (!ObjVal) return nullptr;

             if (ObjVal->getType()->isStructTy()) {
                 return codeGen.Builder->CreateExtractValue(ObjVal, 0, "len");
             }
         }

        // Check for Slice Ptr Property
         if (ObjOType.isArray() && ObjOType.getArrayNumElements() == -1 && E.getFieldName() == "ptr") {
             llvm::Value *ObjVal = codegen(*E.getObject());
             if (!ObjVal) return nullptr;

             if (ObjVal->getType()->isStructTy()) {
                 return codeGen.Builder->CreateExtractValue(ObjVal, 1, "ptr");
             }
         }

        return nullptr;
    }

    // 2. Load Value
    OType FieldType = E.getOType();
    llvm::Type *LLVMFieldType = codeGen.utilCodeGen->getLLVMType(FieldType);

    // For array fields, return the address instead of loading
    if (FieldType.isArray()) {
        return FieldPtr;
    }

    return codeGen.Builder->CreateLoad(LLVMFieldType, FieldPtr, E.getFieldName());
}

llvm::Value *ExpressionCodeGen::codegenAddress(MemberAccessAST &E) {
    fprintf(stderr, "DEBUG: Entering MemberAccessAST::codegenAddress for field '%s'\n", E.getFieldName().c_str());

    // 1. Resolve Object Address/Pointer
    llvm::Value *ObjPtr = nullptr;

    fprintf(stderr, "DEBUG: About to get object for field '%s'\n", E.getFieldName().c_str());
    OType objOType = E.getObject()->getOType();
    fprintf(stderr, "DEBUG: Object OType - base: %d, pointerDepth: %d, structName: %s\n",
            static_cast<int>(objOType.base), objOType.pointerDepth, objOType.structName.c_str());

    // If Object is a pointer (like 'this'), we need its value (the address it points to)
    if (objOType.isPointer()) {
        fprintf(stderr, "DEBUG: Object is a pointer, calling codegen\n");
        ObjPtr = codegen(*E.getObject());
    } else {
        fprintf(stderr, "DEBUG: Object is not a pointer, calling codegenAddress\n");
        // If Object is a struct value, we need its address
        ObjPtr = codegenAddress(*E.getObject());
    }

    if (!ObjPtr) {
        fprintf(stderr, "DEBUG: ObjPtr is null, returning nullptr\n");
        return nullptr;
    }

    fprintf(stderr, "DEBUG: ObjPtr is valid: %p\n", ObjPtr);

    // 2. Resolve Field Info
    OType ObjType = E.getObject()->getOType();
    std::string StructName;

    if (ObjType.isPointer()) {
        OType pointeeType = ObjType.getPointeeType();
        StructName = pointeeType.structName;
        fprintf(stderr, "DEBUG: Object is pointer, pointee structName: %s\n", pointeeType.structName.c_str());

        // Handle Generics for pointer types
        if (!pointeeType.genericArgs.empty()) {
            StructName = codeGen.utilCodeGen->mangleGenericName(pointeeType.structName, pointeeType.genericArgs);
            fprintf(stderr, "DEBUG: Mangled name for pointer type: %s\n", StructName.c_str());
        }
    } else {
        StructName = ObjType.structName;
        fprintf(stderr, "DEBUG: Object is value, structName: %s\n", ObjType.structName.c_str());

        // Handle Generics for value types
        if (!ObjType.genericArgs.empty()) {
            StructName = codeGen.utilCodeGen->mangleGenericName(ObjType.structName, ObjType.genericArgs);
            fprintf(stderr, "DEBUG: Mangled name for value type: %s\n", StructName.c_str());
        }
    }

    // Ensure the struct is properly instantiated if it's generic
    if (!ObjType.genericArgs.empty()) {
        fprintf(stderr, "DEBUG: Instantiating generic struct: %s\n", ObjType.structName.c_str());
        codeGen.utilCodeGen->instantiateStruct(ObjType.structName, ObjType.genericArgs);
        StructName = codeGen.utilCodeGen->mangleGenericName(ObjType.structName, ObjType.genericArgs);
    } else if (ObjType.isPointer()) {
        OType pointeeType = ObjType.getPointeeType();
        if (!pointeeType.genericArgs.empty()) {
            fprintf(stderr, "DEBUG: Instantiating generic pointer struct: %s\n", pointeeType.structName.c_str());
            codeGen.utilCodeGen->instantiateStruct(pointeeType.structName, pointeeType.genericArgs);
            StructName = codeGen.utilCodeGen->mangleGenericName(pointeeType.structName, pointeeType.genericArgs);
        }
    }

    fprintf(stderr, "DEBUG: Looking for struct '%s' in registry\n", StructName.c_str());
    if (!TypeRegistry::getInstance().hasStruct(StructName)) {
        fprintf(stderr, "DEBUG: Struct '%s' not found in registry\n", StructName.c_str());
        codeGen.logError(("Struct not found in registry: " + StructName).c_str());
        return nullptr;
    }

    const StructInfo& info = TypeRegistry::getInstance().getStruct(StructName);
    fprintf(stderr, "DEBUG: Found struct '%s' with %zu fields\n", StructName.c_str(), info.fields.size());

    int FieldIndex = -1;
    for (size_t i = 0; i < info.fields.size(); ++i) {
        fprintf(stderr, "DEBUG: Checking field %zu: '%s' vs '%s'\n", i, info.fields[i].name.c_str(), E.getFieldName().c_str());
        if (info.fields[i].name == E.getFieldName()) {
            FieldIndex = i;
            fprintf(stderr, "DEBUG: Found field '%s' at index %d\n", E.getFieldName().c_str(), FieldIndex);
            break;
        }
    }

    if (FieldIndex == -1) {
        fprintf(stderr, "DEBUG: Field '%s' not found in struct '%s'\n", E.getFieldName().c_str(), StructName.c_str());
        codeGen.logError(("Field '" + E.getFieldName() + "' not found in struct '" + StructName + "'").c_str());
        return nullptr;
    }

    // 3. Generate GEP
    llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), 0);
    llvm::Value *Idx = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*codeGen.TheContext), FieldIndex);
    fprintf(stderr, "DEBUG: Creating GEP with indices [0, %d]\n", FieldIndex);

    // Safety check: ensure the struct type exists in StructTypes
    fprintf(stderr, "DEBUG: Looking for struct type '%s' in StructTypes map\n", StructName.c_str());
    if (codeGen.StructTypes.find(StructName) == codeGen.StructTypes.end()) {
        fprintf(stderr, "DEBUG: Struct type '%s' not found in StructTypes map\n", StructName.c_str());
        codeGen.logError(("Struct type not found in StructTypes map: " + StructName).c_str());
        return nullptr;
    }

    llvm::Type* structType = codeGen.StructTypes[StructName];
    fprintf(stderr, "DEBUG: Retrieved structType: %p, isStructTy: %s\n", structType, structType && structType->isStructTy() ? "true" : "false");

    if (!structType || !structType->isStructTy()) {
        fprintf(stderr, "DEBUG: Invalid struct type for: %s\n", StructName.c_str());
        codeGen.logError(("Invalid struct type for: " + StructName).c_str());
        return nullptr;
    }

    fprintf(stderr, "DEBUG: About to call CreateInBoundsGEP - structType: %p, ObjPtr: %p\n", structType, ObjPtr);
    fprintf(stderr, "DEBUG: ObjPtr type info\n");
    if (structType->isStructTy()) {
        fprintf(stderr, "DEBUG: structType has %d elements\n", structType->getStructNumElements());
    }

    llvm::Value *result = codeGen.Builder->CreateInBoundsGEP(
        structType,
        ObjPtr,
        {Zero, Idx},
        "fieldaddr"
    );
    fprintf(stderr, "DEBUG: CreateInBoundsGEP succeeded, result: %p\n", result);
    return result;
}

llvm::Value *ExpressionCodeGen::codegen(ArrayInitExprAST &E) {
    if (E.getElements().empty()) {
        codeGen.logError("Cannot create empty array without type information");
        return nullptr;
    }

    // Generate code for first element to infer type
    llvm::Value *FirstElement = codegen(*E.getElements()[0]);
    if (!FirstElement) return nullptr;

    llvm::Type *ElementType = FirstElement->getType();
    size_t ArraySize = E.getElements().size();

    // Create array type
    llvm::ArrayType *ArrayType = llvm::ArrayType::get(ElementType, ArraySize);

    // Allocate array on stack
    llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();
    llvm::AllocaInst *ArrayAlloca = codeGen.createEntryBlockAlloca(TheFunction, "array", ArrayType);

    // Store first element
    llvm::Value *FirstIdx = llvm::ConstantInt::get(*codeGen.TheContext, llvm::APInt(32, 0));
    llvm::Value *FirstPtr = codeGen.Builder->CreateInBoundsGEP(ArrayType, ArrayAlloca, {FirstIdx, FirstIdx});
    codeGen.Builder->CreateStore(FirstElement, FirstPtr);

    // Store remaining elements
    for (size_t i = 1; i < E.getElements().size(); ++i) {
        llvm::Value *Element = codegen(*E.getElements()[i]);
        if (!Element) return nullptr;

        // Type check
        if (Element->getType() != ElementType) {
            codeGen.logError("Array elements must have the same type");
            return nullptr;
        }

        llvm::Value *Idx = llvm::ConstantInt::get(*codeGen.TheContext, llvm::APInt(32, i));
        llvm::Value *ElementPtr = codeGen.Builder->CreateInBoundsGEP(ArrayType, ArrayAlloca, {FirstIdx, Idx});
        codeGen.Builder->CreateStore(Element, ElementPtr);
    }

    return codeGen.Builder->CreateLoad(ArrayType, ArrayAlloca, "arrayinit");
}

llvm::Value *ExpressionCodeGen::codegen(ArrayLiteralExprAST &E) {
    // Create a fixed-size array on the stack
    llvm::Type *ElementType = codeGen.utilCodeGen->getLLVMType(E.getElementType());
    if (!ElementType) {
        codeGen.logError("Could not determine element type for array literal");
        return nullptr;
    }

    // Create array type with the specified size
    llvm::ArrayType *ArrayType = llvm::ArrayType::get(ElementType, E.getSize());

    // Allocate array on stack in the entry block
    llvm::Function *TheFunction = codeGen.Builder->GetInsertBlock()->getParent();
    llvm::AllocaInst *ArrayAlloca = codeGen.createEntryBlockAlloca(TheFunction, "fixed_array", ArrayType);

    // Initialize all elements to zero/default value
    for (int i = 0; i < E.getSize(); ++i) {
        llvm::Value *Idx = llvm::ConstantInt::get(*codeGen.TheContext, llvm::APInt(32, 0));
        llvm::Value *Idx2 = llvm::ConstantInt::get(*codeGen.TheContext, llvm::APInt(32, i));
        llvm::Value *ElementPtr = codeGen.Builder->CreateInBoundsGEP(ArrayType, ArrayAlloca, {Idx, Idx2});

        // Create default value based on element type
        llvm::Value *DefaultValue;
        if (ElementType->isIntegerTy()) {
            DefaultValue = llvm::ConstantInt::get(ElementType, 0);
        } else if (ElementType->isDoubleTy()) {
            DefaultValue = llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0));
        } else if (ElementType->isFloatTy()) {
            DefaultValue = llvm::ConstantFP::get(*codeGen.TheContext, llvm::APFloat(0.0f));
        } else if (ElementType->isPointerTy()) {
            DefaultValue = llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(ElementType));
        } else {
            DefaultValue = llvm::UndefValue::get(ElementType);
        }

        codeGen.Builder->CreateStore(DefaultValue, ElementPtr);
    }

    return ArrayAlloca; // Return pointer to the array
}

// Generic address generation method
llvm::Value *ExpressionCodeGen::codegenAddress(ExprAST &E) {
    // Dispatch to the appropriate address codegen method based on dynamic type
    if (auto *VarExpr = dynamic_cast<VariableExprAST*>(&E)) {
        return codegenAddress(*VarExpr);
    } else if (auto *IndexExpr = dynamic_cast<IndexExprAST*>(&E)) {
        return codegenAddress(*IndexExpr);
    } else if (auto *MemberAccess = dynamic_cast<MemberAccessAST*>(&E)) {
        return codegenAddress(*MemberAccess);
    } else if (auto *DerefExpr = dynamic_cast<DerefExprAST*>(&E)) {
        return codegenAddress(*DerefExpr);
    } else {
        codeGen.logError("Address-of operator not supported for this expression type");
        return nullptr;
    }
}