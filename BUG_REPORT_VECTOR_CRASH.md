# Bug Report: Compiler Segmentation Fault in Vector::pop() (LLVM CreateCall)

**Severity:** Critical
**Status:** Open
**Date:** January 12, 2026
**Component:** Code Generation (Deferred Instantiation / LLVM IR Generation)

## Summary
The compiler crashes with a **Segmentation Fault (Signal 139)** during the deferred instantiation of the standard library `Vector<T>` class. Specifically, the crash occurs when generating the code for the `pop()` method, at the point where it calls the external function `exit(1)`.

This issue persists even after applying the "Lazy Declaration Instability" fix (which resolved similar crashes in simpler generic structs like `Box<T>`). The crash appears specific to the `Vector` implementation or its context within the standard library.

## Reproduction Steps

### 1. Minimal Test Case (`vector_test.olang`)
Create a file that instantiates `Vector<int>` and triggers the generation of `pop()` (implicitly or explicitly). Note: `pop` is generic, so it is generated only when used or instantiated.

```olang
import "std/vector.olang";

fn main() -> int {
    var v = new Vector<int>();
    v.push(10);
    
    // Calling pop() triggers the crash during compilation
    v.pop();
    
    return 0;
}
```

### 2. Command
```bash
./oc vector_test.olang -o vector_test -l c
```

### 3. Output
```text
Compiling: vector_test.olang
...
Processing Deferred Item: Vector_int
  Generating method: push
  Generating method: pop
Segmentation fault (core dumped)
```

## Technical Analysis

### Stack Trace / Location
The crash occurs in `o-compiler/src/codegen/ExpressionCodeGen.cpp`, within `codegen(CallExprAST &E)`.
The specific instruction triggering the Segfault is:
```cpp
return codeGen.Builder->CreateCall(CalleeF->getFunctionType(), CalleeF, ArgsV, "calltmp");
```

### Debug Observations
Extensive debugging revealed the following state immediately before the crash:
1.  **Function Lookup:** `CalleeF` (pointer to `exit`) is successfully found in `TheModule`. Address is valid (e.g., `0x607e20be5908`).
2.  **Function Type:** `CalleeF->getFunctionType()` is valid and prints correctly (`declare void @exit(i32)`).
3.  **Arguments:** `ArgsV` contains 1 element. The element is a valid `i32` constant (generated from literal `1`).
4.  **Insertion Point:** `codeGen.Builder->GetInsertBlock()` is valid and set to the `entry` block of `Vector_int_pop`.
5.  **Previous Methods:** `push()` (which calls `malloc` and `free`) generates successfully immediately before `pop()`.

### Contrast with Working Case (`Box<T>`)
A simplified reproduction using a local generic struct `Box<T>` works correctly with the recent "Lazy Declaration" fix:
```olang
// This WORKS
struct Box<T> {
    fn do_exit() {
        exit(1);
    }
}
```
This suggests the issue is **not** simply "calling external functions from generics". It is likely related to:
*   **Complexity:** `Vector` is larger, uses `imports`, and has multiple methods.
*   **State Corruption:** The generation of `Vector_int_push` (which involves complex pointer arithmetic and memory ops) might be subtly corrupting the LLVM `Builder`, `Context`, or `Module` state, causing the subsequent `Vector_int_pop` generation to crash `CreateCall`.
*   **Memory Management:** `Vector` uses `malloc`/`free` via `memory.olang`.

## Hypothesized Root Cause
**Memory Corruption during Generic Instantiation:**
Since `CreateCall` crashes despite valid inputs, it is highly probable that an internal LLVM structure (like the `Use` list or `BasicBlock` instruction list) has been corrupted by a previous operation.
Suspects:
1.  **Buffer Overrun/Invalid Write:** In `push` generation or `instantiateStruct`.
2.  **Dangling Pointer:** `Builder` might be referencing a block that was somehow invalidated, or `CalleeF`'s internal state is inconsistent.
3.  **Double Free:** Potentially related to `Structure` cloning or `Type` substitution logic corrupting the AST or Type references.

## Recommended Investigation
1.  **Valgrind/ASan:** Run the compiler under Valgrind or AddressSanitizer to detect memory corruption during the `push` generation phase.
2.  **Isolate `pop`:** Modify `Vector` to *only* contain `pop` (remove `push`) and see if it still crashes. If it works, `push` generation is the culprit.
3.  **Simplify `Vector`:** Systematically remove fields and methods from `Vector` until the crash disappears to pinpoint the trigger.
