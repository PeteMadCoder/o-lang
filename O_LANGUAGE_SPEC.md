# The O Language Specification & Status Report
**Date:** January 2026
**Version:** 0.3 (Alpha)

## 1. Introduction
"O" is a statically-typed, compiled systems programming language designed to bridge the gap between low-level control (C-style pointers, manual memory management) and high-level safety (RAII, automatic cleanup, strong typing). It targets LLVM IR.

---

## 2. Language Specification

### 2.1 Type System
"O" employs a strong, static type system with support for both primitive and aggregate types.

*   **Primitives:**
    *   `int`: 32-bit signed integer.
    *   `float`: 64-bit floating point (double precision).
    *   `bool`: Boolean (`true` / `false`).
    *   `char`: 8-bit character.
    *   `void`: Empty type.

*   **Aggregate Types:**
    *   **Fixed Arrays:** `T[N]` (e.g., `int[5]`). Allocated on the stack.
    *   **Dynamic Arrays (Slices):** `new T[N]` (e.g., `new int[5]`). Allocated on the heap. Internally represented as `{ i32 size, T* ptr }`.
    *   **Structs:** User-defined composite types.
        ```o
        struct Point {
            int x;
            int y;
        }
        ```

*   **Pointers:**
    *   Raw pointers: `*T` (e.g., `*int`).
    *   References: `&var` (Address-of), `*ptr` (Dereference).

### 2.2 Variables & Scoping
*   **Declaration:** `var name = value;` or `var name = new Type();`.
*   **Scoping:** Block-scoped `{ ... }`. Variables declared inside a block are not visible outside.
*   **Shadowing:** Inner scopes can shadow variables from outer scopes.

### 2.3 Memory Management (The "O" Model)
"O" uses a hybrid memory model:
1.  **RAII (Resource Acquisition Is Initialization):**
    *   Variables owning heap memory (specifically Dynamic Arrays/Slices declared with `new`) are automatically tracked.
    *   When an owning variable goes out of scope (block exit, return, etc.), the compiler automatically emits a `free()` call.
2.  **Unsafe Block:**
    *   Dangerous operations must be enclosed in `unsafe { ... }`.
    *   Allows: Dereferencing raw pointers (`*ptr`), manual `delete`.
3.  **Manual Management:**
    *   `delete expr;` allows early freeing of memory.
    *   **Restriction:** Only allowed inside `unsafe` blocks.
    *   **Safety:** The compiler attempts to remove the variable from the RAII tracker if manually deleted to prevent double-frees.

### 2.4 Control Flow
*   **If-Else:** `if (cond) { ... } else { ... }` (Expressions return values).
*   **Loops:**
    *   `while (cond) { ... }`
    *   `for (init; cond; step) { ... }`
*   **Return:** `return value;` or `return;`.

### 2.5 Functions
```o
fn add(int a, int b) -> int {
    return a + b;
}
```

---

## 3. Implementation Status

### 3.1 Compiler Architecture
*   **Lexer:** Complete. Handles keywords, operators, identifiers, and literals.
*   **Parser:** Recursive Descent. Builds an Abstract Syntax Tree (AST).
*   **CodeGen:** Generates LLVM IR (Targeting LLVM 18).
    *   Uses **Opaque Pointers** (`ptr`) for compatibility with modern LLVM.
    *   Implements a **ScopeStack** for symbol resolution.
    *   Implements **Type Tracking** (`NamedTypes`, `getOType`) for strict GEP generation.

### 3.2 Completed Features (Milestones 1, 2, & 3)
1.  **Basic Compilation:**
    *   Compiles functions, arithmetic, assignments, and returns.
    *   Links with standard C library (relies on `malloc`/`free`).
2.  **Structs:**
    *   Declaration and Instantiation (`new Point()`).
    *   Member Access (`p.x = 10`, `var y = p.y`).
    *   Memory Layout calculation (offsets).
3.  **Arrays:**
    *   Fixed Arrays: Stack allocation, indexing.
    *   Dynamic Arrays: Heap allocation (`new`), indexing, length access (`arr.len`).
    *   Type-Safe Indexing: Strict GEP usage prevents LLVM errors.
4.  **Scope Safety:**
    *   Implemented `EnterScope`/`ExitScope`.
    *   Fixed variable leakage (inner variables do not pollute outer scope).
5.  **Memory Safety (RAII):**
    *   Automatic generation of `free()` calls for Slice types at scope exit.
    *   Scope unwinding on `return` statements.
6.  **Unsafe/Manual Memory:**
    *   `unsafe { ... }` blocks enforced.
    *   `delete ptr;` statement implemented (removes var from RAII stack).
    *   Pointer dereferencing (`*ptr`) restricted to unsafe blocks.
7.  **Object-Oriented Programming:**
    *   **Method Calls:** `obj.method(args)` syntax supported with static dispatch.
    *   **Constructors:** `new Class()` allocates memory (Heap) and calls the constructor `new()`.
    *   **Inheritance:** `class Child : Parent` inherits fields and methods.
    *   **Dynamic Dispatch:** `virtual` and `override` methods use VTables for runtime polymorphism.

### 3.3 Known Limitations & Missing Features (Tech Debt)
1.  **Generics:**
    *   *Status:* Parser supports `<T>`. `StructDeclAST` stores generic params.
    *   *Missing:* Code generation for generics is explicitly skipped. No instantiation logic (monomorphization) exists.
2.  **Strings:**
    *   *Status:* String literals create global `i8*` constants.
    *   *Missing:* No `String` struct/class in stdlib. No string concatenation or manipulation operators.
3.  **Imports:**
    *   `import` keyword is parsed but does nothing. No multi-file compilation/linking logic in the compiler driver.
4.  **Standard Library:**
    *   No built-in print (except relying on external linking), math, or IO libraries. Strings fall into this category, since they should be implemented in the standart library.

---

## 4. Example Code (Supported)
```o
open class Animal {
    new() {}
    virtual fn speak() -> int { return 1; }
}

class Dog : Animal {
    new() {}
    override fn speak() -> int { return 2; }
}

fn main() -> int {
    var d = new Dog();
    
    // Virtual Method Call (Dynamic Dispatch)
    return d.speak(); // Returns 2
}
```
