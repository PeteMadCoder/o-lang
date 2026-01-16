# O Language Documentation

Welcome to the comprehensive documentation for the O programming language.

## Getting Started
- [README](../README.md) - Overview of the O language
- [Tutorial](tutorial.md) - Step-by-step introduction to O programming

## Language Reference
- [Language Guide](language_guide.md) - Complete guide to O language features
- [Syntax Reference](syntax_reference.md) - Detailed syntax reference
- [Quick Reference](quick_reference.md) - Quick lookup for common operations

## Standard Library
- [Standard Library Documentation](../STDLIB_DOCUMENTATION.md) - Documentation for the O standard library
- Individual module documentation in `stdlib/` directory

## Language Specification
- [Language Specification](../O_LANGUAGE_SPEC.md) - Formal specification of the O language

## Compiler
- [Compiler Usage](#compiler-usage) - How to use the O compiler
- [Build Instructions](#building-the-compiler) - How to build the O compiler

## Advanced Topics
- [Memory Management](#memory-management) - Detailed guide to O's memory model
- [Generic Programming](#generics) - Advanced generic programming techniques
- [Object-Oriented Programming](#oop) - Advanced OOP concepts in O
- [Pattern Matching](#pattern-matching) - Using match expressions effectively

## Contributing
- [Contributing Guide](../CONTRIBUTING.md) - How to contribute to the O language
- [Development Setup](#development-setup) - Setting up a development environment

---

## Compiler Usage

The O compiler (`oc`) compiles `.olang` files to native executables:

```bash
# Basic compilation
oc main.olang -o myprogram

# With include paths
oc main.olang -I ./stdlib -I ./mylibs -o myprogram

# Compile only (no linking)
oc main.olang -c -o main.obj

# Link with external libraries
oc main.olang -l SDL2 -l pthread -o game

# Verbose output
oc main.olang -v -o myprogram
```

## Building the Compiler

Requirements:
- LLVM 16 or higher
- CMake 3.14 or higher
- C++17 compatible compiler

```bash
cd o-compiler
mkdir build
cd build
cmake ..
make
```

This produces the `oc` executable.

## Development Setup

To contribute to the O language compiler:

1. Fork the repository
2. Install dependencies (LLVM, CMake, C++ compiler)
3. Build the compiler as described above
4. Run tests: `cd o-compiler && make test`
5. Make your changes
6. Add/update tests as needed
7. Submit a pull request

## Memory Management Deep Dive

O uses a hybrid memory model combining RAII (Resource Acquisition Is Initialization) with optional manual control:

- **RAII**: Objects are automatically cleaned up when they go out of scope
- **Unsafe blocks**: Required for pointer dereferencing and manual memory management
- **Early deletion**: Use `delete` in unsafe blocks to free memory early

## Generic Programming

O supports parametric polymorphism through generic types and functions:

```o
struct Container<T> {
    item: T;
    // ...
}

fn process<T>(item: T) -> T {
    // ...
}
```

## Pattern Matching

The `match` expression provides powerful pattern matching capabilities:

```o
match (value) {
    Some(x) => process(x),
    None => handle_none(),
    _ => default_case()
}
```

## Community Resources

- GitHub repository: [https://github.com/username/o-lang](https://github.com/username/o-lang)
- Issue tracker: For bug reports and feature requests
- Discussions: For questions and community interaction