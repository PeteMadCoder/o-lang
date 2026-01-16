# The O Language

**O** is a statically-typed, compiled systems programming language designed to bridge the gap between low-level control (C-style pointers, manual memory management) and high-level safety (RAII, automatic cleanup, strong typing). It targets LLVM IR for efficient compilation to native code.

## Quick Overview

O combines the performance and control of systems languages like C/C++ with modern safety features and expressive syntax. It's designed for developers who want both low-level control and high-level abstractions.

### Key Features
- **Statically Typed**: Strong, static type system with type inference
- **Memory Safe**: RAII-based automatic memory management with manual override options
- **Modern Syntax**: Clean, readable syntax with explicit type annotations
- **Systems Programming**: Direct memory access, pointers, and control over resource management
- **Generic Programming**: Full support for generic types and functions
- **Object Oriented**: Classes with inheritance, virtual methods, and encapsulation
- **Functional Constructs**: Pattern matching, closures, and functional programming features
- **Module System**: Import/export mechanism for code organization

## Language Basics

### Hello World
```o
import "std/io.olang";

fn main() -> int {
    println("Hello, World!");
    return 0;
}
```

### Variables and Constants
```o
// Variables
var x: int = 42;
var name: *String = new String("O Language");

// Constants
let pi: float = 3.14159;
let count = 10;  // Type inferred as int
```

### Functions
```o
fn add(a: int, b: int) -> int {
    return a + b;
}

// Function with multiple return types (tuple-like)
fn divide_with_remainder(dividend: int, divisor: int) -> (int, int) {
    return (dividend / divisor, dividend % divisor);
}
```

### Structs and Methods
```o
struct Point {
    x: int;
    y: int;

    fn distance_from_origin(self) -> float {
        return sqrt_f((self.x * self.x + self.y * self.y) as float);
    }
}

fn main() -> int {
    var p = new Point { x = 3, y = 4 };
    var dist = p.distance_from_origin();
    return 0;
}
```

### Memory Management
```o
fn example_memory_management() -> int {
    // Automatic memory management with RAII
    var dynamic_array = new int[10];  // Allocated on heap, freed automatically
    
    // Manual memory management in unsafe block
    unsafe {
        var ptr = malloc(100);
        // ... use ptr ...
        free(ptr);  // Manual cleanup
    }
    
    return 0;
}
```

## Installation

### Prerequisites
- LLVM 16 or higher
- CMake 3.14 or higher
- A C++17 compatible compiler

### Building the Compiler
```bash
cd o-compiler
mkdir build
cd build
cmake ..
make
```

This creates the `oc` executable (O compiler).

### Usage
```bash
# Compile a program
oc main.olang -o myprogram

# Compile with include paths
oc main.olang -I ./stdlib -o myprogram

# Compile only (no linking)
oc main.olang -c -o main.obj
```

## Language Guide

For detailed language documentation, see:
 - [Documentation Overview](docs/TOC.md)
 - [Tutorial](docs/tutorial.md)- [Language Guide](docs/language_guide.md)
 - [Syntax Reference](docs/syntax_reference.md)
 - [Quick Reference](docs/quick_reference.md)
 - [Language Specification](O_LANGUAGE_SPEC.md)
 - [Standard Library Documentation](STDLIB_DOCUMENTATION.md)

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

If you have questions or need help:
- Check the documentation
- Open an issue on GitHub
- Join our community discussions