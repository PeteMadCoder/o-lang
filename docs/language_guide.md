# The O Language Guide

Welcome to the O Language! This guide will walk you through everything you need to know to get started with O, from basic syntax to advanced features.

## Table of Contents
1. [Getting Started](#getting-started)
2. [Basic Syntax](#basic-syntax)
3. [Data Types](#data-types)
4. [Variables and Constants](#variables-and-constants)
5. [Functions](#functions)
6. [Control Flow](#control-flow)
7. [Structs and Classes](#structs-and-classes)
8. [Memory Management](#memory-management)
9. [Operators and Expressions](#operators-and-expressions)
10. [Modules and Imports](#modules-and-imports)
11. [Advanced Features](#advanced-features)

## Getting Started

### Installing the Compiler

First, ensure you have the prerequisites:
- LLVM 16 or higher
- CMake 3.14 or higher
- A C++17 compatible compiler

To build the O compiler:

```bash
cd o-compiler
mkdir build
cd build
cmake ..
make
```

This creates the `oc` executable.

### Your First Program

Create a file called `hello.olang`:

```o
import "std/io.olang";

fn main() -> int {
    println("Hello, World!");
    return 0;
}
```

Compile and run it:

```bash
oc hello.olang -o hello
./hello
```

## Basic Syntax

O uses a modern, clean syntax with explicit type annotations:

- Statements end with semicolons (optional in many contexts)
- Curly braces define blocks
- Type annotations use `name: type` syntax
- Function definitions use `fn name(params) -> return_type { body }`

### Comments

```o
// Single-line comment

/*
Multi-line comment
*/
```

## Data Types

O has a strong, static type system with both primitive and composite types.

### Primitive Types

| Type | Description | Size |
|------|-------------|------|
| `int` | Signed 32-bit integer | 4 bytes |
| `float` | 64-bit floating point | 8 bytes |
| `bool` | Boolean value | 1 byte |
| `char` | 8-bit character | 1 byte |
| `byte` | 8-bit unsigned integer | 1 byte |
| `void` | Empty type | 0 bytes |

### Composite Types

#### Arrays
- **Fixed Arrays**: `int[5]` - allocated on the stack
- **Dynamic Arrays**: `new int[5]` - allocated on the heap

```o
// Fixed array
var fixed_arr: int[5] = {1, 2, 3, 4, 5};

// Dynamic array
var dynamic_arr = new int[10];
```

#### Pointers
- **Raw pointers**: `*int`, `*String`
- **Reference operator**: `&variable`
- **Dereference operator**: `*pointer`

```o
var x: int = 42;
var ptr: *int = &x;  // Get address of x
var val: int = *ptr; // Dereference pointer
```

## Variables and Constants

### Variables

Variables are declared with the `var` keyword:

```o
// With explicit type
var age: int = 25;

// With type inference
var name = "Alice";  // Type inferred as String

// Without initialization
var count: int;
count = 10;
```

### Constants

Constants are declared with the `let` keyword and cannot be reassigned:

```o
// With explicit type
let pi: float = 3.14159;

// With type inference
let max_users = 1000;
```

### Scope and Shadowing

Variables follow block scoping rules and can be shadowed in inner scopes:

```o
fn example() -> int {
    var x: int = 5;
    
    {
        var x: int = 10;  // Shadows outer x
        // Here, x is 10
    }
    
    // Here, x is 5 again
    return x;
}
```

## Functions

Functions are defined using the `fn` keyword:

```o
fn greet(name: *String) -> *String {
    var greeting = new String("Hello, ");
    greeting.append(name);
    return greeting;
}
```

### Function Parameters

Parameters use `name: type` syntax:

```o
fn add(a: int, b: int) -> int {
    return a + b;
}

fn process_data(data: *int, size: int) {
    // Process the data array
}
```

### Multiple Return Values

Functions can return tuples (simulated with structs):

```o
struct Result {
    value: int;
    success: bool;
}

fn divide(a: int, b: int) -> Result {
    if (b == 0) {
        return { value = 0, success = false };
    }
    return { value = a / b, success = true };
}
```

## Control Flow

### If-Else Statements

```o
fn check_number(n: int) {
    if (n > 0) {
        println("Positive");
    } else if (n < 0) {
        println("Negative");
    } else {
        println("Zero");
    }
}
```

### Loops

#### While Loop
```o
var i: int = 0;
while (i < 10) {
    print_int(i);
    i = i + 1;
}
```

#### For Loop
```o
for (var i: int = 0; i < 10; i = i + 1) {
    print_int(i);
}
```

### Match Expressions

O supports pattern matching with `match`:

```o
enum Color {
    Red,
    Green,
    Blue
}

fn color_name(c: Color) -> *String {
    match (c) {
        Color.Red => new String("Red"),
        Color.Green => new String("Green"),
        Color.Blue => new String("Blue"),
        _ => new String("Unknown")
    }
}
```

## Structs and Classes

### Structs

Structs are user-defined composite types:

```o
struct Point {
    x: int;
    y: int;
}

// Creating instances
var origin = new Point { x = 0, y = 0 };
var p = new Point { x = 5, y = 10 };

// Accessing fields
var x_coord = p.x;
```

### Methods

Structs can have methods:

```o
struct Rectangle {
    width: int;
    height: int;

    fn area(self) -> int {
        return self.width * self.height;
    }

    fn perimeter(self) -> int {
        return 2 * (self.width + self.height);
    }
}

fn main() -> int {
    var rect = new Rectangle { width = 10, height = 5 };
    var area = rect.area();
    return 0;
}
```

### Classes (with Inheritance)

Classes support inheritance and virtual methods:

```o
class Shape {
    virtual fn area(self) -> float {
        return 0.0;
    }
}

class Circle : Shape {
    radius: float;

    new(r: float) {
        this.radius = r;
    }

    override fn area(self) -> float {
        return 3.14159 * self.radius * self.radius;
    }
}

fn main() -> int {
    var circle = new Circle(5.0);
    var area = circle.area();  // Calls overridden method
    return 0;
}
```

## Memory Management

O uses a hybrid memory management model:

### RAII (Resource Acquisition Is Initialization)

Objects are automatically cleaned up when they go out of scope:

```o
fn example_raii() -> int {
    var dynamic_array = new int[100];  // Allocated on heap
    // ... use array ...
    // Array is automatically freed when function returns
    return 0;
}
```

### Unsafe Blocks

Dangerous operations must be enclosed in `unsafe` blocks:

```o
fn unsafe_example() -> int {
    var ptr = malloc(100);
    unsafe {
        // Dereferencing pointers
        *ptr = 42;
        
        // Manual memory management
        free(ptr);
    }
    return 0;
}
```

### Manual Memory Management

Use `delete` for early freeing (only in unsafe blocks):

```o
fn manual_free() -> int {
    var arr = new int[1000];
    unsafe {
        delete arr;  // Free early
    }
    return 0;
}
```

## Operators and Expressions

### Arithmetic Operators
- `+`, `-`, `*`, `/`, `%` (modulus)

### Comparison Operators
- `==`, `!=`, `<`, `>`, `<=`, `>=`

### Logical Operators
- `&&` (and), `||` (or), `!` (not)

### Assignment Operators
- `=`, `+=`, `-=`, `*=`, `/=`

### Operator Overloading

Structs can overload operators:

```o
struct Vector2 {
    x: float;
    y: float;

    fn op_add(self, other: *Vector2) -> *Vector2 {
        return new Vector2 { x = self.x + other.x, y = self.y + other.y };
    }

    fn op_index(self, idx: int) -> float {
        if (idx == 0) {
            return self.x;
        } else {
            return self.y;
        }
    }
}

fn main() -> int {
    var v1 = new Vector2 { x = 1.0, y = 2.0 };
    var v2 = new Vector2 { x = 3.0, y = 4.0 };
    
    var sum = v1 + v2;  // Uses op_add
    var x_val = v1[0];  // Uses op_index
    
    return 0;
}
```

## Modules and Imports

Use the `import` statement to include other modules:

```o
import "std/io.olang";
import "std/math.olang";
import "my_module.olang";

fn main() -> int {
    println("Using imported functions");
    return 0;
}
```

The compiler searches for modules in:
1. Current directory
2. Directories specified with `-I` flag
3. Standard library path

## Advanced Features

### Generics

Define generic types and functions:

```o
struct Box<T> {
    value: T;

    new(v: T) {
        this.value = v;
    }

    fn get(self) -> T {
        return this.value;
    }
}

fn main() -> int {
    var int_box = new Box<int>(42);
    var str_box = new Box<*String>(new String("Hello"));
    
    var num = int_box.get();
    return 0;
}
```

### Enums

Define enumerated types:

```o
enum Status {
    Pending,
    Success,
    Error
}

fn process_status(s: Status) {
    match (s) {
        Status.Pending => println("Still processing..."),
        Status.Success => println("Success!"),
        Status.Error => println("An error occurred"),
        _ => println("Unknown status")
    }
}
```

### Closures

Anonymous functions that can capture variables from their environment:

```o
fn create_multiplier(factor: int) -> fn(int) -> int {
    return fn(x: int) -> int {
        return x * factor;
    };
}

fn main() -> int {
    var doubler = create_multiplier(2);
    var result = doubler(5);  // Returns 10
    return 0;
}
```

This guide covers the fundamentals of the O language. For more detailed information about specific features, refer to the language specification and standard library documentation.