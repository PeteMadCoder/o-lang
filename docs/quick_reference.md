# O Language Quick Reference

## Basic Syntax

### Variable Declaration
```o
var name: type = value;     // With type annotation
var name = value;           // Type inference
let name: type = value;     // Constant with type annotation
let name = value;           // Constant with type inference
```

### Function Definition
```o
fn name(param: type, ...) -> return_type {
    body
}
```

### Control Flow
```o
// If-else
if (condition) {
    // then block
} else {
    // else block
}

// While loop
while (condition) {
    // body
}

// For loop
for (init; condition; increment) {
    // body
}

// Match expression
match (value) {
    pattern1 => expression1,
    pattern2 => expression2,
    _ => default_expression
}
```

## Common Data Types

| Type | Description | Example |
|------|-------------|---------|
| `int` | 32-bit integer | `var x: int = 42;` |
| `float` | 64-bit float | `var pi: float = 3.14;` |
| `bool` | Boolean | `var flag: bool = true;` |
| `char` | Character | `var c: char = 'A';` |
| `*T` | Pointer to T | `var ptr: *int = &x;` |
| `T[N]` | Fixed array | `var arr: int[10];` |
| `new T[N]` | Dynamic array | `var dyn_arr = new int[10];` |

## Memory Management

### RAII (Automatic)
```o
fn example() -> int {
    var arr = new int[100];  // Automatically freed
    // ... use arr ...
    return 0;  // arr freed here
}
```

### Unsafe Operations
```o
unsafe {
    var ptr = malloc(100);
    *ptr = 42;           // Dereference
    free(ptr);           // Manual free
    delete variable;     // Early deletion
}
```

## Structs and Classes

### Struct Definition
```o
struct Point {
    x: int;
    y: int;
    
    fn new(x: int, y: int) -> *Point {
        var p = new Point();
        p.x = x;
        p.y = y;
        return p;
    }
    
    fn distance_from_origin(self) -> float {
        return sqrt_f((self.x * self.x + self.y * self.y) as float);
    }
}
```

### Class Definition
```o
class Shape {
    virtual fn area(self) -> float;
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
```

## Generic Types

### Generic Struct
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
```

## Operators

### Arithmetic
- `+`, `-`, `*`, `/`, `%` (modulus)

### Comparison
- `==`, `!=`, `<`, `>`, `<=`, `>=`

### Logical
- `&&` (and), `||` (or), `!` (not)

### Assignment
- `=`, `+=`, `-=`, `*=`, `/=`

### Access
- `.` (member access), `[]` (indexing), `->` (pointer member access)

## Common Patterns

### String Handling
```o
import "std/string.olang";
import "std/io.olang";

var str = new String("Hello");
str.append(new String(" World"));
println(str.c_str());
```

### Array Operations
```o
// Fixed array
var fixed: int[5] = {1, 2, 3, 4, 5};

// Dynamic array
var dynamic = new int[10];
dynamic[0] = 42;
var len = dynamic.len;  // Length property
```

### Error Handling (Conceptual)
```o
// Using Option or Result types (if available in stdlib)
match (some_operation()) {
    Some(value) => process(value),
    None => handle_error()
}
```

## Standard Library Imports

```o
import "std/io.olang";      // Input/output functions
import "std/math.olang";    // Mathematical functions
import "std/string.olang";  // String operations
import "std/memory.olang";  // Memory management utilities
```

## Compilation

```bash
# Basic compilation
oc file.olang -o executable

# With include paths
oc file.olang -I ./stdlib -o executable

# Compile only (no linking)
oc file.olang -c -o object.obj

# Link with external libraries
oc file.olang -l library_name -o executable
```

## Common Functions

### I/O
- `print(msg: *byte)` - Print without newline
- `println(msg: *byte)` - Print with newline
- `print_int(value: int)` - Print integer

### Math
- `abs(x: int) -> int` - Absolute value
- `min(a: int, b: int) -> int` - Minimum
- `max(a: int, b: int) -> int` - Maximum
- `sqrt_f(x: float) -> float` - Square root
- `sin_f(x: float) -> float` - Sine
- `cos_f(x: float) -> float` - Cosine

### Memory
- `malloc(size: int) -> *byte` - Allocate memory
- `free(ptr: *byte)` - Free memory
- `alloc(size: int) -> *byte` - Safe allocation
- `dealloc(ptr: *byte)` - Safe deallocation