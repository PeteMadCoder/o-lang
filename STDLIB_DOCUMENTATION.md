# O Language Standard Library Documentation

## Overview
The O Language Standard Library provides essential data structures and utilities for O programs. The library is organized into modules that can be imported using the `import` statement.

---

## Core Modules

### `string.olang` - String Operations

The String class provides dynamic string manipulation with automatic memory management.

#### Class: `String`

**Fields:**
- `*byte buffer` - Internal character buffer
- `int length` - Current string length  
- `int capacity` - Allocated buffer capacity

**Constructors:**
```o
// Create string from C-style string literal
new(*char ptr)

// Create empty string with reserved capacity
new(int cap)
```

**Methods:**

##### `fn append(str: *String)`
Appends another string to this string, automatically resizing if needed.

**Parameters:**
- `str` - String to append

**Example:**
```o
var s1: *String = new String("Hello");
var s2: *String = new String(" World");
s1.append(s2);  // s1 becomes "Hello World"
```

##### `fn c_str() -> *byte`
Returns the internal C-style string buffer for interfacing with C functions.

**Returns:** Pointer to null-terminated character array

**Example:**
```o
var s: *String = new String("Hello");
printf(s.c_str());  // Prints "Hello"
```

**Operator Overloading:**

##### `fn op_add(other: *String) -> *String`
Concatenates two strings using the `+` operator.

**Parameters:**
- `other` - String to concatenate

**Returns:** New String containing concatenated result

**Example:**
```o
var s1: *String = new String("Hello");
var s2: *String = new String(" World!");
var s3: *String = s1 + s2;  // s3 = "Hello World!"
```

##### `fn op_index(idx: int) -> int`
Accesses character at specified index using the `[]` operator.

**Parameters:**
- `idx` - Zero-based character index

**Returns:** ASCII value of character at index

**Example:**
```o
var s: *String = new String("Hello");
var c: int = s[1];  // c = 101 ('e')
```

---

### `std/io.olang` - Input/Output Operations

Provides console I/O functionality with clean, intuitive top-level functions.

#### Functions:

##### `fn print(msg: *byte)`
Prints a string to standard output without newline.

##### `fn println(msg: *byte)`  
Prints a string to standard output with newline.

##### `fn print_int(value: int)`
Prints an integer to standard output.

**Example:**
```o
import "std/io.olang";
import "string.olang";

fn main() -> int {
    var msg: *String = new String("Hello World!");
    println(msg.c_str());
    print_int(42);
    return 0;
}
```

---

### `std/math.olang` - Mathematical Operations

Provides comprehensive mathematical functions with C library integration.

#### Integer Functions:

##### `fn abs(x: int) -> int`
Returns absolute value of integer.

##### `fn min(a: int, b: int) -> int`
Returns minimum of two integers.

##### `fn max(a: int, b: int) -> int`
Returns maximum of two integers.

##### `fn clamp(value: int, min_val: int, max_val: int) -> int`
Clamps value between min and max bounds.

##### `fn sign(x: int) -> int`
Returns sign of integer (-1, 0, or 1).

#### Float Functions:

##### `fn abs_f(x: float) -> float`
Returns absolute value of float.

##### `fn min_f(a: float, b: float) -> float`
Returns minimum of two floats.

##### `fn max_f(a: float, b: float) -> float`
Returns maximum of two floats.

##### `fn clamp_f(value: float, min_val: float, max_val: float) -> float`
Clamps float value between bounds.

##### `fn sign_f(x: float) -> float`
Returns sign of float (-1.0, 0.0, or 1.0).

#### Trigonometric Functions (requires -lm):

##### `fn sin_f(x: float) -> float`
Returns sine of x (radians).

##### `fn cos_f(x: float) -> float`
Returns cosine of x (radians).

##### `fn sqrt_f(x: float) -> float`
Returns square root of x.

##### `fn pow_f(base: float, exp: float) -> float`
Returns base raised to the power of exp.

##### `fn floor_f(x: float) -> float`
Returns largest integer ≤ x.

##### `fn ceil_f(x: float) -> float`
Returns smallest integer ≥ x.

---

### `std/memory.olang` - Memory Management Utilities

Provides safe and efficient memory management functions with C library integration.

#### Memory Allocation:

##### `fn alloc(size: int) -> *byte`
Allocates memory block of specified size.

##### `fn dealloc(ptr: *byte)`
Deallocates previously allocated memory.

#### Memory Operations:

##### `fn copy(dest: *byte, src: *byte, size: int)`
Copies memory from source to destination.

##### `fn set(ptr: *byte, value: int, size: int)`
Sets memory block to specified value.

##### `fn zero(ptr: *byte, size: int)`
Zeros out memory block.

##### `fn compare(ptr1: *byte, ptr2: *byte, size: int) -> int`
Compares two memory blocks. Returns -1, 0, or 1.

#### Safe Memory Operations:

##### `fn safe_copy(dest: *byte, dest_size: int, src: *byte, copy_size: int) -> bool`
Safely copies memory with bounds checking.

##### `fn safe_set(ptr: *byte, buffer_size: int, value: int, set_size: int) -> bool`
Safely sets memory with bounds checking.

#### Memory Utilities:

##### `fn is_aligned(ptr: *byte, alignment: int) -> bool`
Checks if pointer is aligned to specified boundary.

##### `fn align_up(size: int, alignment: int) -> int`
Rounds size up to next alignment boundary.

---

## Usage Examples

### Basic String Operations
```o
import "string.olang";
import "std/io.olang";

fn main() -> int {
    var greeting: *String = new String("Hello");
    var target: *String = new String(" World!");

    // String concatenation with operator overloading
    var message: *String = greeting + target;

    // Print result
    println(message.c_str());

    // Character access with operator overloading
    var first_char: int = message[0];  // 'H' = 72
    print_int(first_char);

    return 0;
}
```

### Memory-Safe String Building
```o
import "string.olang";

fn build_message() -> *String {
    var result: *String = new String("Processing");

    var status: *String = new String(" complete");
    result.append(status);

    return result;  // Automatic memory management
}
```

---

## External C Library Dependencies

The standard library interfaces with these C library functions:

- `strlen(*byte str) -> int` - String length calculation
- `memcpy(*byte dest, *byte src, int n) -> *void` - Memory copying
- `malloc(int size) -> *byte` - Memory allocation  
- `free(*byte ptr)` - Memory deallocation
- `printf(*byte str)` - Formatted output

These must be available during linking (typically provided by libc).

---

## Import System

Use the `import` statement to include standard library modules:

```o
import "std/string.olang";           // String class
import "std/io.olang";          // Console I/O
import "std/math.olang";        // Math functions
import "std/memory.olang";      // Memory utilities
```

The compiler searches for modules in:
1. Current directory
2. Directories specified with `-I` flag
3. Standard library path (still needs to be implemented)

---

## Modern Syntax Reference

The O language uses a modern syntax with explicit type annotations:

*   **Function Parameters:** Parameters are declared with `name: type` syntax (e.g., `fn func(param: int, other: float)`).
*   **Variable Declarations:** Variables are declared with `var name: type` syntax, optionally followed by initialization (e.g., `var x: int = 5;` or `var y: int;` for uninitialized variables).
*   **Constant Declarations:** Constants are declared with `let name: type = value` or `let name = value` (with type inference). Constants must be initialized and cannot be reassigned (e.g., `let pi: float = 3.14159;` or `let count = 10;`).
*   **Return Types:** Function return types are specified with the arrow syntax `-> type`.
