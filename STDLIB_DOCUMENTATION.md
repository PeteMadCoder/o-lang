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

##### `fn append(*String str)`
Appends another string to this string, automatically resizing if needed.

**Parameters:**
- `str` - String to append

**Example:**
```o
var s1 = new String("Hello");
var s2 = new String(" World");
s1.append(s2);  // s1 becomes "Hello World"
```

##### `fn c_str() -> *byte`
Returns the internal C-style string buffer for interfacing with C functions.

**Returns:** Pointer to null-terminated character array

**Example:**
```o
var s = new String("Hello");
printf(s.c_str());  // Prints "Hello"
```

**Operator Overloading:**

##### `fn op_add(*String other) -> *String`
Concatenates two strings using the `+` operator.

**Parameters:**
- `other` - String to concatenate

**Returns:** New String containing concatenated result

**Example:**
```o
var s1 = new String("Hello");
var s2 = new String(" World!");
var s3 = s1 + s2;  // s3 = "Hello World!"
```

##### `fn op_index(int idx) -> int`
Accesses character at specified index using the `[]` operator.

**Parameters:**
- `idx` - Zero-based character index

**Returns:** ASCII value of character at index

**Example:**
```o
var s = new String("Hello");
var c = s[1];  // c = 101 ('e')
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
    var msg = new String("Hello World!");
    println(msg.c_str());
    print_int(42);
    return 0;
}
```

---

### `std/math.olang` - Mathematical Operations

Provides comprehensive mathematical functions with C library integration.

#### Integer Functions:

##### `fn abs(int x) -> int`
Returns absolute value of integer.

##### `fn min(int a, int b) -> int` 
Returns minimum of two integers.

##### `fn max(int a, int b) -> int`
Returns maximum of two integers.

##### `fn clamp(int value, int min_val, int max_val) -> int`
Clamps value between min and max bounds.

##### `fn sign(int x) -> int`
Returns sign of integer (-1, 0, or 1).

#### Float Functions:

##### `fn abs_f(float x) -> float`
Returns absolute value of float.

##### `fn min_f(float a, float b) -> float`
Returns minimum of two floats.

##### `fn max_f(float a, float b) -> float`
Returns maximum of two floats.

##### `fn clamp_f(float value, float min_val, float max_val) -> float`
Clamps float value between bounds.

##### `fn sign_f(float x) -> float`
Returns sign of float (-1.0, 0.0, or 1.0).

#### Trigonometric Functions (requires -lm):

##### `fn sin_f(float x) -> float`
Returns sine of x (radians).

##### `fn cos_f(float x) -> float`
Returns cosine of x (radians).

##### `fn sqrt_f(float x) -> float`
Returns square root of x.

##### `fn pow_f(float base, float exp) -> float`
Returns base raised to the power of exp.

##### `fn floor_f(float x) -> float`
Returns largest integer ≤ x.

##### `fn ceil_f(float x) -> float`
Returns smallest integer ≥ x.

---

### `std/memory.olang` - Memory Management Utilities

Provides safe and efficient memory management functions with C library integration.

#### Memory Allocation:

##### `fn alloc(int size) -> *byte`
Allocates memory block of specified size.

##### `fn dealloc(*byte ptr)`
Deallocates previously allocated memory.

#### Memory Operations:

##### `fn copy(*byte dest, *byte src, int size)`
Copies memory from source to destination.

##### `fn set(*byte ptr, int value, int size)`
Sets memory block to specified value.

##### `fn zero(*byte ptr, int size)`
Zeros out memory block.

##### `fn compare(*byte ptr1, *byte ptr2, int size) -> int`
Compares two memory blocks. Returns -1, 0, or 1.

#### Safe Memory Operations:

##### `fn safe_copy(*byte dest, int dest_size, *byte src, int copy_size) -> bool`
Safely copies memory with bounds checking.

##### `fn safe_set(*byte ptr, int buffer_size, int value, int set_size) -> bool`
Safely sets memory with bounds checking.

#### Memory Utilities:

##### `fn is_aligned(*byte ptr, int alignment) -> bool`
Checks if pointer is aligned to specified boundary.

##### `fn align_up(int size, int alignment) -> int`
Rounds size up to next alignment boundary.

---

## Usage Examples

### Basic String Operations
```o
import "string.olang";
import "std/io.olang";

fn main() -> int {
    var greeting = new String("Hello");
    var target = new String(" World!");
    
    // String concatenation with operator overloading
    var message = greeting + target;
    
    // Print result
    println(message.c_str());
    
    // Character access with operator overloading  
    var first_char = message[0];  // 'H' = 72
    print_int(first_char);
    
    return 0;
}
```

### Memory-Safe String Building
```o
import "string.olang";

fn build_message() -> *String {
    var result = new String("Processing");
    
    var status = new String(" complete");
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
import "string.olang";           // String class
import "std/io.olang";          // Console I/O
import "std/math.olang";        // Math functions
import "std/memory.olang";      // Memory utilities
```

The compiler searches for modules in:
1. Current directory
2. Directories specified with `-I` flag
3. Standard library path (still needs to be implemented)
