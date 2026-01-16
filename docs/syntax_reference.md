# O Language Syntax Reference

This document provides a comprehensive reference for the O language syntax, covering all language constructs and their usage.

## Lexical Structure

### Keywords

Reserved words that cannot be used as identifiers:

```
break, case, class, const, continue, default, else, enum, extern, false, fn, for, if, import, in, let, loop, match, new, nil, return, self, Self, super, true, type, typeof, unsafe, use, virtual, while, open, override
```

### Identifiers

Identifiers must start with a letter or underscore, followed by letters, digits, or underscores:
- `variable_name`
- `_private_var`
- `camelCase`
- `PascalCase`

### Literals

#### Integer Literals
- Decimal: `42`, `-17`
- Hexadecimal: `0xFF`, `0xab12`
- Octal: `0o755`
- Binary: `0b1010`

#### Floating Point Literals
- Standard: `3.14`, `-2.0`
- Scientific notation: `1.5e10`, `2.3E-4`

#### Character Literals
- Single quotes: `'A'`, `'\n'`, `'\t'`

#### String Literals
- Double quotes: `"Hello, World!"`, `"Line 1\nLine 2"`

#### Boolean Literals
- `true`, `false`

## Type System

### Primitive Types

| Keyword | Description | Size | Default Value |
|---------|-------------|------|---------------|
| `int` | 32-bit signed integer | 4 bytes | 0 |
| `float` | 64-bit floating point | 8 bytes | 0.0 |
| `bool` | Boolean | 1 byte | false |
| `char` | 8-bit character | 1 byte | '\0' |
| `byte` | 8-bit unsigned integer | 1 byte | 0 |
| `void` | No value | 0 bytes | N/A |

### Composite Types

#### Arrays
- **Fixed-size**: `T[N]` where `T` is the element type and `N` is the size
- **Dynamic**: `new T[N]` allocates on the heap

```o
var fixed_arr: int[10];           // Stack-allocated array of 10 ints
var dynamic_arr = new int[10];    // Heap-allocated array of 10 ints
```

#### Pointers
- **Declaration**: `*T` where `T` is the pointed-to type
- **Address-of**: `&variable`
- **Dereference**: `*pointer`

```o
var x: int = 42;
var ptr: *int = &x;    // Get address of x
var val: int = *ptr;   // Dereference pointer
```

#### Structs
```o
struct Point {
    x: int;
    y: int;
}
```

#### Classes
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

#### Generic Types
```o
struct Box<T> {
    value: T;
    
    fn get(self) -> T {
        return self.value;
    }
}
```

## Declarations

### Variables
```o
var identifier: type = initializer;
var identifier: type;  // Uninitialized
var identifier = initializer;  // Type inferred
```

### Constants
```o
let identifier: type = initializer;
let identifier = initializer;  // Type inferred
```

### Functions
```o
fn name(param: type, ...) -> return_type {
    body
}

// Example
fn add(a: int, b: int) -> int {
    return a + b;
}
```

### Structs
```o
struct Name {
    field: type;
    ...
    
    fn method(self, param: type) -> type {
        body
    }
}
```

### Classes
```o
open class BaseClass {
    field: type;
    
    virtual fn method(self) -> type {
        body
    }
}

class DerivedClass : BaseClass {
    override fn method(self) -> type {
        body
    }
}
```

## Expressions

### Primary Expressions
- Identifiers: `x`, `func`
- Literals: `42`, `"hello"`, `true`
- Parenthesized: `(expression)`
- `this` (in methods)

### Postfix Expressions
- Function calls: `f(args)`
- Method calls: `obj.method(args)`
- Field access: `obj.field`
- Array indexing: `arr[index]`
- Pointer member access: `ptr->field`

### Unary Expressions
- Prefix operators: `+expr`, `-expr`, `!expr`, `*expr`, `&expr`

### Binary Expressions
- Multiplicative: `*`, `/`, `%`
- Additive: `+`, `-`
- Relational: `<`, `>`, `<=`, `>=`
- Equality: `==`, `!=`
- Logical AND: `&&`
- Logical OR: `||`
- Assignment: `=`, `+=`, `-=`, `*=`, `/=`

### Conditional Expression
```o
condition ? true_expr : false_expr
```

## Statements

### Declaration Statements
```o
var x: int = 42;
let PI: float = 3.14159;
```

### Expression Statements
```o
x = 42;
func_call();
```

### Compound Statements (Blocks)
```o
{
    statement1;
    statement2;
    ...
}
```

### If Statement
```o
if (condition) {
    then_block;
} else if (condition2) {
    elseif_block;
} else {
    else_block;
}
```

### While Loop
```o
while (condition) {
    body;
}
```

### For Loop
```o
for (init; condition; increment) {
    body;
}
```

### Match Statement
```o
match (expression) {
    pattern1 => expression1,
    pattern2 => expression2,
    _ => default_expression  // Wildcard
}
```

### Return Statement
```o
return expression;  // With value
return;             // Without value (for void functions)
```

### Break and Continue
```o
while (...) {
    if (condition) {
        break;      // Exit loop
    }
    if (condition) {
        continue;   // Skip to next iteration
    }
}
```

### Unsafe Block
```o
unsafe {
    // Potentially unsafe operations
    *ptr = value;
    free(memory);
}
```

## Special Constructs

### Constructor Calls
```o
var obj = new ClassName(args);
var generic_obj = new Box<int>(42);
```

### Method Calls
```o
obj.method(args);
```

### Operator Overloading
```o
struct Vec2 {
    x: float;
    y: float;
    
    fn op_add(self, other: *Vec2) -> *Vec2 {
        return new Vec2 { x = self.x + other.x, y = self.y + other.y };
    }
    
    fn op_index(self, idx: int) -> float {
        if (idx == 0) { return self.x; }
        else { return self.y; }
    }
}
```

### Import Statements
```o
import "module_name";
import "path/to/module";
import "module" as alias;
```

### Cast Expressions
```o
(expression as type)
```

## Memory Management

### RAII (Automatic)
```o
fn example() -> int {
    var arr = new int[100];  // Allocated
    // ... use arr ...
    return 0;  // arr automatically freed
}
```

### Manual (Unsafe)
```o
fn manual() -> int {
    var ptr = malloc(100);
    unsafe {
        // Use ptr
        free(ptr);  // Manual cleanup
    }
    return 0;
}
```

### Early Deallocation
```o
fn early_free() -> int {
    var obj = new SomeClass();
    unsafe {
        delete obj;  // Early deallocation
    }
    return 0;
}
```

## Examples

### Complete Program
```o
import "std/io.olang";
import "std/math.olang";

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
    
    print("Distance: ");
    print_int(dist as int);
    println("");
    
    return 0;
}
```

This reference provides a comprehensive overview of O language syntax and constructs. For more detailed information about specific features, refer to the language specification and standard library documentation.