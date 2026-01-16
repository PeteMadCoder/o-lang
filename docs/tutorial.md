# O Language Tutorial

This tutorial will guide you through practical examples to help you learn the O language step by step.

## Table of Contents
1. [Hello World](#hello-world)
2. [Variables and Basic Types](#variables-and-basic-types)
3. [Functions](#functions)
4. [Control Flow](#control-flow)
5. [Data Structures](#data-structures)
6. [Memory Management](#memory-management)
7. [Object-Oriented Programming](#object-oriented-programming)
8. [Generic Programming](#generic-programming)
9. [Modules and Imports](#modules-and-imports)

## Hello World

Let's start with the classic "Hello, World!" program:

```o
import "std/io.olang";

fn main() -> int {
    println("Hello, World!");
    return 0;
}
```

Save this as `hello.olang` and compile it:
```bash
oc hello.olang -o hello
./hello
```

## Variables and Basic Types

In O, variables are declared with the `var` keyword and constants with `let`:

```o
import "std/io.olang";

fn main() -> int {
    // Variables with explicit types
    var age: int = 25;
    var temperature: float = 98.6;
    var is_sunny: bool = true;
    var grade: char = 'A';
    
    // Variables with type inference
    var name = "Alice";  // Type inferred as String
    var score = 100;     // Type inferred as int
    
    // Constants
    let pi: float = 3.14159;
    let max_attempts = 3;  // Type inferred as int
    
    // Print values
    print("Age: ");
    print_int(age);
    println("");
    
    return 0;
}
```

### Working with Different Types

```o
import "std/io.olang";
import "std/math.olang";

fn main() -> int {
    // Integer operations
    var a: int = 10;
    var b: int = 3;
    
    print_int(a + b);  // Addition: 13
    println("");
    print_int(a - b);  // Subtraction: 7
    println("");
    print_int(a * b);  // Multiplication: 30
    println("");
    print_int(a / b);  // Division: 3 (integer division)
    println("");
    print_int(a % b);  // Modulo: 1
    println("");
    
    // Float operations
    var x: float = 5.5;
    var y: float = 2.0;
    
    // Note: Need to cast to int for print_int
    print_int(sqrt_f(x * y) as int);
    println("");
    
    return 0;
}
```

## Functions

Functions in O are declared with the `fn` keyword:

```o
import "std/io.olang";

// Simple function
fn greet(name: *String) -> *String {
    var greeting = new String("Hello, ");
    greeting.append(name);
    return greeting;
}

// Function with multiple parameters
fn add_numbers(a: int, b: int) -> int {
    return a + b;
}

// Function without return value (returns void implicitly)
fn print_sum(x: int, y: int) {
    var sum = add_numbers(x, y);
    print("Sum: ");
    print_int(sum);
    println("");
}

fn main() -> int {
    var name = new String("World");
    var greeting = greet(name);
    
    println(greeting.c_str());
    
    print_sum(5, 7);
    
    return 0;
}
```

### Recursive Functions

```o
import "std/io.olang";

// Factorial function
fn factorial(n: int) -> int {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

// Fibonacci sequence
fn fibonacci(n: int) -> int {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

fn main() -> int {
    print("Factorial of 5: ");
    print_int(factorial(5));
    println("");
    
    print("Fibonacci of 10: ");
    print_int(fibonacci(10));
    println("");
    
    return 0;
}
```

## Control Flow

### If-Else Statements

```o
import "std/io.olang";

fn check_number(n: int) {
    if (n > 0) {
        println("Positive number");
    } else if (n < 0) {
        println("Negative number");
    } else {
        println("Zero");
    }
}

fn main() -> int {
    check_number(10);
    check_number(-5);
    check_number(0);
    
    return 0;
}
```

### Loops

```o
import "std/io.olang";

fn main() -> int {
    // While loop
    var i: int = 0;
    while (i < 5) {
        print_int(i);
        println("");
        i = i + 1;
    }
    
    // For loop
    for (var j: int = 0; j < 5; j = j + 1) {
        print("For loop: ");
        print_int(j);
        println("");
    }
    
    return 0;
}
```

### Match Expressions

```o
import "std/io.olang";

enum DayOfWeek {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday
}

fn day_type(day: DayOfWeek) -> *String {
    match (day) {
        DayOfWeek.Monday => new String("Start of work week"),
        DayOfWeek.Tuesday => new String("Work day"),
        DayOfWeek.Wednesday => new String("Work day"),
        DayOfWeek.Thursday => new String("Work day"),
        DayOfWeek.Friday => new String("End of work week!"),
        DayOfWeek.Saturday => new String("Weekend!"),
        DayOfWeek.Sunday => new String("Weekend!"),
        _ => new String("Unknown day")
    }
}

fn main() -> int {
    var friday = DayOfWeek.Friday;
    var saturday = DayOfWeek.Saturday;
    
    println(day_type(friday).c_str());
    println(day_type(saturday).c_str());
    
    return 0;
}
```

## Data Structures

### Arrays

```o
import "std/io.olang";

fn main() -> int {
    // Fixed-size array (stack allocated)
    var numbers: int[5] = {1, 2, 3, 4, 5};
    
    print("Fixed array: ");
    for (var i: int = 0; i < 5; i = i + 1) {
        print_int(numbers[i]);
        print(" ");
    }
    println("");
    
    // Dynamic array (heap allocated)
    var dynamic_arr = new int[5];
    for (var i: int = 0; i < 5; i = i + 1) {
        dynamic_arr[i] = i * i;  // Square of index
    }
    
    print("Dynamic array: ");
    for (var i: int = 0; i < 5; i = i + 1) {
        print_int(dynamic_arr[i]);
        print(" ");
    }
    println("");
    
    return 0;
}
```

### Structs

```o
import "std/io.olang";

struct Person {
    name: *String;
    age: int;
    email: *String;
    
    // Constructor
    new(n: *String, a: int, e: *String) {
        this.name = n;
        this.age = a;
        this.email = e;
    }
    
    // Method
    fn introduce(self) -> *String {
        var intro = new String("Hi, I'm ");
        intro.append(self.name);
        intro.append(new String(", age "));
        
        var age_str = new String((self.age as byte) + 48);  // Simple conversion
        intro.append(age_str);
        
        return intro;
    }
}

fn main() -> int {
    var person = new Person(
        new String("Alice"),
        30,
        new String("alice@example.com")
    );
    
    var intro = person.introduce();
    println(intro.c_str());
    
    return 0;
}
```

## Memory Management

### RAII (Automatic)

```o
import "std/io.olang";

fn create_and_use_array() -> int {
    // Array is automatically freed when function returns
    var arr = new int[1000];
    
    // Initialize array
    for (var i: int = 0; i < 1000; i = i + 1) {
        arr[i] = i;
    }
    
    // Use array
    var sum: int = 0;
    for (var i: int = 0; i < 1000; i = i + 1) {
        sum = sum + arr[i];
    }
    
    print("Sum: ");
    print_int(sum);
    println("");
    
    return 0;
}

fn main() -> int {
    create_and_use_array();
    return 0;
}
```

### Unsafe Operations

```o
import "std/io.olang";
import "std/memory.olang";

fn unsafe_example() -> int {
    var ptr = alloc(100);
    
    unsafe {
        // Write to allocated memory
        *ptr = 42;
        *(ptr + 1) = 43;
        
        // Read from memory
        var val1 = *ptr;
        var val2 = *(ptr + 1);
        
        print("Values: ");
        print_int(val1);
        print(" ");
        print_int(val2);
        println("");
        
        // Free memory
        dealloc(ptr);
    }
    
    return 0;
}

fn main() -> int {
    unsafe_example();
    return 0;
}
```

## Object-Oriented Programming

### Classes and Inheritance

```o
import "std/io.olang";

class Animal {
    name: *String;
    
    new(n: *String) {
        this.name = n;
    }
    
    virtual fn speak(self) -> *String {
        return new String("Some generic animal sound");
    }
    
    fn get_name(self) -> *String {
        return self.name;
    }
}

class Dog : Animal {
    breed: *String;
    
    new(n: *String, b: *String) {
        super.new(n);  // Call parent constructor
        this.breed = b;
    }
    
    override fn speak(self) -> *String {
        return new String("Woof! Woof!");
    }
    
    fn get_breed(self) -> *String {
        return self.breed;
    }
}

class Cat : Animal {
    color: *String;
    
    new(n: *String, c: *String) {
        super.new(n);
        this.color = c;
    }
    
    override fn speak(self) -> *String {
        return new String("Meow!");
    }
    
    fn get_color(self) -> *String {
        return self.color;
    }
}

fn make_animal_speak(animal: *Animal) {
    println(animal.speak().c_str());
}

fn main() -> int {
    var dog = new Dog(new String("Buddy"), new String("Golden Retriever"));
    var cat = new Cat(new String("Whiskers"), new String("Orange"));
    
    println(dog.get_name().c_str());
    println(dog.speak().c_str());
    println(dog.get_breed().c_str());
    
    println(cat.get_name().c_str());
    println(cat.speak().c_str());
    println(cat.get_color().c_str());
    
    // Polymorphism
    make_animal_speak(dog as *Animal);
    make_animal_speak(cat as *Animal);
    
    return 0;
}
```

## Generic Programming

### Generic Structs

```o
import "std/io.olang";

struct Box<T> {
    value: T;
    
    new(v: T) {
        this.value = v;
    }
    
    fn get(self) -> T {
        return this.value;
    }
    
    fn set(self, v: T) {
        this.value = v;
    }
}

struct Pair<T, U> {
    first: T;
    second: U;
    
    new(f: T, s: U) {
        this.first = f;
        this.second = s;
    }
}

fn main() -> int {
    // Generic box with integer
    var int_box = new Box<int>(42);
    print("Integer in box: ");
    print_int(int_box.get());
    println("");
    
    // Generic box with string
    var str_box = new Box<*String>(new String("Hello, Generic!"));
    println(str_box.get().c_str());
    
    // Generic pair
    var name_age = new Pair<*String, int>(new String("Alice"), 30);
    println(name_age.first.c_str());
    print("Age: ");
    print_int(name_age.second);
    println("");
    
    return 0;
}
```

### Generic Functions

```o
import "std/io.olang";

fn swap<T>(a: *T, b: *T) {
    unsafe {
        var temp = *a;
        *a = *b;
        *b = temp;
    }
}

fn max<T>(a: T, b: T) -> T {
    if (a > b) {
        return a;
    }
    return b;
}

fn main() -> int {
    var x: int = 5;
    var y: int = 10;
    
    print("Before swap: x=");
    print_int(x);
    print(", y=");
    print_int(y);
    println("");
    
    unsafe {
        swap(&x, &y);
    }
    
    print("After swap: x=");
    print_int(x);
    print(", y=");
    print_int(y);
    println("");
    
    var larger = max(15, 20);
    print("Max of 15 and 20: ");
    print_int(larger);
    println("");
    
    return 0;
}
```

## Modules and Imports

### Creating a Module

Create a file called `math_utils.olang`:

```o
// math_utils.olang
import "std/io.olang";

// Utility function
fn square(x: int) -> int {
    return x * x;
}

// Another utility function
fn cube(x: int) -> int {
    return x * x * x;
}

// Constant
let MAX_VALUE: int = 1000;
```

### Using the Module

```o
// main.olang
import "std/io.olang";
import "math_utils.olang";  // Import our custom module

fn main() -> int {
    var num: int = 5;
    
    print_int(num);
    print(" squared is ");
    print_int(square(num));
    println("");
    
    print_int(num);
    print(" cubed is ");
    print_int(cube(num));
    println("");
    
    print("Max value constant: ");
    print_int(MAX_VALUE);
    println("");
    
    return 0;
}
```

To compile with the module:
```bash
oc main.olang -I . -o main
```

## Complete Example: A Simple Calculator

Here's a more comprehensive example that puts together many concepts:

```o
// calculator.olang
import "std/io.olang";
import "std/math.olang";

enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide
}

struct Calculator {
    last_result: float;
    
    new() {
        this.last_result = 0.0;
    }
    
    fn calculate(self, op: Operation, a: float, b: float) -> float {
        var result: float = 0.0;
        
        match (op) {
            Operation.Add => result = a + b,
            Operation.Subtract => result = a - b,
            Operation.Multiply => result = a * b,
            Operation.Divide => {
                if (b != 0.0) {
                    result = a / b;
                } else {
                    println("Error: Division by zero!");
                    result = 0.0;
                }
            },
            _ => {
                println("Error: Unknown operation!");
                result = 0.0;
            }
        }
        
        self.last_result = result;
        return result;
    }
    
    fn get_last_result(self) -> float {
        return self.last_result;
    }
}

fn print_operation(op: Operation) {
    match (op) {
        Operation.Add => print("+"),
        Operation.Subtract => print("-"),
        Operation.Multiply => print("*"),
        Operation.Divide => print("/"),
        _ => print("?")
    }
}

fn main() -> int {
    var calc = new Calculator();
    
    var ops = {Operation.Add, Operation.Subtract, Operation.Multiply, Operation.Divide};
    var values = {{10.0, 5.0}, {20.0, 4.0}, {6.0, 7.0}, {15.0, 3.0}};
    
    for (var i: int = 0; i < 4; i = i + 1) {
        var result = calc.calculate(ops[i], values[i][0], values[i][1]);
        
        print(values[i][0] as int);
        print(" ");
        print_operation(ops[i]);
        print(" ");
        print(values[i][1] as int);
        print(" = ");
        print_int(result as int);
        println("");
    }
    
    print("Last result: ");
    print_int(calc.get_last_result() as int);
    println("");
    
    return 0;
}
```

This tutorial covered the basics of the O language. Practice with these examples and gradually build more complex programs to become proficient with the language!

For more advanced topics and detailed specifications, refer to the language specification and standard library documentation.