# O Language Compiler Test Suite

This directory contains a comprehensive test suite for the O programming language compiler.

## Overview

This test suite validates all aspects of the O compiler including basic syntax, advanced features, error handling, and edge cases. All tests are contained in a single executable script for simplicity.

## Running Tests

To run all tests:

```bash
./run_all_tests.sh
```

This will:
1. Compile and run all test cases
2. Report pass/fail status for each test
3. Generate a detailed test report
4. Exit with appropriate status code

## Test Categories

The test suite covers:

- **Basic Syntax**: Variables, functions, return statements
- **Structs**: Struct definitions, constructors, and methods  
- **Control Flow**: If/else, loops, match expressions
- **Memory**: Arrays, pointers, unsafe operations
- **Generics**: Generic struct functionality
- **Operators**: Operator overloading
- **Standard Library**: Import functionality
- **Errors**: Error detection for invalid code
- **Advanced**: Complex language features

## Test Structure

Each test follows this pattern:
- `[category]_[description].olang` for positive tests (should compile successfully)
- Error detection tests verify the compiler properly catches invalid code
- Security feature tests verify memory safety restrictions (e.g., pointer operations outside unsafe blocks)

## Generated Reports

Test reports are saved with timestamps in the format `test_report_YYYYMMDD_HHMMSS.txt`.

## Purpose

This test suite serves to:
- Validate compiler functionality after changes
- Detect regressions automatically
- Document current capabilities and limitations
- Provide examples of language usage