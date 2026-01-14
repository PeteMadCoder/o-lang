#!/bin/bash
# Compile the simple test
echo "Compiling simple test..."
./o-compiler/build/oc simple_string_test.olang

# Check if compilation succeeded
if [ $? -eq 0 ]; then
    echo "Compilation successful. Running executable..."
    # Run with timeout to prevent hanging
    timeout 10s ./output || echo "Program crashed or timed out"
else
    echo "Compilation failed"
fi