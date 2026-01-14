cmake_minimum_required(VERSION 3.14)
project(O_Compiler)

set(CMAKE_CXX_STANDARD 17)

# Find LLVM (Install LLVM 16+ first)
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Source files
set(SOURCES
    src/main.cpp
    src/Lexer.cpp
    src/CodeGen.cpp
    src/Parser.cpp
    src/CompilerDriver.cpp
    src/SymbolTable.cpp
    src/codegen/CodeGenerator.cpp
    src/codegen/ExpressionCodeGen.cpp
    src/codegen/FunctionCodeGen.cpp
    src/codegen/TypeCodeGen.cpp
    src/codegen/UtilityCodeGen.cpp
    src/codegen/TypeResolver.cpp
    src/codegen/InstantiationManager.cpp
    src/codegen/SemanticWalker.cpp
)

add_executable(oc ${SOURCES})

# Link against LLVM Core libraries
llvm_map_components_to_libnames(llvm_libs core support native)
target_link_libraries(oc ${llvm_libs})