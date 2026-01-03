#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <optional>
#include "Lexer.h"
#include "AST.h"

// LLVM Includes
#include "llvm/TargetParser/Host.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"

// Forward declarations from CodeGen.cpp
void InitializeModuleAndPassManager();
extern std::unique_ptr<llvm::Module> TheModule;

// Include the Parser logic (assuming Parser.cpp contains the class definition)
#include "Parser.cpp" 

int main(int argc, char** argv) {
    // 1. Initialize LLVM
    InitializeModuleAndPassManager();
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // 2. Read Source Code
    std::string filename = "../tests/source.o"; 
    if (argc > 1) filename = argv[1];

    std::ifstream file(filename);
    if (!file.is_open()) {
        file.open(filename); 
         if (!file.is_open()) {
             std::cerr << "Could not open file: " << filename << "\n";
             return 1;
         }
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source_code = buffer.str();

    std::cout << "Compiling: " << filename << "\n";

    // 3. Parse & Codegen Loop
    Lexer lexer(source_code);
    Parser parser(lexer);

    while (!parser.isEOF()) {
        auto funcAST = parser.ParseDefinition();
        if (funcAST) {
            auto *IR = funcAST->codegen();
            if (!IR) {
                std::cerr << "Codegen failed.\n";
                return 1;
            }
        } else {
            std::cerr << "Parsing failed.\n";
            return 1;
        }
    }

    // 5. Target Setup
    auto TargetTriple = llvm::sys::getDefaultTargetTriple();
    TheModule->setTargetTriple(TargetTriple);
    
    std::string Error;
    auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);
    
    if (!Target) {
        std::cerr << "Error looking up target: " << Error << "\n";
        return 1;
    }
    
    auto CPU = "generic";
    auto Features = "";
    llvm::TargetOptions opt;
    auto RM = std::optional<llvm::Reloc::Model>();
    auto TheTargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
    
    TheModule->setDataLayout(TheTargetMachine->createDataLayout());
    
    // 6. Emit Object File
    std::string objFilename = "output.obj";
    std::error_code EC;
    llvm::raw_fd_ostream dest(objFilename, EC, llvm::sys::fs::OF_None);
    
    if (EC) {
        std::cerr << "Could not open output file: " << EC.message() << "\n";
        return 1;
    }
    
    llvm::legacy::PassManager pass;
    auto FileType = llvm::CodeGenFileType::ObjectFile;
    
    if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        std::cerr << "TargetMachine can't emit a file of this type\n";
        return 1;
    }
    
    pass.run(*TheModule);
    dest.flush();
    
    std::cout << "Object file written to " << objFilename << "\n";
    
    // 7. Link to Executable
    std::string cmd = "clang " + objFilename + " -o output -lm";
    
    std::cout << "Linking: " << cmd << "\n";
    int ret = system(cmd.c_str());
    if (ret != 0) {
        std::cerr << "Linking failed. Ensure 'clang' is installed.\n";
        return 1;
    }
    
    std::cout << "Compilation successful! Run with ./output\n";

    return 0;
}