#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <optional>

// LLVM Includes
#include "llvm/TargetParser/Host.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"

// Forward declarations from CodeGen.cpp
void InitializeModuleAndPassManager();
extern std::unique_ptr<llvm::Module> TheModule;

#include "CompilerDriver.h"

// Forward declarations for three-phase architecture
void processDeferredInstantiations();
void semanticDiscoveryPhase();
void validationPhase();
void codeGenerationPhase();

namespace cl = llvm::cl;

// Command Line Options
cl::opt<std::string> InputFilename(cl::Positional, cl::desc("<input file>"), cl::Required);
cl::opt<std::string> OutputFilename("o", cl::desc("Specify output filename"), cl::value_desc("filename"));
cl::list<std::string> IncludePaths("I", cl::desc("Add directory to include search path"), cl::value_desc("directory"), cl::Prefix);
cl::list<std::string> LinkLibs("l", cl::desc("Link external library"), cl::value_desc("library"), cl::Prefix);
cl::opt<bool> CompileOnly("c", cl::desc("Compile only (do not link)"), cl::init(false));

int main(int argc, char** argv) {
    // 0. Parse Arguments
    cl::ParseCommandLineOptions(argc, argv, "O Compiler\n");

    // 1. Initialize LLVM
    InitializeModuleAndPassManager();
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    // 2. Configure Driver
    CompilerDriver driver;
    // Add default include paths (optional, but good for UX)
    driver.addIncludePath(".");
    driver.addIncludePath("tests");
    driver.addIncludePath("o-compiler/tests");
    driver.addIncludePath("../stdlib");
    driver.addIncludePath("stdlib");
    driver.addIncludePath("../../stdlib");
    
    for (const auto& path : IncludePaths) {
        driver.addIncludePath(path);
    }

    std::cout << "Compiling: " << InputFilename << "\n";

    // 3. Process File
    driver.processFile(InputFilename);

    // 4. NEW THREE-PHASE ARCHITECTURE
    // Phase 1: Semantic Discovery - Discover all required instantiations
    semanticDiscoveryPhase();

    // Phase 2: Validation - Validate the semantic graph
    validationPhase();

    // Phase 3: Code Generation - Generate LLVM IR for all discovered elements
    codeGenerationPhase();

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
    
    // Determine Output Filenames
    std::string FinalOutput = OutputFilename.empty() ? "output" : OutputFilename.getValue();
    std::string ObjFilename = FinalOutput + ".obj";
    if (CompileOnly && !OutputFilename.empty()) {
        ObjFilename = FinalOutput; // If -c -o foo.o, use foo.o
    }
    
    // 6. Emit Object File
    std::error_code EC;
    llvm::raw_fd_ostream dest(ObjFilename, EC, llvm::sys::fs::OF_None);
    
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
    dest.close(); // Close before linking
    
    std::cout << "Object file written to " << ObjFilename << "\n";
    
    if (CompileOnly) {
        return 0;
    }

    // 7. Link to Executable
    std::string cmd = "clang -no-pie " + ObjFilename + " -o " + FinalOutput + " -lm";
    
    // Add libraries
    for (const auto& lib : LinkLibs) {
        cmd += " -l" + lib;
    }
    
    std::cout << "Linking: " << cmd << "\n";
    int ret = system(cmd.c_str());
    if (ret != 0) {
        std::cerr << "Linking failed. Ensure 'clang' is installed.\n";
        return 1;
    }
    
    // Clean up temporary object file if we didn't ask for it specifically? 
    // Usually compilers keep .o files only if requested. But for now keep it.
    // To match gcc/clang behavior: if linking, delete temp .o unless -save-temps.
    // But let's keep it simple.

    std::cout << "Build successful! -> " << FinalOutput << "\n";

    return 0;
}