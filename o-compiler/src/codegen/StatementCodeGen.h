#pragma once

#include "CodeGenerator.h"

class StatementCodeGen {
private:
    CodeGenerator &codeGen;

public:
    explicit StatementCodeGen(CodeGenerator &cg) : codeGen(cg) {}

    // Currently, statements are handled as expressions in this language
    // This module can be expanded as needed
};