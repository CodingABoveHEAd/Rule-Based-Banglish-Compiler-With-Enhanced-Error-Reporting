/*
 * semantic.h — Semantic Analysis Interface for the Banglish Compiler
 * ==================================================================
 *
 * This header declares the public API for semantic analysis:
 *   - analyze_ast()   — the single entry point
 *   - SemanticResult  — summary of the analysis pass
 *
 * The semantic analyser traverses the AST, builds the symbol table,
 * and reports all detectable semantic errors and warnings.
 *
 * Design:
 *   - Completely AST-driven (no parser/lexer coupling)
 *   - Error messages are in Banglish
 *   - No evaluation or code generation
 */

#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "ast.h"
#include "symbol_table.h"

/* ================================================================== */
/*  Semantic Result                                                    */
/* ================================================================== */
/*  Returned by analyze_ast() to summarise the outcome.                */
/* ================================================================== */

typedef struct {
    int error_count;        /* Number of semantic errors found          */
    int warning_count;      /* Number of semantic warnings found        */
    SymbolTable *symtab;    /* The populated symbol table (caller owns) */
} SemanticResult;

/* ================================================================== */
/*  Public API                                                         */
/* ================================================================== */

/*
 * analyze_ast — perform a complete semantic analysis pass.
 *
 * Traverses the entire AST starting from `root`, builds the symbol
 * table, and reports errors/warnings to `out`.
 *
 * Parameters:
 *   root — the root ASTNode (typically NODE_PROGRAM)
 *   out  — output file for error/warning messages (e.g. output.txt)
 *
 * Returns:
 *   A SemanticResult containing the error/warning counts and the
 *   populated symbol table.  The caller is responsible for calling
 *   symtab_destroy() on result.symtab when done.
 */
SemanticResult analyze_ast(ASTNode *root, FILE *out);

#endif /* SEMANTIC_H */
