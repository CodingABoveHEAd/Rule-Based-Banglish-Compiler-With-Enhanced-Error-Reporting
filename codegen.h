#ifndef CODEGEN_H
#define CODEGEN_H

#include "ast.h"

/* Generates equivalent C code from AST and writes it to c_output_path.
 * Returns 1 on success, 0 on failure.
 */
int generate_c_code(ASTNode *root, const char *c_output_path, FILE *log);

/* Compiles and runs generated C code.
 * - c_path: generated .c source path
 * - exe_path: compiled executable path
 * - runtime_output_path: program stdout/stderr output path
 * - report_path: combined report containing generated C code + run output
 * - log: compiler pipeline log (output.txt)
 * Returns 1 on successful compile+run, 0 otherwise.
 */
int compile_and_run_c_code(const char *c_path,
                           const char *exe_path,
                           const char *runtime_output_path,
                           const char *report_path,
                           FILE *log);

#endif
