# Rule-Based Banglish Compiler with Enhanced Error Reporting

A beginner-friendly, rule-based compiler for a Bangla-inspired programming language (Banglish). The compiler performs lexical analysis, parsing, AST construction, semantic checks, and optional Banglish-to-C translation and execution.

## 1. Project Overview

This project is a **rule-based Banglish compiler** built using classical compiler design stages.

It takes Banglish source code (`.bl`) and processes it through:

- Lexical analysis (tokenization)
- Syntax analysis (grammar parsing)
- AST generation (program structure)
- Semantic analysis (meaning/type/scope checks)
- C code generation and optional execution

The project also includes **enhanced error reporting**, so new users can understand exactly where and why code fails.

### Purpose

- Help students learn compiler design using a readable Banglish syntax.
- Demonstrate a full compile pipeline from source code to executable output.
- Provide human-readable and meaningful error messages for lexical, syntax, and semantic issues.

## 2. Features

- **Banglish to C translation**
	- Converts valid Banglish programs to C source code.
- **Token generation (Lexical Analysis)**
	- Produces token-level output with lexeme and line number.
- **AST generation (Syntax Analysis)**
	- Builds and prints a readable abstract syntax tree.
- **Enhanced error detection and reporting**
	- Reports lexical, syntax, and semantic issues with suggestions.
- **Code execution via generated C code**
	- Compiles generated C using GCC and runs it automatically in `execute` mode.

## 3. Project Workflow (Pipeline)

The compiler follows this end-to-end flow:

```text
Banglish Source (.bl)
				|
				v
[1] Lexical Analysis (Flex)
		-> Tokens + lexical errors/warnings
				|
				v
[2] Syntax Analysis (Bison)
		-> Parse validation + syntax errors
				|
				v
[3] AST Generation
		-> Structured program tree
				|
				v
[4] Semantic Analysis
		-> Type/scope/const/control-flow checks
				|
				v
[5] C Code Generation (optional by mode)
		-> <source_stem>_generated.c
				|
				v
[6] C Compilation + Execution (execute mode)
		-> <source_stem>_generated.exe
		-> <source_stem>_runtime_output.txt
		-> <source_stem>_c_translation_and_output.txt
				|
				v
[7] Final Output Reports
		-> Main analysis report (chosen output file)
```

### Step-by-step explanation

1. **Input Banglish code**
	 - You provide a `.bl` source file.
2. **Lexical Analysis -> Tokens**
	 - The lexer scans characters and emits tokens like `INT_KW`, `IDENTIFIER`, `INT_LIT`.
3. **Syntax Analysis -> AST**
	 - The parser checks grammar and builds the AST.
4. **Error Handling**
	 - Lexical, syntax, and semantic errors are reported with line information and friendly text.
5. **Translation -> C code**
	 - Valid AST can be translated into equivalent C.
6. **Compilation and Execution**
	 - Generated C is compiled with GCC and then run (in execute mode).
7. **Final Output**
	 - Report files show tokens, summaries, AST, semantic results, generated C, and runtime output.

## 4. Installation Guide

### Prerequisites

You need:

- **Flex** (lexer generator)
- **Bison** (parser generator)
- **GCC** (C compiler)
- A terminal (Git Bash / MSYS2 / Linux shell / WSL)

This project is written in C and does **not** require Node.js or Python.

### Setup Steps

1. Clone repository:

```bash
git clone https://github.com/CodingABoveHEAd/Rule-Based-Banglish-Compiler-With-Enhanced-Error-Reporting.git
cd Rule-Based-Banglish-Compiler-With-Enhanced-Error-Reporting
```

2. Generate lexer and parser sources:

```bash
unalias flex bison 2>/dev/null || true
flex lexer.l
bison -d parser.y
```

3. Build compiler executable:

```bash
gcc lex.yy.c parser.tab.c ast.c semantic.c symbol_table.c codegen.c -o banglish.exe
```

If your GCC requires math linkage, use:

```bash
gcc lex.yy.c parser.tab.c ast.c semantic.c symbol_table.c codegen.c -o banglish.exe -lm
```

## 5. How to Run the Project

### Command format

```bash
./banglish.exe <source-file> [output-file] [mode]
```

### Modes

- `analysis` or `1`: Lexical + syntax + AST + semantic report only
- `generate` or `2`: Analysis + generate C code
- `execute` or `3`: Analysis + generate C + compile and run (default)

### Typical commands

Run analysis only:

```bash
./banglish.exe demo_step3_ast_valid.bl demo_step3_ast_report.txt analysis
```

Run analysis + C generation:

```bash
./banglish.exe test_mode_flags_valid.bl mode_generate_output.txt generate
```

Run full pipeline (analysis + C generation + execution):

```bash
./banglish.exe demo_step5_codegen_execute.bl demo_step5_pipeline_report.txt execute
```

### Where to see outputs

- Main report: the output file you pass (for example `demo_step5_pipeline_report.txt`)
- Generated C: `<input_stem>_generated.c`
- Generated executable: `<input_stem>_generated.exe`
- Runtime output: `<input_stem>_runtime_output.txt`
- Combined C + runtime report: `<input_stem>_c_translation_and_output.txt`

## 6. Example Usage

### Example input (Banglish)

```banglish
purno a = 4;
dosomik b = 1.5;
torkik ok = shotti;

jodi (ok == shotti) {
		dekhao "ok true";
} nahole {
		dekhao "ok false";
}

dekhao a + b;
dekhao 'X';
```

Run:

```bash
./banglish.exe demo_step5_codegen_execute.bl demo_step5_pipeline_report.txt execute
```

### Token preview (short)

```text
INT_KW           | purno                  | line 4
IDENTIFIER       | a                      | line 4
OPERATOR         | =                      | line 4
INT_LIT          | 4                      | line 4
DELIMITER        | ;                      | line 4
```

### AST preview (simplified)

```text
Program
	VarDecl: purno a = 4
	VarDecl: dosomik b = 1.5
	VarDecl: torkik ok = shotti
	IfStmt
	PrintStmt: a + b
	PrintStmt: 'X'
```

### Generated C preview (short)

```c
int main(void)
{
		int a = 4;
		double b = 1.5;
		int ok = 1;
		if ((ok == 1)) {
				printf("%s\n", "ok true");
		} else {
				printf("%s\n", "ok false");
		}
		printf("%g\n", (double)(a + b));
		printf("%c\n", 'X');
		return 0;
}
```

### Final output (runtime sample)

```text
Program started
ok true
0
1
2
two
15
3.5
X
```

## 7. Error Handling (Enhanced Reporting)

Enhanced reporting is available at multiple stages:

- **Lexical errors**: invalid identifiers, malformed numbers, illegal symbols, unterminated strings/comments
- **Syntax errors**: grammar mismatch with line context
- **Semantic errors/warnings**: type/scope/const/control-flow/function misuse

### Common examples

1. Undeclared variable

```text
[SEMANTIC ERROR] Line 14: 'x' ghosona kora hoyni! Byabohar korar age ghosona korun.
```

2. Assignment to constant

```text
[SEMANTIC ERROR] Line 10: Dhrubok 'MAX' er man poribortion kora jabe na!
```

3. Break outside loop/switch

```text
[SEMANTIC ERROR] Line 22: tham (break) shudhu loop ba switch-er vitore byabohar kora jay!
```

4. Unterminated string

```text
LEXICAL ERROR     | Unterminated string         | line N
```

5. Syntax mismatch

```text
[SYNTAX ERROR] Line N: <parser message>
	-> Banglish: '<token>' er kache syntax bhul ache.
```

## 8. Project Structure

```text
.
|- lexer.l                 # Flex lexer rules (tokenization)
|- parser.y                # Bison grammar + main pipeline control
|- ast.h / ast.c           # AST node definitions and printing
|- semantic.h / semantic.c # Semantic checks and diagnostics
|- symbol_table.h/.c       # Symbol table management
|- codegen.h / codegen.c   # Banglish -> C generation and execution helper
|- demo_step*.bl           # Demo Banglish source files per pipeline stage
|- demo_step*_report.txt   # Demo reports
|- run_scripts.txt         # Ready-to-use command sequence
|- test_*.bl               # Additional test programs
```

## 9. Technologies Used

- **C** (core compiler implementation)
- **Flex** (lexer generation)
- **Bison** (parser generation)
- **GCC** (compilation of compiler and generated C code)

## 10. Future Improvements

- Stronger type system and additional semantic validations
- Better recovery strategy for multiple syntax errors in one run
- Optimized C code generation (constant folding, dead code removal)
- More Banglish language features (arrays, structs, richer stdlib)
- Cross-platform runtime abstraction (remove platform-specific assumptions)
- Optional GUI or web-based visualizer for tokens/AST/errors

## 11. Contribution Guidelines

Contributions are welcome.

1. Fork the repository.
2. Create a new feature branch.
3. Make changes with clear commit messages.
4. Add or update test `.bl` files and reports.
5. Open a pull request with:
	 - What changed
	 - Why it changed
	 - How to test it

### Suggested contribution areas

- Grammar improvements in `parser.y`
- Better lexical rules in `lexer.l`
- New semantic diagnostics in `semantic.c`
- Better C output and runtime robustness in `codegen.c`
- Documentation and example programs

## Quick Start (Minimal)

```bash
flex lexer.l
bison -d parser.y
gcc lex.yy.c parser.tab.c ast.c semantic.c symbol_table.c codegen.c -o banglish.exe
./banglish.exe demo_step5_codegen_execute.bl demo_step5_pipeline_report.txt execute
```

Open the generated report files to inspect the full pipeline.
