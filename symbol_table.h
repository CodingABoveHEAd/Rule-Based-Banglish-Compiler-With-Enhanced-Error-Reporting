/*
 * symbol_table.h — Symbol Table Definitions for the Banglish Compiler
 * ====================================================================
 *
 * This header defines:
 *   - SymbolKind   enum   — variable, constant, function, parameter
 *   - DataType     enum   — purno, dosomik, torkik, shunno (void)
 *   - Symbol       struct — one entry in the symbol table
 *   - SymbolTable  struct — a flat array-based symbol table
 *   - Function prototypes for insert, lookup, dump, and cleanup
 *
 * Design notes:
 *   - Flat array with linear search — simple and sufficient for a
 *     lab-level compiler (< 500 symbols).
 *   - The symbol table is built ONLY by traversing the AST; it has
 *     no coupling to the lexer or parser.
 *   - No evaluation logic lives here — this is pure bookkeeping.
 *   - Error messages are in Banglish for consistency with the rest
 *     of the compiler.
 *
 * Constraints:
 *   - No code generation
 *   - No expression evaluation
 *   - No optimisation
 */

#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================== */
/*  Symbol Kind Enumeration                                            */
/* ================================================================== */
/*  Classifies what a symbol represents in the source program.         */
/* ================================================================== */

typedef enum {
    SYM_VARIABLE,       /* Regular variable  (purno x = 10;)          */
    SYM_CONSTANT,       /* Constant variable (dhrubo purno PI = 3;)   */
    SYM_FUNCTION,       /* Function name     (purno kaj add(...))     */
    SYM_PARAMETER,      /* Function parameter (purno x in param list) */
} SymbolKind;

/* ================================================================== */
/*  Data Type Enumeration                                              */
/* ================================================================== */
/*  Maps the Banglish type keywords to an internal enum.               */
/* ================================================================== */

typedef enum {
    TYPE_PURNO,         /* purno   → integer                           */
    TYPE_DOSOMIK,       /* dosomik → float / double                    */
    TYPE_TORKIK,        /* torkik  → boolean                           */
    TYPE_SHUNNO,        /* shunno  → void (functions only)             */
    TYPE_UNKNOWN,       /* Unresolved or error type                    */
} DataType;

/* ================================================================== */
/*  Symbol Structure                                                   */
/* ================================================================== */
/*  One entry in the symbol table.  Stores everything needed for       */
/*  semantic analysis and later evaluation.                            */
/* ================================================================== */

typedef struct {
    char       name[256];        /* Identifier name                    */
    DataType   data_type;        /* Data type (purno / dosomik / …)    */
    SymbolKind kind;             /* variable / constant / function     */

    int        is_initialized;   /* 1 if assigned a value at decl     */
    int        is_const;         /* 1 if declared with dhrubo         */

    /* Optional value fields (for constant folding / evaluation)       */
    int        int_value;        /* Stored integer value               */
    double     float_value;      /* Stored float value                 */

    int        line_declared;    /* Source line of declaration          */
    int        scope_level;      /* 0 = global, 1+ = nested blocks     */
} Symbol;

/* ================================================================== */
/*  Symbol Table Structure                                             */
/* ================================================================== */
/*  A simple dynamically-sized array of Symbol entries.                */
/* ================================================================== */

#define SYMTAB_INIT_CAPACITY 64

typedef struct {
    Symbol *entries;         /* Dynamic array of symbols                */
    int     count;           /* Number of symbols currently stored      */
    int     capacity;        /* Allocated capacity                      */
    int     scope_level;     /* Current nesting depth (0 = global)      */
} SymbolTable;

/* ================================================================== */
/*  Function Prototypes                                                */
/* ================================================================== */

/* ---- Lifecycle ---- */

/* Create and initialise a new (empty) symbol table.                   */
SymbolTable *symtab_create(void);

/* Free all memory owned by the symbol table.                          */
void         symtab_destroy(SymbolTable *tab);

/* ---- Scope management ---- */

/* Enter a new scope (increments scope depth).                         */
void         symtab_enter_scope(SymbolTable *tab);

/* Leave the current scope (decrements depth, removes local symbols).  */
void         symtab_exit_scope(SymbolTable *tab);

/* ---- Core operations ---- */

/*
 * symtab_insert — add a new symbol to the table.
 *
 * Returns 1 on success, 0 if a symbol with the same name already
 * exists at the SAME scope level (redeclaration).
 */
int          symtab_insert(SymbolTable *tab, const char *name,
                           DataType data_type, SymbolKind kind,
                           int is_initialized, int is_const,
                           int line);

/*
 * symtab_lookup — search for a symbol by name.
 *
 * Searches from the current scope outward (inner-to-outer).
 * Returns a pointer to the Symbol if found, or NULL.
 */
Symbol      *symtab_lookup(SymbolTable *tab, const char *name);

/*
 * symtab_lookup_current_scope — search only in the current scope.
 *
 * Used to detect redeclarations within the same block.
 */
Symbol      *symtab_lookup_current_scope(SymbolTable *tab, const char *name);

/* ---- Utilities ---- */

/* Convert a Banglish type-name string to the DataType enum.           */
DataType     str_to_datatype(const char *type_name);

/* Convert a DataType enum to a human-readable Banglish string.        */
const char  *datatype_to_str(DataType dt);

/* Convert a SymbolKind enum to a human-readable Banglish string.      */
const char  *symbolkind_to_str(SymbolKind kind);

/* Mark an existing symbol as initialized.                             */
void         symtab_mark_initialized(Symbol *sym);

/* Print the entire symbol table to a file (for debugging / output).   */
void         symtab_dump(SymbolTable *tab, FILE *fp);

#endif /* SYMBOL_TABLE_H */
