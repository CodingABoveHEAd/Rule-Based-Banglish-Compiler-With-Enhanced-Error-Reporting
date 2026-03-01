/*
 * Banglish Compiler - Symbol Table (Header)
 * ------------------------------------------
 * Scoped symbol table using a hash-table per scope level.
 * Supports: insert, lookup, scope enter/exit, unused-variable warnings.
 *
 * Semantic checks enabled:
 *   1.  Undeclared variable usage
 *   2.  Variable redeclaration in same scope
 *   3.  Type mismatch in assignments / expressions
 *   4.  Const variable reassignment
 *   5.  Unused variable detection
 *   6.  Uninitialized variable usage
 *   7.  Void variable declaration
 *   8.  Break/continue outside loop
 *   9.  Return type mismatch (void fn returning value, etc.)
 *   10. Function argument count mismatch
 *   11. Modulus (%) with float operands
 *   12. Non-boolean condition in if/while
 */

#ifndef SYMTAB_H
#define SYMTAB_H

#include <stdio.h>

/* ------------------------------------------------------------------ */
/*  Types                                                              */
/* ------------------------------------------------------------------ */

/* Data types in Banglish */
typedef enum {
    TYPE_PURNO   = 0,   /* int    */
    TYPE_DOSOMIK = 1,   /* float  */
    TYPE_TORKIK  = 2,   /* bool   */
    TYPE_SHUNNO  = 3,   /* void   */
    TYPE_UNKNOWN = 4
} DataType;

/* One symbol entry */
typedef struct Symbol {
    char        name[256];
    DataType    type;
    int         is_const;       /* 1 = dhrubo (const) */
    int         is_initialized; /* 1 = has been assigned a value */
    int         is_used;        /* 1 = referenced after declaration */
    int         is_function;    /* 1 = function definition */
    int         param_count;    /* -1 = not a function */
    int         decl_line;      /* line number of declaration */
    int         scope_level;    /* scope depth (0 = global) */
    struct Symbol *next;        /* hash chain */
} Symbol;

/* Hash table bucket count */
#define SYMTAB_SIZE 211

/* Scope node (linked list stack of scopes) */
typedef struct Scope {
    Symbol        *table[SYMTAB_SIZE];
    int            level;
    struct Scope  *parent;
} Scope;

/* ------------------------------------------------------------------ */
/*  API                                                                */
/* ------------------------------------------------------------------ */

/* Initialize the global (level-0) scope */
void    symtab_init(void);

/* Enter a new nested scope (block / function body) */
void    symtab_enter_scope(void);

/* Exit the current scope; reports unused-variable warnings */
void    symtab_exit_scope(FILE *out);

/* Insert a new symbol.  Returns NULL and prints error if redeclared. */
Symbol *symtab_insert(const char *name, DataType type,
                      int is_const, int is_init, int line, FILE *out);

/* Insert a function symbol */
Symbol *symtab_insert_func(const char *name, DataType ret_type,
                           int param_count, int line, FILE *out);

/* Lookup a symbol starting from current scope upward.
 * Returns NULL if not found (prints "undeclared" error). */
Symbol *symtab_lookup(const char *name);

/* Lookup silently (no error if missing) — used internally */
Symbol *symtab_lookup_quiet(const char *name);

/* Lookup only in the current (innermost) scope */
Symbol *symtab_lookup_current(const char *name);

/* Mark a symbol as used */
void    symtab_mark_used(const char *name);

/* Mark a symbol as initialized */
void    symtab_mark_initialized(const char *name);

/* Get current scope level */
int     symtab_current_level(void);

/* Print the entire symbol table (all scopes) to file */
void    symtab_dump(FILE *out);

/* Cleanup all memory */
void    symtab_free(void);

/* ------------------------------------------------------------------ */
/*  Helpers                                                            */
/* ------------------------------------------------------------------ */

/* Convert DataType enum to Banglish string */
const char *type_to_banglish(DataType t);

/* Convert DataType enum to English string */
const char *type_to_english(DataType t);

/* Check type compatibility; returns 1 if compatible */
int types_compatible(DataType left, DataType right);

#endif /* SYMTAB_H */
