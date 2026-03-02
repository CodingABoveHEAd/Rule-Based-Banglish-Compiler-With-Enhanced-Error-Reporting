/*
 * symbol_table.c — Symbol Table Implementation for the Banglish Compiler
 * ======================================================================
 *
 * Implements the symbol table operations declared in symbol_table.h:
 *   - Create / destroy the table
 *   - Enter / exit scopes
 *   - Insert new symbols (with redeclaration check)
 *   - Lookup by name (full scope chain or current scope only)
 *   - Utility converters and a pretty-print dump
 *
 * All error messages are in Banglish to match the compiler style.
 */

#include "symbol_table.h"

/* ================================================================== */
/*  Lifecycle                                                          */
/* ================================================================== */

/*
 * symtab_create — allocate and initialise an empty symbol table.
 */
SymbolTable *symtab_create(void) {
    SymbolTable *tab = (SymbolTable *)malloc(sizeof(SymbolTable));
    if (!tab) {
        fprintf(stderr, "[SymbolTable] Mrityu: memory allocate korte parini!\n");
        exit(EXIT_FAILURE);
    }
    tab->capacity    = SYMTAB_INIT_CAPACITY;
    tab->count       = 0;
    tab->scope_level = 0;  /* start at global scope */
    tab->entries     = (Symbol *)calloc(tab->capacity, sizeof(Symbol));
    if (!tab->entries) {
        fprintf(stderr, "[SymbolTable] Mrityu: memory allocate korte parini!\n");
        free(tab);
        exit(EXIT_FAILURE);
    }
    return tab;
}

/*
 * symtab_destroy — free all memory owned by the symbol table.
 */
void symtab_destroy(SymbolTable *tab) {
    if (!tab) return;
    free(tab->entries);
    free(tab);
}

/* ================================================================== */
/*  Scope Management                                                   */
/* ================================================================== */

/*
 * symtab_enter_scope — go one level deeper (e.g. entering a block).
 */
void symtab_enter_scope(SymbolTable *tab) {
    tab->scope_level++;
}

/*
 * symtab_exit_scope — leave the current scope.
 *
 * All symbols declared at the current scope level are removed
 * (popped from the end of the array).  This simple strategy works
 * because symbols are always appended and inner scopes are exited
 * before outer ones.
 */
void symtab_exit_scope(SymbolTable *tab) {
    /* Remove all symbols belonging to the current scope level */
    while (tab->count > 0 &&
           tab->entries[tab->count - 1].scope_level == tab->scope_level) {
        tab->count--;
    }
    if (tab->scope_level > 0)
        tab->scope_level--;
}

/* ================================================================== */
/*  Internal Helpers                                                   */
/* ================================================================== */

/*
 * grow_if_needed — double the capacity when the array is full.
 */
static void grow_if_needed(SymbolTable *tab) {
    if (tab->count < tab->capacity) return;

    tab->capacity *= 2;
    tab->entries = (Symbol *)realloc(tab->entries,
                                     tab->capacity * sizeof(Symbol));
    if (!tab->entries) {
        fprintf(stderr, "[SymbolTable] Mrityu: memory barhate parini!\n");
        exit(EXIT_FAILURE);
    }
}

/* ================================================================== */
/*  Core Operations                                                    */
/* ================================================================== */

/*
 * symtab_insert — add a new symbol.
 *
 * Checks for redeclaration at the same scope level first.
 * Returns 1 on success, 0 if a duplicate exists in the same scope.
 */
int symtab_insert(SymbolTable *tab, const char *name,
                  DataType data_type, SymbolKind kind,
                  int is_initialized, int is_const,
                  int line) {

    /* Check for redeclaration in the current scope */
    if (symtab_lookup_current_scope(tab, name) != NULL) {
        return 0;   /* caller will report the error */
    }

    grow_if_needed(tab);

    Symbol *sym = &tab->entries[tab->count];
    memset(sym, 0, sizeof(Symbol));

    strncpy(sym->name, name, sizeof(sym->name) - 1);
    sym->name[sizeof(sym->name) - 1] = '\0';

    sym->data_type      = data_type;
    sym->kind           = kind;
    sym->is_initialized = is_initialized;
    sym->is_const       = is_const;
    sym->line_declared  = line;
    sym->scope_level    = tab->scope_level;

    tab->count++;
    return 1;
}

/*
 * symtab_lookup — search for a symbol by name across ALL scopes.
 *
 * Scans from the end of the array backward so that inner (more recent)
 * scopes shadow outer ones.  Returns NULL if not found.
 */
Symbol *symtab_lookup(SymbolTable *tab, const char *name) {
    for (int i = tab->count - 1; i >= 0; i--) {
        if (strcmp(tab->entries[i].name, name) == 0) {
            return &tab->entries[i];
        }
    }
    return NULL;
}

/*
 * symtab_lookup_current_scope — search ONLY in the current scope level.
 *
 * Used to detect redeclarations within the same block.
 */
Symbol *symtab_lookup_current_scope(SymbolTable *tab, const char *name) {
    for (int i = tab->count - 1; i >= 0; i--) {
        /* Stop when we leave the current scope */
        if (tab->entries[i].scope_level < tab->scope_level)
            break;
        if (tab->entries[i].scope_level == tab->scope_level &&
            strcmp(tab->entries[i].name, name) == 0) {
            return &tab->entries[i];
        }
    }
    return NULL;
}

/* ================================================================== */
/*  Utilities                                                          */
/* ================================================================== */

/*
 * symtab_mark_initialized — update a symbol's initialization status.
 */
void symtab_mark_initialized(Symbol *sym) {
    if (sym) sym->is_initialized = 1;
}

/*
 * str_to_datatype — convert a Banglish type keyword string to enum.
 */
DataType str_to_datatype(const char *type_name) {
    if (!type_name)                          return TYPE_UNKNOWN;
    if (strcmp(type_name, "purno")   == 0)   return TYPE_PURNO;
    if (strcmp(type_name, "dosomik") == 0)   return TYPE_DOSOMIK;
    if (strcmp(type_name, "torkik")  == 0)   return TYPE_TORKIK;
    if (strcmp(type_name, "shunno")  == 0)   return TYPE_SHUNNO;
    return TYPE_UNKNOWN;
}

/*
 * datatype_to_str — convert a DataType enum to a printable string.
 */
const char *datatype_to_str(DataType dt) {
    switch (dt) {
        case TYPE_PURNO:   return "purno";
        case TYPE_DOSOMIK: return "dosomik";
        case TYPE_TORKIK:  return "torkik";
        case TYPE_SHUNNO:  return "shunno";
        default:           return "ojana";    /* "unknown" */
    }
}

/*
 * symbolkind_to_str — convert a SymbolKind enum to a printable string.
 */
const char *symbolkind_to_str(SymbolKind kind) {
    switch (kind) {
        case SYM_VARIABLE:  return "chorocho";    /* variable  */
        case SYM_CONSTANT:  return "dhrubok";     /* constant  */
        case SYM_FUNCTION:  return "kaj";         /* function  */
        case SYM_PARAMETER: return "parameter";   /* parameter */
        default:            return "ojana";       /* unknown   */
    }
}

/* ================================================================== */
/*  Pretty-Print / Dump                                                */
/* ================================================================== */

/*
 * symtab_dump — print the entire symbol table to `fp` in a neat
 * tabular format, suitable for output.txt or debugging.
 */
void symtab_dump(SymbolTable *tab, FILE *fp) {
    fprintf(fp, "\n===== SYMBOL TABLE =====\n\n");

    fprintf(fp, "%-4s | %-20s | %-10s | %-10s | %-6s | %-6s | %-6s\n",
            "No.", "Naam", "Dharon", "Prokar", "Init?", "Const?", "Line");
    fprintf(fp, "%-4s-+-%-20s-+-%-10s-+-%-10s-+-%-6s-+-%-6s-+-%-6s\n",
            "----", "--------------------", "----------",
            "----------", "------", "------", "------");

    for (int i = 0; i < tab->count; i++) {
        Symbol *s = &tab->entries[i];
        fprintf(fp, "%-4d | %-20s | %-10s | %-10s | %-6s | %-6s | %-6d\n",
                i + 1,
                s->name,
                datatype_to_str(s->data_type),
                symbolkind_to_str(s->kind),
                s->is_initialized ? "ha"  : "na",
                s->is_const       ? "ha"  : "na",
                s->line_declared);
    }

    fprintf(fp, "\nMot symbol: %d\n", tab->count);
}
