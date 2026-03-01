/*
 * Banglish Compiler - Symbol Table (Implementation)
 * ---------------------------------------------------
 * Hash-table-per-scope with scope stack.
 * All semantic error messages are in Banglish.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symtab.h"

/* ------------------------------------------------------------------ */
/*  Internal state                                                     */
/* ------------------------------------------------------------------ */

static Scope *current_scope = NULL;

/* Counters exposed for summary */
int semantic_errors   = 0;
int semantic_warnings = 0;

/* ------------------------------------------------------------------ */
/*  Hash function (djb2)                                               */
/* ------------------------------------------------------------------ */

static unsigned int hash(const char *str) {
    unsigned int h = 5381;
    int c;
    while ((c = *str++))
        h = ((h << 5) + h) + c;
    return h % SYMTAB_SIZE;
}

/* ------------------------------------------------------------------ */
/*  Scope management                                                   */
/* ------------------------------------------------------------------ */

static Scope *scope_create(int level, Scope *parent) {
    Scope *s = (Scope *)calloc(1, sizeof(Scope));
    if (!s) { fprintf(stderr, "Out of memory\n"); exit(1); }
    s->level  = level;
    s->parent = parent;
    return s;
}

void symtab_init(void) {
    if (current_scope) symtab_free();
    current_scope = scope_create(0, NULL);
}

void symtab_enter_scope(void) {
    int new_level = current_scope ? current_scope->level + 1 : 0;
    Scope *s = scope_create(new_level, current_scope);
    current_scope = s;
}

void symtab_exit_scope(FILE *out) {
    if (!current_scope) return;

    /* --- Report unused variables in this scope --- */
    for (int i = 0; i < SYMTAB_SIZE; i++) {
        Symbol *sym = current_scope->table[i];
        while (sym) {
            if (!sym->is_used && !sym->is_function) {
                fprintf(out,
                    "[SEMANTIC WARNING] '%s' declare kora hoyeche (line %d) "
                    "kintu kothao use hoyni.\n",
                    sym->name, sym->decl_line);
                fprintf(out,
                    "  -> Suggestion: Variable '%s' muche din ba use korun.\n",
                    sym->name);
                semantic_warnings++;
            }
            sym = sym->next;
        }
    }

    /* Pop scope */
    Scope *old = current_scope;
    current_scope = current_scope->parent;

    /* Free symbols in the old scope */
    for (int i = 0; i < SYMTAB_SIZE; i++) {
        Symbol *sym = old->table[i];
        while (sym) {
            Symbol *tmp = sym;
            sym = sym->next;
            free(tmp);
        }
    }
    free(old);
}

int symtab_current_level(void) {
    return current_scope ? current_scope->level : 0;
}

/* ------------------------------------------------------------------ */
/*  Insert                                                             */
/* ------------------------------------------------------------------ */

Symbol *symtab_insert(const char *name, DataType type,
                      int is_const, int is_init, int line, FILE *out)
{
    /* Check redeclaration in current scope only */
    Symbol *existing = symtab_lookup_current(name);
    if (existing) {
        fprintf(out,
            "[SEMANTIC ERROR] '%s' age thekei declare kora ache (line %d). "
            "Abar declare kora jabe na (line %d).\n",
            name, existing->decl_line, line);
        fprintf(out,
            "  -> Suggestion: Onno naam byabohar korun ba age declare kora variable use korun.\n");
        semantic_errors++;
        return NULL;
    }

    unsigned int h = hash(name);
    Symbol *sym = (Symbol *)calloc(1, sizeof(Symbol));
    if (!sym) { fprintf(stderr, "Out of memory\n"); exit(1); }

    strncpy(sym->name, name, 255);
    sym->name[255]      = '\0';
    sym->type           = type;
    sym->is_const       = is_const;
    sym->is_initialized = is_init;
    sym->is_used        = 0;
    sym->is_function    = 0;
    sym->param_count    = -1;
    sym->decl_line      = line;
    sym->scope_level    = current_scope->level;
    sym->next           = current_scope->table[h];
    current_scope->table[h] = sym;

    return sym;
}

Symbol *symtab_insert_func(const char *name, DataType ret_type,
                           int param_count, int line, FILE *out)
{
    Symbol *existing = symtab_lookup_current(name);
    if (existing) {
        fprintf(out,
            "[SEMANTIC ERROR] '%s' function age thekei declare kora ache (line %d). "
            "Abar declare kora jabe na (line %d).\n",
            name, existing->decl_line, line);
        semantic_errors++;
        return NULL;
    }

    unsigned int h = hash(name);
    Symbol *sym = (Symbol *)calloc(1, sizeof(Symbol));
    if (!sym) { fprintf(stderr, "Out of memory\n"); exit(1); }

    strncpy(sym->name, name, 255);
    sym->name[255]      = '\0';
    sym->type           = ret_type;
    sym->is_const       = 0;
    sym->is_initialized = 1;
    sym->is_used        = 0;
    sym->is_function    = 1;
    sym->param_count    = param_count;
    sym->decl_line      = line;
    sym->scope_level    = current_scope->level;
    sym->next           = current_scope->table[h];
    current_scope->table[h] = sym;

    return sym;
}

/* ------------------------------------------------------------------ */
/*  Lookup                                                             */
/* ------------------------------------------------------------------ */

Symbol *symtab_lookup_current(const char *name) {
    if (!current_scope) return NULL;
    unsigned int h = hash(name);
    Symbol *sym = current_scope->table[h];
    while (sym) {
        if (strcmp(sym->name, name) == 0) return sym;
        sym = sym->next;
    }
    return NULL;
}

Symbol *symtab_lookup_quiet(const char *name) {
    Scope *sc = current_scope;
    while (sc) {
        unsigned int h = hash(name);
        Symbol *sym = sc->table[h];
        while (sym) {
            if (strcmp(sym->name, name) == 0) return sym;
            sym = sym->next;
        }
        sc = sc->parent;
    }
    return NULL;
}

Symbol *symtab_lookup(const char *name) {
    return symtab_lookup_quiet(name);
}

/* ------------------------------------------------------------------ */
/*  Mark helpers                                                       */
/* ------------------------------------------------------------------ */

void symtab_mark_used(const char *name) {
    Symbol *sym = symtab_lookup_quiet(name);
    if (sym) sym->is_used = 1;
}

void symtab_mark_initialized(const char *name) {
    Symbol *sym = symtab_lookup_quiet(name);
    if (sym) sym->is_initialized = 1;
}

/* ------------------------------------------------------------------ */
/*  Type helpers                                                       */
/* ------------------------------------------------------------------ */

const char *type_to_banglish(DataType t) {
    switch (t) {
        case TYPE_PURNO:   return "purno";
        case TYPE_DOSOMIK: return "dosomik";
        case TYPE_TORKIK:  return "torkik";
        case TYPE_SHUNNO:  return "shunno";
        default:           return "ojana";   /* unknown */
    }
}

const char *type_to_english(DataType t) {
    switch (t) {
        case TYPE_PURNO:   return "int";
        case TYPE_DOSOMIK: return "float";
        case TYPE_TORKIK:  return "bool";
        case TYPE_SHUNNO:  return "void";
        default:           return "unknown";
    }
}

int types_compatible(DataType left, DataType right) {
    if (left == right) return 1;
    /* purno <-> dosomik implicit promotion */
    if ((left == TYPE_PURNO && right == TYPE_DOSOMIK) ||
        (left == TYPE_DOSOMIK && right == TYPE_PURNO))
        return 1;
    /* torkik can assign from purno (0/1) */
    if (left == TYPE_TORKIK && right == TYPE_PURNO) return 1;
    if (left == TYPE_PURNO && right == TYPE_TORKIK) return 1;
    return 0;
}

/* ------------------------------------------------------------------ */
/*  Dump (print all symbols across all live scopes)                    */
/* ------------------------------------------------------------------ */

void symtab_dump(FILE *out) {
    fprintf(out, "\n===== SYMBOL TABLE =====\n");
    fprintf(out, "%-20s | %-10s | %-6s | %-6s | %-6s | %-6s | %-5s | %s\n",
            "NAME", "TYPE", "CONST", "INIT", "USED", "FUNC", "SCOPE", "LINE");
    fprintf(out, "%-20s | %-10s | %-6s | %-6s | %-6s | %-6s | %-5s | %s\n",
            "--------------------", "----------", "------",
            "------", "------", "------", "-----", "----");

    Scope *sc = current_scope;
    while (sc) {
        for (int i = 0; i < SYMTAB_SIZE; i++) {
            Symbol *sym = sc->table[i];
            while (sym) {
                fprintf(out, "%-20s | %-10s | %-6s | %-6s | %-6s | %-6s | %-5d | %d\n",
                        sym->name,
                        type_to_banglish(sym->type),
                        sym->is_const       ? "yes" : "no",
                        sym->is_initialized ? "yes" : "no",
                        sym->is_used        ? "yes" : "no",
                        sym->is_function    ? "yes" : "no",
                        sym->scope_level,
                        sym->decl_line);
                sym = sym->next;
            }
        }
        sc = sc->parent;
    }
}

/* ------------------------------------------------------------------ */
/*  Cleanup all memory                                                 */
/* ------------------------------------------------------------------ */

void symtab_free(void) {
    while (current_scope) {
        Scope *old = current_scope;
        current_scope = current_scope->parent;
        for (int i = 0; i < SYMTAB_SIZE; i++) {
            Symbol *sym = old->table[i];
            while (sym) {
                Symbol *tmp = sym;
                sym = sym->next;
                free(tmp);
            }
        }
        free(old);
    }
}
