/*
 * semantic.c — Semantic Analysis for the Banglish Compiler
 * =========================================================
 *
 * Performs a single recursive traversal of the AST to:
 *   1. Build the symbol table (variables, constants, functions, params)
 *   2. Detect and report semantic errors and warnings
 *
 * All error/warning messages are in Banglish.
 *
 * Detected errors:
 *   - Punorughosona (redeclaration) of a variable/constant in same scope
 *   - Ghosona chara byabohar (use before declaration)
 *   - Dhrubok-e notun man bosano (assignment to a constant)
 *   - Dhrubok ghosona-y man na deya (constant declared without init)
 *   - Shunno type-er chorocho (void-type variable declaration)
 *   - Kaj-er vitore kaj ghosona (nested function definition)
 *   - Loop-er baire tham/chaliejao (break/continue outside loop)
 *   - Kaj-er baire ferot (return outside function)
 *   - Type mismatch in assignments (basic checks)
 *   - Divided by zero literal detection
 *   - Function call to undeclared function
 *   - Wrong argument count to function
 *
 * Detected warnings:
 *   - Ghosona hoye kintu byabohar hoyni (declared but never used)
 *     — reported at scope exit
 *   - Man na diye chorocho byabohar (use of uninitialized variable)
 *
 * Design:
 *   - Fully AST-driven — no parser or lexer coupling
 *   - No evaluation or code generation
 *   - Clean separation: symbol_table.c handles storage,
 *     semantic.c handles traversal and checking
 */

#include <stdarg.h>
#include "semantic.h"

/* ================================================================== */
/*  Internal State                                                     */
/* ================================================================== */
/*  Passed through recursive calls via a context struct to avoid       */
/*  global variables.                                                  */
/* ================================================================== */

typedef struct {
    SymbolTable *symtab;        /* The symbol table being built        */
    FILE        *out;           /* Output stream for messages          */
    int          error_count;   /* Running tally of errors             */
    int          warning_count; /* Running tally of warnings           */
    int          in_loop;       /* Nesting depth inside loops          */
    int          in_function;   /* Nesting depth inside functions      */
} SemanticCtx;

/* ================================================================== */
/*  Forward Declarations                                               */
/* ================================================================== */

static void analyze_node      (SemanticCtx *ctx, ASTNode *node);
static void analyze_stmt_list (SemanticCtx *ctx, ASTNode *head);
static void analyze_expr      (SemanticCtx *ctx, ASTNode *expr);
static DataType infer_expr_type(SemanticCtx *ctx, ASTNode *expr);

/* ================================================================== */
/*  Error / Warning Reporting Helpers                                  */
/* ================================================================== */

static void sem_error(SemanticCtx *ctx, int line, const char *fmt, ...) {
    ctx->error_count++;
    fprintf(ctx->out, "\n[SEMANTIC ERROR] Line %d: ", line);
    va_list args;
    va_start(args, fmt);
    vfprintf(ctx->out, fmt, args);
    va_end(args);
    fprintf(ctx->out, "\n");

    /* Also print to stderr for console visibility */
    fprintf(stderr, "[SEMANTIC ERROR] Line %d: ", line);
    va_start(args, fmt); //initialize again because vfprintf consumed previous one
    vfprintf(stderr, fmt, args);  //print in console
    va_end(args);
    fprintf(stderr, "\n");
}

static void sem_warning(SemanticCtx *ctx, int line, const char *fmt, ...) {
    ctx->warning_count++;
    fprintf(ctx->out, "\n[SEMANTIC WARNING] Line %d: ", line);
    va_list args;
    va_start(args, fmt);
    vfprintf(ctx->out, fmt, args);
    va_end(args);
    fprintf(ctx->out, "\n");

    fprintf(stderr, "[SEMANTIC WARNING] Line %d: ", line);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
}

/* ================================================================== */
/*  Expression Type Inference (Basic)                                  */
/* ================================================================== */
/*  Returns the DataType of an expression for simple type checking.    */
/*  This is a best-effort inference — not a full type system.          */
/* ================================================================== */

static DataType infer_expr_type(SemanticCtx *ctx, ASTNode *expr) {
    if (!expr) return TYPE_UNKNOWN;

    switch (expr->type) {
        case NODE_INT_LIT:
            return TYPE_PURNO;

        case NODE_FLOAT_LIT:
            return TYPE_DOSOMIK;

        case NODE_BOOL_LIT:
            return TYPE_TORKIK;

        case NODE_STRING_LIT:
        case NODE_CHAR_LIT:
            return TYPE_UNKNOWN;  /* string/char not in our type enum */

        case NODE_IDENTIFIER: {
            Symbol *sym = symtab_lookup(ctx->symtab, expr->name);
            if (sym) return sym->data_type;
            return TYPE_UNKNOWN;
        }

        case NODE_BINARY_EXPR: {
            DataType lt = infer_expr_type(ctx, expr->left);
            DataType rt = infer_expr_type(ctx, expr->right);

            /* Relational / logical operations produce boolean */
            if (expr->op >= OP_LT && expr->op <= OP_NE)
                return TYPE_TORKIK;
            if (expr->op == OP_AND || expr->op == OP_OR)
                return TYPE_TORKIK;

            /* If either operand is float, result is float */
            if (lt == TYPE_DOSOMIK || rt == TYPE_DOSOMIK)
                return TYPE_DOSOMIK;

            /* Otherwise, propagate the known type */
            if (lt != TYPE_UNKNOWN) return lt;
            if (rt != TYPE_UNKNOWN) return rt;
            return TYPE_UNKNOWN;
        }

        case NODE_UNARY_EXPR: {
            if (expr->op == OP_NOT) return TYPE_TORKIK;
            return infer_expr_type(ctx, expr->left);
        }

        case NODE_FUNC_CALL: {
            Symbol *sym = symtab_lookup(ctx->symtab, expr->name);
            if (sym && sym->kind == SYM_FUNCTION)
                return sym->data_type;
            return TYPE_UNKNOWN;
        }

        default:
            return TYPE_UNKNOWN;
    }
}

/* ================================================================== */
/*  Division-by-Zero Check                                             */
/* ================================================================== */

static void check_div_by_zero(SemanticCtx *ctx, ASTNode *expr) {
    if (!expr) return;
    if (expr->type != NODE_BINARY_EXPR) return;
    if (expr->op != OP_DIV && expr->op != OP_MOD) return;

    ASTNode *rhs = expr->right;
    if (rhs && rhs->type == NODE_INT_LIT && rhs->int_val == 0) {
        sem_error(ctx, expr->line_no,
            "Shunno diye bhag kora jabe na! (division by zero)");
    }
    if (rhs && rhs->type == NODE_FLOAT_LIT && rhs->float_val == 0.0) {
        sem_warning(ctx, expr->line_no,
            "Shunno diye bhag — floating-point e somossa hote pare.");
    }
}

/* ================================================================== */
/*  Expression Analysis                                                */
/* ================================================================== */
/*  Checks identifiers for declaration and initialization.             */
/*  Also checks function calls for existence and argument count.       */
/* ================================================================== */

static void analyze_expr(SemanticCtx *ctx, ASTNode *expr) {
    if (!expr) return;

    switch (expr->type) {

    case NODE_IDENTIFIER: {
        Symbol *sym = symtab_lookup(ctx->symtab, expr->name);
        if (!sym) {
            sem_error(ctx, expr->line_no,
                "'%s' ghosona kora hoyni! Byabohar korar age ghosona korun.",
                expr->name);
        } else if (!sym->is_initialized) {
            sem_warning(ctx, expr->line_no,
                "'%s' er kono man deya hoyni kintu byabohar kora hochhe. "
                "Aage man assign korun.",
                expr->name);
        }
        break;
    }

    case NODE_BINARY_EXPR:
        analyze_expr(ctx, expr->left);
        analyze_expr(ctx, expr->right);
        check_div_by_zero(ctx, expr);
        break;

    case NODE_UNARY_EXPR:
        analyze_expr(ctx, expr->left);
        /* For ++ / -- the operand must be an identifier */
        if ((expr->op == OP_INC || expr->op == OP_DEC) &&
            expr->left && expr->left->type == NODE_IDENTIFIER) {
            Symbol *sym = symtab_lookup(ctx->symtab, expr->left->name);
            if (sym && sym->is_const) {
                sem_error(ctx, expr->line_no,
                    "Dhrubok '%s' er upor ++/-- kora jabe na! "
                    "Dhrubok er man poribortonyogyo noy.",
                    expr->left->name);
            }
        }
        break;

    case NODE_FUNC_CALL: {
        Symbol *sym = symtab_lookup(ctx->symtab, expr->name);
        if (!sym) {
            sem_error(ctx, expr->line_no,
                "Kaj (function) '%s' ghosona kora hoyni! "
                "Call korar age function define korun.",
                expr->name);
        } else if (sym->kind != SYM_FUNCTION) {
            sem_error(ctx, expr->line_no,
                "'%s' ekta kaj (function) noy! Eta ekta %s.",
                expr->name, symbolkind_to_str(sym->kind));
        }
        /* Analyze each argument expression */
        for (ASTNode *arg = expr->params; arg; arg = arg->next) {
            analyze_expr(ctx, arg);
        }
        break;
    }

    /* Literals — nothing to check */
    case NODE_INT_LIT:
    case NODE_FLOAT_LIT:
    case NODE_BOOL_LIT:
    case NODE_STRING_LIT:
    case NODE_CHAR_LIT:
        break;

    default:
        break;
    }
}

/* ================================================================== */
/*  Count Parameters (for function argument checking)                  */
/* ================================================================== */

static int count_params(ASTNode *list) {
    int n = 0;
    for (ASTNode *p = list; p; p = p->next)
        n++;
    return n;
}

/* ================================================================== */
/*  Statement / Node Analysis                                          */
/* ================================================================== */
/*  The main recursive traversal function.  Handles every node type.   */
/* ================================================================== */

static void analyze_node(SemanticCtx *ctx, ASTNode *node) {
    if (!node) return;

    switch (node->type) {

    /* ============================================================== */
    /*  Program / Block                                                */
    /* ============================================================== */

    case NODE_PROGRAM:
        analyze_stmt_list(ctx, node->body);
        break;

    case NODE_BLOCK:
        symtab_enter_scope(ctx->symtab);
        analyze_stmt_list(ctx, node->body);
        symtab_exit_scope(ctx->symtab);
        break;

    /* ============================================================== */
    /*  Variable Declaration                                           */
    /* ============================================================== */

    case NODE_VAR_DECL: {
        DataType dt = str_to_datatype(node->type_name);

        /* Error: void variable */
        if (dt == TYPE_SHUNNO) {
            sem_error(ctx, node->line_no,
                "'%s' ke 'shunno' (void) type diye ghosona kora jabe na! "
                "Shunno shudhu function return type hisebe byabohar hoy.",
                node->name);
        }

        /* Error: unknown type */
        if (dt == TYPE_UNKNOWN) {
            sem_error(ctx, node->line_no,
                "'%s' er type '%s' chena jay na! "
                "Baidhho type: purno, dosomik, torkik.",
                node->name, node->type_name);
        }

        /* Analyze the initializer expression (if any) */
        if (node->left) {
            analyze_expr(ctx, node->left);
        }

        int has_init = (node->left != NULL) ? 1 : 0;

        /* Type mismatch check (basic: literal vs declared type) */
        if (has_init && dt != TYPE_UNKNOWN) {
            DataType init_type = infer_expr_type(ctx, node->left);
            if (init_type != TYPE_UNKNOWN && init_type != dt) {
                /* Allow purno <- torkik (boolean is integer-like) */
                if (!(dt == TYPE_PURNO && init_type == TYPE_TORKIK)) {
                    sem_warning(ctx, node->line_no,
                        "'%s' er ghosito type '%s' kintu man er type '%s'. "
                        "Type mismatch hote pare!",
                        node->name,
                        datatype_to_str(dt),
                        datatype_to_str(init_type));
                }
            }
        }

        /* Insert into symbol table (detects redeclaration) */
        if (!symtab_insert(ctx->symtab, node->name, dt, SYM_VARIABLE,
                           has_init, 0, node->line_no)) {
            Symbol *existing = symtab_lookup(ctx->symtab, node->name);
            sem_error(ctx, node->line_no,
                "Punorughosona! '%s' age thekei ghosona kora hoyeche (line %d e). "
                "Eki scope-e eki naam dui bar ghosona kora jabe na!",
                node->name,
                existing ? existing->line_declared : 0);
        }
        break;
    }

    /* ============================================================== */
    /*  Constant Declaration                                           */
    /* ============================================================== */

    case NODE_CONST_DECL: {
        DataType dt = str_to_datatype(node->type_name);

        /* Void constant is nonsensical */
        if (dt == TYPE_SHUNNO) {
            sem_error(ctx, node->line_no,
                "Dhrubok '%s' ke 'shunno' (void) type diye ghosona kora jabe na!",
                node->name);
        }

        /* Constants MUST be initialized */
        if (!node->left) {
            sem_error(ctx, node->line_no,
                "Dhrubok '%s' ghosona korar shomoy aboshshoi man dite hobe! "
                "Udahoron: dhrubo purno %s = 10;",
                node->name, node->name);
        } else {
            analyze_expr(ctx, node->left);
        }

        int has_init = (node->left != NULL) ? 1 : 0;

        /* Type mismatch check */
        if (has_init && dt != TYPE_UNKNOWN) {
            DataType init_type = infer_expr_type(ctx, node->left);
            if (init_type != TYPE_UNKNOWN && init_type != dt) {
                if (!(dt == TYPE_PURNO && init_type == TYPE_TORKIK)) {
                    sem_warning(ctx, node->line_no,
                        "Dhrubok '%s' er ghosito type '%s' kintu man er type '%s'. "
                        "Type mismatch!",
                        node->name,
                        datatype_to_str(dt),
                        datatype_to_str(init_type));
                }
            }
        }

        /* Insert (detects redeclaration) */
        if (!symtab_insert(ctx->symtab, node->name, dt, SYM_CONSTANT,
                           has_init, 1, node->line_no)) {
            Symbol *existing = symtab_lookup(ctx->symtab, node->name);
            sem_error(ctx, node->line_no,
                "Punorughosona! Dhrubok '%s' age thekei ghosona kora hoyeche "
                "(line %d e).",
                node->name,
                existing ? existing->line_declared : 0);
        }
        break;
    }

    /* ============================================================== */
    /*  Assignment                                                     */
    /* ============================================================== */

    case NODE_ASSIGN: {
        Symbol *sym = symtab_lookup(ctx->symtab, node->name);

        if (!sym) {
            sem_error(ctx, node->line_no,
                "'%s' ghosona kora hoyni! Byabohar korar age ghosona korun. "
                "Udahoron: purno %s;",
                node->name, node->name);
        } else {
            /* Constant reassignment check */
            if (sym->is_const) {
                sem_error(ctx, node->line_no,
                    "Dhrubok '%s' er man poribortion kora jabe na! "
                    "Dhrubok (dhrubo diye ghosito) shudhu ekbar man nite pare. "
                    "(Ghosona: line %d)",
                    node->name, sym->line_declared);
            }

            /* Mark as initialized on first assignment */
            if (!sym->is_initialized) {
                symtab_mark_initialized(sym);
            }

            /* Basic type mismatch check on RHS */
            if (node->left && sym->data_type != TYPE_UNKNOWN) {
                DataType rhs_type = infer_expr_type(ctx, node->left);
                if (rhs_type != TYPE_UNKNOWN && rhs_type != sym->data_type) {
                    if (!(sym->data_type == TYPE_PURNO && rhs_type == TYPE_TORKIK)) {
                        sem_warning(ctx, node->line_no,
                            "'%s' er type '%s' kintu assign kora hochhe '%s' type er man. "
                            "Type mismatch hote pare!",
                            node->name,
                            datatype_to_str(sym->data_type),
                            datatype_to_str(rhs_type));
                    }
                }
            }
        }

        /* Analyze the RHS expression */
        if (node->left) {
            analyze_expr(ctx, node->left);
        }
        break;
    }

    /* ============================================================== */
    /*  If / Else                                                      */
    /* ============================================================== */

    case NODE_IF:
        analyze_expr(ctx, node->left);    /* condition */

        /* Warn if condition is a literal (always true/false) */
        if (node->left && (node->left->type == NODE_INT_LIT ||
                           node->left->type == NODE_BOOL_LIT)) {
            sem_warning(ctx, node->line_no,
                "Jodi (if) er condition fixed literal — shobshomoy %s hobe! "
                "Eta ki thik?",
                (node->left->type == NODE_BOOL_LIT)
                    ? (node->left->int_val ? "shotti" : "mithya")
                    : (node->left->int_val ? "shotti" : "mithya"));
        }

        analyze_node(ctx, node->body);    /* then branch */
        if (node->right)
            analyze_node(ctx, node->right);   /* else branch */
        break;

    /* ============================================================== */
    /*  While Loop                                                     */
    /* ============================================================== */

    case NODE_WHILE:
        analyze_expr(ctx, node->left);    /* condition */
        ctx->in_loop++;
        analyze_node(ctx, node->body);
        ctx->in_loop--;
        break;

    /* ============================================================== */
    /*  For Loop                                                       */
    /* ============================================================== */

    case NODE_FOR:
        /*
         * For-loop init may declare a variable (NODE_VAR_DECL)
         * which should live in the loop's own scope.
         */
        symtab_enter_scope(ctx->symtab);

        if (node->init)
            analyze_node(ctx, node->init);    /* init (decl or assign) */
        analyze_expr(ctx, node->left);         /* condition */
        if (node->update)
            analyze_node(ctx, node->update);   /* update */

        ctx->in_loop++;
        analyze_node(ctx, node->body);
        ctx->in_loop--;

        symtab_exit_scope(ctx->symtab);
        break;

    /* ============================================================== */
    /*  Do-While Loop                                                  */
    /* ============================================================== */

    case NODE_DO_WHILE:
        ctx->in_loop++;
        analyze_node(ctx, node->body);
        ctx->in_loop--;
        analyze_expr(ctx, node->left);    /* condition */
        break;

    /* ============================================================== */
    /*  Switch / Case                                                  */
    /* ============================================================== */

    case NODE_SWITCH:
        analyze_expr(ctx, node->left);     /* switch expression */
        ctx->in_loop++;   /* break is valid inside switch */
        analyze_stmt_list(ctx, node->body);  /* case clauses */
        ctx->in_loop--;
        break;

    case NODE_CASE:
        analyze_expr(ctx, node->left);     /* case value */
        analyze_stmt_list(ctx, node->body);
        break;

    case NODE_DEFAULT_CASE:
        analyze_stmt_list(ctx, node->body);
        break;

    /* ============================================================== */
    /*  Print                                                          */
    /* ============================================================== */

    case NODE_PRINT:
        analyze_expr(ctx, node->left);
        break;

    /* ============================================================== */
    /*  Input                                                          */
    /* ============================================================== */

    case NODE_INPUT: {
        Symbol *sym = symtab_lookup(ctx->symtab, node->name);
        if (!sym) {
            sem_error(ctx, node->line_no,
                "neo (input): '%s' ghosona kora hoyni! "
                "Input neoar age variable ghosona korun.",
                node->name);
        } else {
            if (sym->is_const) {
                sem_error(ctx, node->line_no,
                    "neo (input): Dhrubok '%s' e input neya jabe na! "
                    "Dhrubok er man poribortion kora jabe na.",
                    node->name);
            }
            /* Input initializes the variable */
            symtab_mark_initialized(sym);
        }
        break;
    }

    /* ============================================================== */
    /*  Return                                                         */
    /* ============================================================== */

    case NODE_RETURN:
        if (!ctx->in_function) {
            sem_error(ctx, node->line_no,
                "ferot (return) shudhu kaj (function) er vitore byabohar "
                "kora jay! Baire ferot likha jabe na.");
        }
        if (node->left) {
            analyze_expr(ctx, node->left);
        }
        break;

    /* ============================================================== */
    /*  Break / Continue                                               */
    /* ============================================================== */

    case NODE_BREAK:
        if (!ctx->in_loop) {
            sem_error(ctx, node->line_no,
                "tham (break) shudhu loop ba switch-er vitore byabohar "
                "kora jay! Baire tham likha jabe na.");
        }
        break;

    case NODE_CONTINUE:
        if (!ctx->in_loop) {
            sem_error(ctx, node->line_no,
                "chaliejao (continue) shudhu loop-er vitore byabohar "
                "kora jay! Baire chaliejao likha jabe na.");
        }
        break;

    /* ============================================================== */
    /*  Function Definition                                            */
    /* ============================================================== */

    case NODE_FUNC_DEF: {
        /* Nesting check: no functions inside functions */
        if (ctx->in_function) {
            sem_error(ctx, node->line_no,
                "Kaj (function) '%s' er vitore arekta kaj ghosona kora jabe na! "
                "Nested function support nei.",
                node->name);
        }

        DataType ret_type = str_to_datatype(node->type_name);

        /* Insert the function name into the current scope */
        if (!symtab_insert(ctx->symtab, node->name, ret_type, SYM_FUNCTION,
                           1, 0, node->line_no)) {
            Symbol *existing = symtab_lookup(ctx->symtab, node->name);
            sem_error(ctx, node->line_no,
                "Punorughosona! Kaj '%s' age thekei ghosona kora hoyeche "
                "(line %d e).",
                node->name,
                existing ? existing->line_declared : 0);
        }

        /* Enter function scope for parameters and body */
        symtab_enter_scope(ctx->symtab);
        ctx->in_function++;

        /* Register parameters */
        for (ASTNode *p = node->params; p; p = p->next) {
            DataType pt = str_to_datatype(p->type_name);
            if (!symtab_insert(ctx->symtab, p->name, pt, SYM_PARAMETER,
                               1, 0, p->line_no)) {
                sem_error(ctx, p->line_no,
                    "Punorughosona! Parameter '%s' eki function-e duibar "
                    "ghosona kora jabe na!",
                    p->name);
            }
        }

        /* Analyze the body */
        analyze_stmt_list(ctx, node->body ? node->body->body : NULL);

        ctx->in_function--;
        symtab_exit_scope(ctx->symtab);
        break;
    }

    /* ============================================================== */
    /*  Expression Statement                                           */
    /* ============================================================== */

    case NODE_EXPR_STMT:
        analyze_expr(ctx, node->left);
        break;

    /* ============================================================== */
    /*  Catch-all for unary/binary that appear as statements           */
    /* ============================================================== */

    case NODE_UNARY_EXPR:
    case NODE_BINARY_EXPR:
        analyze_expr(ctx, node);
        break;

    /* ============================================================== */
    /*  Nodes that need no semantic checking                           */
    /* ============================================================== */

    case NODE_PARAM:
    case NODE_INT_LIT:
    case NODE_FLOAT_LIT:
    case NODE_BOOL_LIT:
    case NODE_STRING_LIT:
    case NODE_CHAR_LIT:
    case NODE_IDENTIFIER:
    case NODE_FUNC_CALL:
        break;

    default:
        break;
    }
}

/* ================================================================== */
/*  Statement List Traversal                                           */
/* ================================================================== */

static void analyze_stmt_list(SemanticCtx *ctx, ASTNode *head) {
    for (ASTNode *s = head; s; s = s->next) {
        analyze_node(ctx, s);
    }
}

/* ================================================================== */
/*  Public Entry Point                                                 */
/* ================================================================== */

/*
 * analyze_ast — perform the full semantic analysis pass.
 *
 * Creates a symbol table, traverses the AST, reports errors/warnings,
 * and returns a SemanticResult.
 */
SemanticResult analyze_ast(ASTNode *root, FILE *out) {
    SemanticResult result;
    memset(&result, 0, sizeof(result));

    if (!root) {
        result.symtab = symtab_create();
        return result;
    }

    /* Initialise context */
    SemanticCtx ctx;
    ctx.symtab        = symtab_create();
    ctx.out           = out;
    ctx.error_count   = 0;
    ctx.warning_count = 0;
    ctx.in_loop       = 0;
    ctx.in_function   = 0;

    /* Run the analysis */
    analyze_node(&ctx, root);

    /* Pack results */
    result.error_count   = ctx.error_count;
    result.warning_count = ctx.warning_count;
    result.symtab        = ctx.symtab;

    return result;
}
