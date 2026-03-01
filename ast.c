/*
 * ast.c — AST Construction and Pretty-Printing for the Banglish Compiler
 * =======================================================================
 *
 * Implements every constructor declared in ast.h, a recursive
 * indented pretty-printer, and a recursive free function.
 *
 * All nodes are heap-allocated with calloc() so that unused fields
 * start as zero / NULL — only the relevant fields are set by each
 * constructor.
 */

#include "ast.h"

/* ================================================================== */
/*  Internal Helpers                                                   */
/* ================================================================== */

/*
 * create_node — allocate and zero-initialise a new AST node.
 * Every public constructor calls this first.
 */
static ASTNode *create_node(NodeType type, int line) {
    ASTNode *n = (ASTNode *)calloc(1, sizeof(ASTNode));
    if (!n) {
        fprintf(stderr, "[AST] Fatal: out of memory at line %d\n", line);
        exit(EXIT_FAILURE);
    }
    n->type    = type;
    n->line_no = line;
    return n;
}

/*
 * safe_str — copy src into a fixed-size dst buffer with truncation.
 */
static void safe_str(char *dst, const char *src, size_t size) {
    if (src) {
        strncpy(dst, src, size - 1);
        dst[size - 1] = '\0';
    }
}

/*
 * print_indent — emit `depth` levels of indentation (2 spaces each).
 */
static void print_indent(FILE *fp, int depth) {
    for (int i = 0; i < depth; i++)
        fprintf(fp, "  ");
}

/*
 * op_to_str — return a human-readable string for an Operator tag.
 */
static const char *op_to_str(Operator op) {
    switch (op) {
        case OP_ADD:        return "+";
        case OP_SUB:        return "-";
        case OP_MUL:        return "*";
        case OP_DIV:        return "/";
        case OP_MOD:        return "%%";
        case OP_LT:         return "<";
        case OP_GT:         return ">";
        case OP_LE:         return "<=";
        case OP_GE:         return ">=";
        case OP_EQ:         return "==";
        case OP_NE:         return "!=";
        case OP_AND:        return "&&";
        case OP_OR:         return "||";
        case OP_NOT:        return "!";
        case OP_NEG:        return "-";
        case OP_INC:        return "++";
        case OP_DEC:        return "--";
        case OP_ASSIGN:     return "=";
        case OP_ADD_ASSIGN: return "+=";
        case OP_SUB_ASSIGN: return "-=";
        case OP_MUL_ASSIGN: return "*=";
        case OP_DIV_ASSIGN: return "/=";
        default:            return "?";
    }
}

/* ================================================================== */
/*  Constructor / Factory Functions                                     */
/* ================================================================== */

/* ------------------------------------------------------------------ */
/*  Top-level / structural                                             */
/* ------------------------------------------------------------------ */

/*
 * make_program — create the root program node.
 *   body → linked list of top-level statements
 */
ASTNode *make_program(ASTNode *stmts) {
    ASTNode *n = create_node(NODE_PROGRAM, 0);
    n->body = stmts;
    return n;
}

/*
 * make_block — create a { … } block node.
 *   body → linked list of statements inside the block
 */
ASTNode *make_block(ASTNode *stmts) {
    ASTNode *n = create_node(NODE_BLOCK, 0);
    n->body = stmts;
    return n;
}

/* ------------------------------------------------------------------ */
/*  Declarations                                                       */
/* ------------------------------------------------------------------ */

/*
 * make_var_decl — e.g.  purno x = 10;   dosomik y;
 *   type_name = "purno"   name = "x"   left = IntLit(10) or NULL
 */
ASTNode *make_var_decl(const char *type, const char *name,
                       ASTNode *init_expr, int line) {
    ASTNode *n = create_node(NODE_VAR_DECL, line);
    safe_str(n->type_name, type, sizeof(n->type_name));
    safe_str(n->name,      name, sizeof(n->name));
    n->left = init_expr;   /* initialiser expression, or NULL */
    return n;
}

/*
 * make_const_decl — e.g.  dhrubo purno PI = 3;
 *   type_name = "purno"   name = "PI"   left = IntLit(3)
 */
ASTNode *make_const_decl(const char *type, const char *name,
                         ASTNode *init_expr, int line) {
    ASTNode *n = create_node(NODE_CONST_DECL, line);
    safe_str(n->type_name, type, sizeof(n->type_name));
    safe_str(n->name,      name, sizeof(n->name));
    n->left = init_expr;
    return n;
}

/* ------------------------------------------------------------------ */
/*  Assignment                                                         */
/* ------------------------------------------------------------------ */

/*
 * make_assign — e.g.  a = expr;   a += expr;   a++;
 *   op   = OP_ASSIGN / OP_ADD_ASSIGN / OP_INC / …
 *   name = target variable
 *   left = rhs expression  (NULL for OP_INC / OP_DEC)
 */
ASTNode *make_assign(Operator op, const char *name,
                     ASTNode *value, int line) {
    ASTNode *n = create_node(NODE_ASSIGN, line);
    n->op = op;
    safe_str(n->name, name, sizeof(n->name));
    n->left = value;
    return n;
}

/* ------------------------------------------------------------------ */
/*  Control flow                                                       */
/* ------------------------------------------------------------------ */

/*
 * make_if_stmt — jodi (cond) { then } nahole { else }
 *   left  = condition expression
 *   body  = then-branch (block)
 *   right = else-branch (block or nested if_stmt, may be NULL)
 */
ASTNode *make_if_stmt(ASTNode *cond, ASTNode *then_body,
                      ASTNode *else_body, int line) {
    ASTNode *n = create_node(NODE_IF, line);
    n->left  = cond;
    n->body  = then_body;
    n->right = else_body;
    return n;
}

/*
 * make_while_stmt — jotokhon (cond) { body }
 *   left = condition     body = loop body
 */
ASTNode *make_while_stmt(ASTNode *cond, ASTNode *body, int line) {
    ASTNode *n = create_node(NODE_WHILE, line);
    n->left = cond;
    n->body = body;
    return n;
}

/*
 * make_for_stmt — ghuro (init; cond; update) { body }
 *   init   = initialiser (decl or assign, may be NULL)
 *   left   = condition expression
 *   update = update expression (may be NULL)
 *   body   = loop body
 */
ASTNode *make_for_stmt(ASTNode *init, ASTNode *cond,
                       ASTNode *update, ASTNode *body, int line) {
    ASTNode *n = create_node(NODE_FOR, line);
    n->init   = init;
    n->left   = cond;
    n->update = update;
    n->body   = body;
    return n;
}

/*
 * make_do_while_stmt — koro { body } jotokhon (cond);
 *   body = loop body     left = condition
 */
ASTNode *make_do_while_stmt(ASTNode *body, ASTNode *cond, int line) {
    ASTNode *n = create_node(NODE_DO_WHILE, line);
    n->body = body;
    n->left = cond;
    return n;
}

/*
 * make_switch_stmt — bachai (expr) { cases }
 *   left = switch expression     body = linked list of case clauses
 */
ASTNode *make_switch_stmt(ASTNode *expr, ASTNode *cases, int line) {
    ASTNode *n = create_node(NODE_SWITCH, line);
    n->left = expr;
    n->body = cases;
    return n;
}

/*
 * make_case_clause — khetre val: stmts
 *   left = case constant expression     body = statement list
 */
ASTNode *make_case_clause(ASTNode *value, ASTNode *stmts, int line) {
    ASTNode *n = create_node(NODE_CASE, line);
    n->left = value;
    n->body = stmts;
    return n;
}

/*
 * make_default_clause — onnothay: stmts
 *   body = statement list
 */
ASTNode *make_default_clause(ASTNode *stmts, int line) {
    ASTNode *n = create_node(NODE_DEFAULT_CASE, line);
    n->body = stmts;
    return n;
}

/* ------------------------------------------------------------------ */
/*  Break / Continue                                                   */
/* ------------------------------------------------------------------ */

ASTNode *make_break_stmt(int line) {
    return create_node(NODE_BREAK, line);
}

ASTNode *make_continue_stmt(int line) {
    return create_node(NODE_CONTINUE, line);
}

/* ------------------------------------------------------------------ */
/*  Functions                                                          */
/* ------------------------------------------------------------------ */

/*
 * make_func_def — purno kaj add(purno x, purno y) { body }
 *   type_name = return type     name = function name
 *   params    = linked list of NODE_PARAM
 *   body      = function body block
 */
ASTNode *make_func_def(const char *ret_type, const char *name,
                       ASTNode *params, ASTNode *body, int line) {
    ASTNode *n = create_node(NODE_FUNC_DEF, line);
    safe_str(n->type_name, ret_type, sizeof(n->type_name));
    safe_str(n->name,      name,     sizeof(n->name));
    n->params = params;
    n->body   = body;
    return n;
}

/*
 * make_param — single formal parameter: purno x
 *   type_name = "purno"    name = "x"
 */
ASTNode *make_param(const char *type, const char *name, int line) {
    ASTNode *n = create_node(NODE_PARAM, line);
    safe_str(n->type_name, type, sizeof(n->type_name));
    safe_str(n->name,      name, sizeof(n->name));
    return n;
}

/*
 * make_return_stmt — ferot;  or  ferot expr;
 *   left = return value expression (NULL for void return)
 */
ASTNode *make_return_stmt(ASTNode *value, int line) {
    ASTNode *n = create_node(NODE_RETURN, line);
    n->left = value;
    return n;
}

/* ------------------------------------------------------------------ */
/*  I/O                                                                */
/* ------------------------------------------------------------------ */

/*
 * make_print_stmt — dekhao expr;
 *   left = expression to print
 */
ASTNode *make_print_stmt(ASTNode *expr, int line) {
    ASTNode *n = create_node(NODE_PRINT, line);
    n->left = expr;
    return n;
}

/*
 * make_input_stmt — neo varname;
 *   name = target variable name
 */
ASTNode *make_input_stmt(const char *name, int line) {
    ASTNode *n = create_node(NODE_INPUT, line);
    safe_str(n->name, name, sizeof(n->name));
    return n;
}

/* ------------------------------------------------------------------ */
/*  Expressions                                                        */
/* ------------------------------------------------------------------ */

/*
 * make_binary_expr — e.g.  a + b,  x == y,  p && q
 *   op = operator     left / right = operands
 */
ASTNode *make_binary_expr(Operator op, ASTNode *left,
                          ASTNode *right, int line) {
    ASTNode *n = create_node(NODE_BINARY_EXPR, line);
    n->op    = op;
    n->left  = left;
    n->right = right;
    return n;
}

/*
 * make_unary_expr — e.g.  !flag,  -x,  i++,  j--
 *   op = operator     left = operand
 */
ASTNode *make_unary_expr(Operator op, ASTNode *operand, int line) {
    ASTNode *n = create_node(NODE_UNARY_EXPR, line);
    n->op   = op;
    n->left = operand;
    return n;
}

/* ---- Literals ---- */

ASTNode *make_int_lit(int value, int line) {
    ASTNode *n = create_node(NODE_INT_LIT, line);
    n->int_val = value;
    return n;
}

ASTNode *make_float_lit(double value, int line) {
    ASTNode *n = create_node(NODE_FLOAT_LIT, line);
    n->float_val = value;
    return n;
}

ASTNode *make_bool_lit(int value, int line) {
    ASTNode *n = create_node(NODE_BOOL_LIT, line);
    n->int_val = value;   /* 1 = shotti (true), 0 = mithya (false) */
    return n;
}

ASTNode *make_string_lit(const char *value, int line) {
    ASTNode *n = create_node(NODE_STRING_LIT, line);
    safe_str(n->string_val, value, sizeof(n->string_val));
    return n;
}

ASTNode *make_char_lit(const char *value, int line) {
    ASTNode *n = create_node(NODE_CHAR_LIT, line);
    safe_str(n->string_val, value, sizeof(n->string_val));
    return n;
}

/* ---- Identifier ---- */

ASTNode *make_identifier(const char *name, int line) {
    ASTNode *n = create_node(NODE_IDENTIFIER, line);
    safe_str(n->name, name, sizeof(n->name));
    return n;
}

/* ---- Function call ---- */

/*
 * make_func_call — e.g.  add(x, y)
 *   name   = function name
 *   params = linked list of argument expressions
 */
ASTNode *make_func_call(const char *name, ASTNode *args, int line) {
    ASTNode *n = create_node(NODE_FUNC_CALL, line);
    safe_str(n->name, name, sizeof(n->name));
    n->params = args;
    return n;
}

/* ---- Expression statement ---- */

ASTNode *make_expr_stmt(ASTNode *expr, int line) {
    ASTNode *n = create_node(NODE_EXPR_STMT, line);
    n->left = expr;
    return n;
}

/* ================================================================== */
/*  List Utility                                                       */
/* ================================================================== */

/*
 * append_node — append `node` at the tail of a singly-linked list
 * that is chained through the `next` pointer.
 * Returns the head of the (possibly new) list.
 */
ASTNode *append_node(ASTNode *list, ASTNode *node) {
    if (!node) return list;          /* nothing to append              */
    if (!list) return node;          /* start a new list               */

    ASTNode *cur = list;
    while (cur->next)
        cur = cur->next;             /* walk to the tail               */
    cur->next = node;
    return list;
}

/* ================================================================== */
/*  Recursive Pretty-Printer                                           */
/* ================================================================== */

/*
 * print_stmt_list — iterate a next-linked list and print each node.
 */
static void print_stmt_list(FILE *fp, ASTNode *head, int depth) {
    for (ASTNode *s = head; s; s = s->next)
        print_ast(fp, s, depth);
}

/*
 * print_ast — recursively print a single AST node (and its children)
 * with `depth` levels of indentation (2 spaces per level).
 */
void print_ast(FILE *fp, ASTNode *node, int depth) {
    if (!node) return;

    switch (node->type) {

    /* ============================================================== */
    /*  Program / Block                                                */
    /* ============================================================== */

    case NODE_PROGRAM:
        print_indent(fp, depth);
        fprintf(fp, "Program\n");
        print_stmt_list(fp, node->body, depth + 1);
        break;

    case NODE_BLOCK:
        print_indent(fp, depth);
        fprintf(fp, "Block\n");
        print_stmt_list(fp, node->body, depth + 1);
        break;

    /* ============================================================== */
    /*  Declarations                                                   */
    /* ============================================================== */

    case NODE_VAR_DECL:
        print_indent(fp, depth);
        fprintf(fp, "VarDecl [line %d]: %s %s\n",
                node->line_no, node->type_name, node->name);
        if (node->left) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Init:\n");
            print_ast(fp, node->left, depth + 2);
        }
        break;

    case NODE_CONST_DECL:
        print_indent(fp, depth);
        fprintf(fp, "ConstDecl [line %d]: dhrubo %s %s\n",
                node->line_no, node->type_name, node->name);
        if (node->left) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Init:\n");
            print_ast(fp, node->left, depth + 2);
        }
        break;

    /* ============================================================== */
    /*  Assignment                                                     */
    /* ============================================================== */

    case NODE_ASSIGN:
        print_indent(fp, depth);
        fprintf(fp, "Assign [line %d]: %s %s\n",
                node->line_no, node->name, op_to_str(node->op));
        if (node->left) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Value:\n");
            print_ast(fp, node->left, depth + 2);
        }
        break;

    /* ============================================================== */
    /*  If / Else                                                      */
    /* ============================================================== */

    case NODE_IF:
        print_indent(fp, depth);
        fprintf(fp, "IfStmt [line %d]\n", node->line_no);

        print_indent(fp, depth + 1);
        fprintf(fp, "Condition:\n");
        print_ast(fp, node->left, depth + 2);

        print_indent(fp, depth + 1);
        fprintf(fp, "Then:\n");
        print_ast(fp, node->body, depth + 2);

        if (node->right) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Else:\n");
            print_ast(fp, node->right, depth + 2);
        }
        break;

    /* ============================================================== */
    /*  Loops                                                          */
    /* ============================================================== */

    case NODE_WHILE:
        print_indent(fp, depth);
        fprintf(fp, "WhileStmt [line %d]\n", node->line_no);

        print_indent(fp, depth + 1);
        fprintf(fp, "Condition:\n");
        print_ast(fp, node->left, depth + 2);

        print_indent(fp, depth + 1);
        fprintf(fp, "Body:\n");
        print_ast(fp, node->body, depth + 2);
        break;

    case NODE_FOR:
        print_indent(fp, depth);
        fprintf(fp, "ForStmt [line %d]\n", node->line_no);

        if (node->init) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Init:\n");
            print_ast(fp, node->init, depth + 2);
        }

        print_indent(fp, depth + 1);
        fprintf(fp, "Condition:\n");
        print_ast(fp, node->left, depth + 2);

        if (node->update) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Update:\n");
            print_ast(fp, node->update, depth + 2);
        }

        print_indent(fp, depth + 1);
        fprintf(fp, "Body:\n");
        print_ast(fp, node->body, depth + 2);
        break;

    case NODE_DO_WHILE:
        print_indent(fp, depth);
        fprintf(fp, "DoWhileStmt [line %d]\n", node->line_no);

        print_indent(fp, depth + 1);
        fprintf(fp, "Body:\n");
        print_ast(fp, node->body, depth + 2);

        print_indent(fp, depth + 1);
        fprintf(fp, "Condition:\n");
        print_ast(fp, node->left, depth + 2);
        break;

    /* ============================================================== */
    /*  Switch / Case                                                  */
    /* ============================================================== */

    case NODE_SWITCH:
        print_indent(fp, depth);
        fprintf(fp, "SwitchStmt [line %d]\n", node->line_no);

        print_indent(fp, depth + 1);
        fprintf(fp, "Expr:\n");
        print_ast(fp, node->left, depth + 2);

        print_indent(fp, depth + 1);
        fprintf(fp, "Cases:\n");
        print_stmt_list(fp, node->body, depth + 2);
        break;

    case NODE_CASE:
        print_indent(fp, depth);
        fprintf(fp, "Case [line %d]\n", node->line_no);

        print_indent(fp, depth + 1);
        fprintf(fp, "Value:\n");
        print_ast(fp, node->left, depth + 2);

        print_indent(fp, depth + 1);
        fprintf(fp, "Body:\n");
        print_stmt_list(fp, node->body, depth + 2);
        break;

    case NODE_DEFAULT_CASE:
        print_indent(fp, depth);
        fprintf(fp, "Default [line %d]\n", node->line_no);

        print_indent(fp, depth + 1);
        fprintf(fp, "Body:\n");
        print_stmt_list(fp, node->body, depth + 2);
        break;

    /* ============================================================== */
    /*  Print / Input                                                  */
    /* ============================================================== */

    case NODE_PRINT:
        print_indent(fp, depth);
        fprintf(fp, "PrintStmt [line %d]\n", node->line_no);
        print_ast(fp, node->left, depth + 1);
        break;

    case NODE_INPUT:
        print_indent(fp, depth);
        fprintf(fp, "InputStmt [line %d]: %s\n", node->line_no, node->name);
        break;

    /* ============================================================== */
    /*  Return / Break / Continue                                      */
    /* ============================================================== */

    case NODE_RETURN:
        print_indent(fp, depth);
        fprintf(fp, "ReturnStmt [line %d]\n", node->line_no);
        if (node->left) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Value:\n");
            print_ast(fp, node->left, depth + 2);
        }
        break;

    case NODE_BREAK:
        print_indent(fp, depth);
        fprintf(fp, "Break [line %d]\n", node->line_no);
        break;

    case NODE_CONTINUE:
        print_indent(fp, depth);
        fprintf(fp, "Continue [line %d]\n", node->line_no);
        break;

    /* ============================================================== */
    /*  Function Definition / Parameter                                */
    /* ============================================================== */

    case NODE_FUNC_DEF:
        print_indent(fp, depth);
        fprintf(fp, "FuncDef [line %d]: %s %s\n",
                node->line_no, node->type_name, node->name);

        if (node->params) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Params:\n");
            print_stmt_list(fp, node->params, depth + 2);
        }

        print_indent(fp, depth + 1);
        fprintf(fp, "Body:\n");
        print_ast(fp, node->body, depth + 2);
        break;

    case NODE_PARAM:
        print_indent(fp, depth);
        fprintf(fp, "Param: %s %s\n", node->type_name, node->name);
        break;

    /* ============================================================== */
    /*  Expression Statement                                           */
    /* ============================================================== */

    case NODE_EXPR_STMT:
        print_indent(fp, depth);
        fprintf(fp, "ExprStmt [line %d]\n", node->line_no);
        print_ast(fp, node->left, depth + 1);
        break;

    /* ============================================================== */
    /*  Binary / Unary Expressions                                     */
    /* ============================================================== */

    case NODE_BINARY_EXPR:
        print_indent(fp, depth);
        fprintf(fp, "BinaryExpr: %s\n", op_to_str(node->op));

        print_indent(fp, depth + 1);
        fprintf(fp, "Left:\n");
        print_ast(fp, node->left, depth + 2);

        print_indent(fp, depth + 1);
        fprintf(fp, "Right:\n");
        print_ast(fp, node->right, depth + 2);
        break;

    case NODE_UNARY_EXPR:
        print_indent(fp, depth);
        fprintf(fp, "UnaryExpr: %s\n", op_to_str(node->op));
        print_ast(fp, node->left, depth + 1);
        break;

    /* ============================================================== */
    /*  Literals                                                       */
    /* ============================================================== */

    case NODE_INT_LIT:
        print_indent(fp, depth);
        fprintf(fp, "IntLit: %d\n", node->int_val);
        break;

    case NODE_FLOAT_LIT:
        print_indent(fp, depth);
        fprintf(fp, "FloatLit: %.6g\n", node->float_val);
        break;

    case NODE_BOOL_LIT:
        print_indent(fp, depth);
        fprintf(fp, "BoolLit: %s\n", node->int_val ? "shotti" : "mithya");
        break;

    case NODE_STRING_LIT:
        print_indent(fp, depth);
        fprintf(fp, "StringLit: \"%s\"\n", node->string_val);
        break;

    case NODE_CHAR_LIT:
        print_indent(fp, depth);
        fprintf(fp, "CharLit: '%s'\n", node->string_val);
        break;

    /* ============================================================== */
    /*  Identifier / Function Call                                     */
    /* ============================================================== */

    case NODE_IDENTIFIER:
        print_indent(fp, depth);
        fprintf(fp, "Ident: %s\n", node->name);
        break;

    case NODE_FUNC_CALL:
        print_indent(fp, depth);
        fprintf(fp, "FuncCall: %s\n", node->name);
        if (node->params) {
            print_indent(fp, depth + 1);
            fprintf(fp, "Args:\n");
            print_stmt_list(fp, node->params, depth + 2);
        }
        break;

    /* ============================================================== */
    /*  Fallback                                                       */
    /* ============================================================== */

    default:
        print_indent(fp, depth);
        fprintf(fp, "<unknown node type %d>\n", node->type);
        break;
    }
}

/* ================================================================== */
/*  Memory Cleanup                                                     */
/* ================================================================== */

/*
 * free_ast — recursively free every node reachable from `node`.
 * Traverses all child and next pointers.
 */
void free_ast(ASTNode *node) {
    if (!node) return;

    free_ast(node->left);
    free_ast(node->right);
    free_ast(node->body);
    free_ast(node->init);
    free_ast(node->update);
    free_ast(node->params);
    free_ast(node->next);

    free(node);
}
