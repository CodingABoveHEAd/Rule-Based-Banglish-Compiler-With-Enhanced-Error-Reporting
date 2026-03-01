/*
 * ast.h — Abstract Syntax Tree Definitions for the Banglish Compiler
 * ===================================================================
 *
 * This header defines:
 *   - NodeType   enum   — tag for every AST node kind
 *   - Operator   enum   — tag for operators (arithmetic, relational, …)
 *   - ASTNode    struct — the universal AST node (flat layout)
 *   - Constructor prototypes for building the tree
 *   - Pretty-printer and memory-cleanup prototypes
 *
 * Design note:
 *   A single flat struct is used for all node kinds.  Each constructor
 *   populates only the relevant fields; the rest are zero-initialised
 *   by calloc().  This keeps the API simple and highly readable — ideal
 *   for a university compiler-lab front end.
 *
 * Constraints:
 *   - No symbol-table logic
 *   - No expression evaluation
 *   - No code generation
 *   This is a pure AST module for the compiler front end.
 */

#ifndef AST_H
#define AST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================== */
/*  Node Type Enumeration                                              */
/* ================================================================== */
/*  Every AST node carries exactly one of these tags to identify the   */
/*  language construct it represents.                                  */
/* ================================================================== */

typedef enum {

    /* ---- Top-level / structural ---- */
    NODE_PROGRAM,         /* Root of the entire program                */
    NODE_BLOCK,           /* Brace-enclosed block  { … }              */

    /* ---- Declarations ---- */
    NODE_VAR_DECL,        /* Variable declaration  (purno x = 10;)    */
    NODE_CONST_DECL,      /* Constant declaration  (dhrubo purno N=5) */

    /* ---- Statements ---- */
    NODE_ASSIGN,          /* Assignment  (=, +=, -=, *=, /=, ++, --)  */
    NODE_IF,              /* if / if-else / if-else-if                 */
    NODE_WHILE,           /* while loop  (jotokhon)                    */
    NODE_FOR,             /* for loop    (ghuro)                       */
    NODE_DO_WHILE,        /* do-while    (koro … jotokhon)             */
    NODE_SWITCH,          /* switch      (bachai)                      */
    NODE_CASE,            /* case clause (khetre)                      */
    NODE_DEFAULT_CASE,    /* default clause (onnothay)                 */
    NODE_PRINT,           /* print       (dekhao)                      */
    NODE_INPUT,           /* input       (neo)                         */
    NODE_RETURN,          /* return      (ferot)                       */
    NODE_BREAK,           /* break       (tham)                        */
    NODE_CONTINUE,        /* continue    (chaliejao)                   */
    NODE_FUNC_DEF,        /* function definition (kaj)                 */
    NODE_PARAM,           /* single function parameter                 */
    NODE_EXPR_STMT,       /* expression used as a statement            */

    /* ---- Expressions ---- */
    NODE_BINARY_EXPR,     /* binary expression   a op b                */
    NODE_UNARY_EXPR,      /* unary expression    !x  -x  x++  x--     */
    NODE_INT_LIT,         /* integer literal                           */
    NODE_FLOAT_LIT,       /* floating-point literal                    */
    NODE_BOOL_LIT,        /* boolean literal  (shotti / mithya)        */
    NODE_STRING_LIT,      /* string literal                            */
    NODE_CHAR_LIT,        /* character literal                         */
    NODE_IDENTIFIER,      /* variable / identifier reference           */
    NODE_FUNC_CALL,       /* function call      name(args)             */

} NodeType;

/* ================================================================== */
/*  Operator Enumeration                                               */
/* ================================================================== */

typedef enum {

    /* Arithmetic */
    OP_ADD,  OP_SUB,  OP_MUL,  OP_DIV,  OP_MOD,

    /* Relational */
    OP_LT,  OP_GT,  OP_LE,  OP_GE,  OP_EQ,  OP_NE,

    /* Logical */
    OP_AND,  OP_OR,  OP_NOT,

    /* Unary */
    OP_NEG,          /* unary minus  (-x)    */
    OP_INC,          /* post-increment (x++) */
    OP_DEC,          /* post-decrement (x--) */

    /* Assignment flavours */
    OP_ASSIGN,       /*  =  */
    OP_ADD_ASSIGN,   /* +=  */
    OP_SUB_ASSIGN,   /* -=  */
    OP_MUL_ASSIGN,   /* *=  */
    OP_DIV_ASSIGN,   /* /=  */

} Operator;

/* ================================================================== */
/*  AST Node Structure                                                 */
/* ================================================================== */
/*                                                                     */
/*  A single, flat struct covers every construct.  Each node type uses */
/*  a documented subset of the fields; unused fields are zeroed.       */
/*                                                                     */
/*  Field usage per node type:                                         */
/*                                                                     */
/*  NODE_PROGRAM      body   → first stmt in the linked list           */
/*  NODE_BLOCK        body   → first stmt in the linked list           */
/*  NODE_VAR_DECL     type_name, name, left = init expr (may be NULL)  */
/*  NODE_CONST_DECL   type_name, name, left = init expr                */
/*  NODE_ASSIGN       op, name, left = rhs expr (NULL for ++/--)      */
/*  NODE_IF           left = cond, body = then, right = else (or NULL) */
/*  NODE_WHILE        left = cond, body = loop body                    */
/*  NODE_FOR          init, left = cond, update, body                  */
/*  NODE_DO_WHILE     body = loop body, left = cond                    */
/*  NODE_SWITCH       left = switch-expr, body = first case clause     */
/*  NODE_CASE         left = case value, body = stmt list              */
/*  NODE_DEFAULT_CASE body = stmt list                                 */
/*  NODE_PRINT        left = expr to print                             */
/*  NODE_INPUT        name = variable name                             */
/*  NODE_RETURN       left = return value (may be NULL for void)       */
/*  NODE_BREAK        (no extra data)                                  */
/*  NODE_CONTINUE     (no extra data)                                  */
/*  NODE_FUNC_DEF     type_name (ret-type), name, params, body         */
/*  NODE_PARAM        type_name, name                                  */
/*  NODE_EXPR_STMT    left = expression                                */
/*  NODE_BINARY_EXPR  op, left, right                                  */
/*  NODE_UNARY_EXPR   op, left = operand                               */
/*  NODE_INT_LIT      int_val                                          */
/*  NODE_FLOAT_LIT    float_val                                        */
/*  NODE_BOOL_LIT     int_val  (1 = shotti, 0 = mithya)               */
/*  NODE_STRING_LIT   string_val                                       */
/*  NODE_CHAR_LIT     string_val                                       */
/*  NODE_IDENTIFIER   name                                             */
/*  NODE_FUNC_CALL    name, params = arg list                          */
/*                                                                     */
/* ================================================================== */

typedef struct ASTNode {

    NodeType type;           /* Discriminator tag                       */
    int      line_no;        /* Source line number (for diagnostics)    */

    /* ---- Operator (binary / unary / assignment) ---- */
    Operator op;

    /* ---- String data ---- */
    char  name[256];         /* Identifier / variable / function name   */
    char  type_name[32];     /* Type keyword  ("purno", "dosomik" …)   */

    /* ---- Literal values ---- */
    int    int_val;          /* Integer or boolean literal value        */
    double float_val;        /* Floating-point literal value            */
    char   string_val[256];  /* String or character literal text        */

    /* ---- Child pointers (meaning varies by node type — see table) ---- */
    struct ASTNode *left;    /* Condition / value / left operand        */
    struct ASTNode *right;   /* Else branch / right operand             */
    struct ASTNode *body;    /* Loop/func body, then-branch, block list */
    struct ASTNode *init;    /* For-loop initialiser                    */
    struct ASTNode *update;  /* For-loop update expression              */
    struct ASTNode *params;  /* Parameter list / argument list head     */

    /* ---- Linked-list pointer ---- */
    struct ASTNode *next;    /* Next sibling in stmt / param / arg list */

} ASTNode;

/* ================================================================== */
/*  Constructor / Factory Functions                                     */
/* ================================================================== */

/* ---- Top-level / structural ---- */
ASTNode *make_program        (ASTNode *stmts);
ASTNode *make_block          (ASTNode *stmts);

/* ---- Declarations ---- */
ASTNode *make_var_decl       (const char *type, const char *name,
                              ASTNode *init_expr, int line);
ASTNode *make_const_decl     (const char *type, const char *name,
                              ASTNode *init_expr, int line);

/* ---- Assignment ---- */
ASTNode *make_assign         (Operator op, const char *name,
                              ASTNode *value, int line);

/* ---- Control flow ---- */
ASTNode *make_if_stmt        (ASTNode *cond, ASTNode *then_body,
                              ASTNode *else_body, int line);
ASTNode *make_while_stmt     (ASTNode *cond, ASTNode *body, int line);
ASTNode *make_for_stmt       (ASTNode *init, ASTNode *cond,
                              ASTNode *update, ASTNode *body, int line);
ASTNode *make_do_while_stmt  (ASTNode *body, ASTNode *cond, int line);
ASTNode *make_switch_stmt    (ASTNode *expr, ASTNode *cases, int line);
ASTNode *make_case_clause    (ASTNode *value, ASTNode *stmts, int line);
ASTNode *make_default_clause (ASTNode *stmts, int line);

/* ---- Break / Continue ---- */
ASTNode *make_break_stmt     (int line);
ASTNode *make_continue_stmt  (int line);

/* ---- Functions ---- */
ASTNode *make_func_def       (const char *ret_type, const char *name,
                              ASTNode *params, ASTNode *body, int line);
ASTNode *make_param          (const char *type, const char *name, int line);
ASTNode *make_return_stmt    (ASTNode *value, int line);

/* ---- I/O ---- */
ASTNode *make_print_stmt     (ASTNode *expr, int line);
ASTNode *make_input_stmt     (const char *name, int line);

/* ---- Expressions ---- */
ASTNode *make_binary_expr    (Operator op, ASTNode *left,
                              ASTNode *right, int line);
ASTNode *make_unary_expr     (Operator op, ASTNode *operand, int line);
ASTNode *make_int_lit        (int value, int line);
ASTNode *make_float_lit      (double value, int line);
ASTNode *make_bool_lit       (int value, int line);
ASTNode *make_string_lit     (const char *value, int line);
ASTNode *make_char_lit       (const char *value, int line);
ASTNode *make_identifier     (const char *name, int line);
ASTNode *make_func_call      (const char *name, ASTNode *args, int line);
ASTNode *make_expr_stmt      (ASTNode *expr, int line);

/* ================================================================== */
/*  List Utility                                                       */
/* ================================================================== */

/* Append `node` to the tail of a singly-linked list; return head. */
ASTNode *append_node         (ASTNode *list, ASTNode *node);

/* ================================================================== */
/*  Pretty-Printer                                                     */
/* ================================================================== */

/* Recursively print the AST to `fp` with indentation.                 */
void     print_ast           (FILE *fp, ASTNode *node, int indent);

/* ================================================================== */
/*  Memory Cleanup                                                     */
/* ================================================================== */

/* Recursively free every node in the tree.                            */
void     free_ast            (ASTNode *node);

#endif /* AST_H */
