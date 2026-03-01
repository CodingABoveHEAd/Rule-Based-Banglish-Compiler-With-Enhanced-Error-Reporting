%{
/*
 * Banglish Compiler - Parser (Bison) with AST Generation
 * -------------------------------------------------------
 * Parses Banglish source code and builds an Abstract Syntax Tree.
 * Grammar covers: declarations, assignments, if-else, while,
 * for, do-while, switch-case, print, input, return, functions,
 * break, continue, and expressions with proper precedence.
 *
 * Every grammar action creates AST nodes (defined in ast.h/ast.c)
 * instead of performing direct evaluation or printing.
 * The resulting tree is pretty-printed after a successful parse.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"

/* Provided by Flex */
extern int  yylex(void);
extern int  line_no;
extern FILE *yyin;
extern FILE *out;
extern char *yytext;

/* Counts from lexer (for summary) */
extern int keyword_count;
extern int id_count;
extern int literal_count;
extern int operator_count;
extern int delimiter_count;
extern int comment_count;
extern int warning_count;
extern int error_count;

/* Parser error count */
int syntax_errors = 0;

/* AST root — populated by the top-level `program` rule */
ASTNode *ast_root = NULL;

void yyerror(const char *msg);
%}

/* ------------------------------------------------------------------ */
/*  Semantic value union                                               */
/* ------------------------------------------------------------------ */
%union {
    int    ival;
    double fval;
    char   sval[256];
    struct ASTNode *node;   /* AST node pointer for non-terminals      */
}

/* ------------------------------------------------------------------ */
/*  Token declarations  (must match what Flex returns)                  */
/* ------------------------------------------------------------------ */

/* Data-type keywords */
%token <sval> INT_KW FLOAT_KW BOOL_KW VOID_KW CONST_KW

/* Boolean literal keywords */
%token <ival> TRUE_KW FALSE_KW

/* Control-flow keywords */
%token IF_KW ELSE_KW WHILE_KW FOR_KW DO_KW
%token SWITCH_KW CASE_KW DEFAULT_KW
%token BREAK_KW CONTINUE_KW

/* Function / IO keywords */
%token RETURN_KW PRINT_KW INPUT_KW FUNC_KW

/* Literals */
%token <ival>  INT_LIT
%token <fval>  FLOAT_LIT
%token <sval>  STRING_LIT CHAR_LIT

/* Identifier */
%token <sval>  IDENTIFIER

/* Multi-char operators */
%token LE_OP GE_OP EQ_OP NE_OP AND_OP OR_OP
%token INC_OP DEC_OP
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN

/* ------------------------------------------------------------------ */
/*  Operator precedence (lowest to highest)                            */
/* ------------------------------------------------------------------ */
%right '='  ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN
%left  OR_OP
%left  AND_OP
%left  EQ_OP NE_OP
%left  '<' '>' LE_OP GE_OP
%left  '+' '-'
%left  '*' '/' '%'
%right '!'
%nonassoc INC_OP DEC_OP

/* ------------------------------------------------------------------ */
/*  Non-terminal types  (AST nodes returned by grammar rules)          */
/* ------------------------------------------------------------------ */

%type <node> program stmt_list stmt block
%type <node> decl_stmt assign_stmt
%type <node> if_stmt while_stmt for_stmt do_while_stmt
%type <node> for_init for_update
%type <node> switch_stmt case_list case_clause
%type <node> print_stmt input_stmt return_stmt
%type <node> break_stmt continue_stmt
%type <node> func_def param_list param
%type <node> expr_stmt expr arg_list
%type <sval> type_spec

/* Resolve dangling-else: ELSE_KW binds tighter than "no else" */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE_KW

%%

/* ================================================================== */
/*  Top-level: a program is a list of statements/declarations          */
/* ================================================================== */

program
    : stmt_list             { ast_root = make_program($1); }
    ;

stmt_list
    : /* empty */           { $$ = NULL; }
    | stmt_list stmt        { $$ = append_node($1, $2); }
    ;

/* ================================================================== */
/*  Statements                                                         */
/* ================================================================== */

stmt
    : decl_stmt             { $$ = $1; }
    | assign_stmt           { $$ = $1; }
    | if_stmt               { $$ = $1; }
    | while_stmt            { $$ = $1; }
    | for_stmt              { $$ = $1; }
    | do_while_stmt         { $$ = $1; }
    | switch_stmt           { $$ = $1; }
    | print_stmt            { $$ = $1; }
    | input_stmt            { $$ = $1; }
    | return_stmt           { $$ = $1; }
    | break_stmt            { $$ = $1; }
    | continue_stmt         { $$ = $1; }
    | func_def              { $$ = $1; }
    | block                 { $$ = $1; }
    | expr_stmt             { $$ = $1; }
    | error ';'             { yyerrok; $$ = NULL; }
    ;

/* ------------------------------------------------------------------ */
/*  Block: { stmt_list }                                               */
/* ------------------------------------------------------------------ */

block
    : '{' stmt_list '}'     { $$ = make_block($2); }
    ;

/* ------------------------------------------------------------------ */
/*  Declarations                                                       */
/*  purno x = 10;   dosomik y = 3.14;   torkik flag = shotti;         */
/*  doshomik a;                                               */
/*  dhrubo purno PI = 3;                                               */
/* ------------------------------------------------------------------ */

decl_stmt
    : type_spec IDENTIFIER '=' expr ';'
        { $$ = make_var_decl($1, $2, $4, line_no); }
    | type_spec IDENTIFIER ';'
        { $$ = make_var_decl($1, $2, NULL, line_no); }
    | CONST_KW type_spec IDENTIFIER '=' expr ';'
        { $$ = make_const_decl($2, $3, $5, line_no); }
    ;

type_spec
    : INT_KW                { strcpy($$, "purno"); }
    | FLOAT_KW              { strcpy($$, "dosomik"); }
    | BOOL_KW               { strcpy($$, "torkik"); }
    | VOID_KW               { strcpy($$, "shunno"); }
    ;

/* ------------------------------------------------------------------ */
/*  Assignment                                                         */
/*  a = 10;   a += 2;   a++;                                          */
/* ------------------------------------------------------------------ */

assign_stmt
    : IDENTIFIER '=' expr ';'
        { $$ = make_assign(OP_ASSIGN, $1, $3, line_no); }
    | IDENTIFIER ADD_ASSIGN expr ';'
        { $$ = make_assign(OP_ADD_ASSIGN, $1, $3, line_no); }
    | IDENTIFIER SUB_ASSIGN expr ';'
        { $$ = make_assign(OP_SUB_ASSIGN, $1, $3, line_no); }
    | IDENTIFIER MUL_ASSIGN expr ';'
        { $$ = make_assign(OP_MUL_ASSIGN, $1, $3, line_no); }
    | IDENTIFIER DIV_ASSIGN expr ';'
        { $$ = make_assign(OP_DIV_ASSIGN, $1, $3, line_no); }
    | IDENTIFIER INC_OP ';'
        { $$ = make_assign(OP_INC, $1, NULL, line_no); }
    | IDENTIFIER DEC_OP ';'
        { $$ = make_assign(OP_DEC, $1, NULL, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  If / Else                                                          */
/*  jodi (expr) { ... }   nahole { ... }                               */
/* ------------------------------------------------------------------ */

if_stmt
    : IF_KW '(' expr ')' block  %prec LOWER_THAN_ELSE
        { $$ = make_if_stmt($3, $5, NULL, line_no); }
    | IF_KW '(' expr ')' block ELSE_KW block
        { $$ = make_if_stmt($3, $5, $7, line_no); }
    | IF_KW '(' expr ')' block ELSE_KW if_stmt
        { $$ = make_if_stmt($3, $5, $7, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  While loop                                                         */
/*  jotokhon (expr) { ... }                                            */
/* ------------------------------------------------------------------ */

while_stmt
    : WHILE_KW '(' expr ')' block
        { $$ = make_while_stmt($3, $5, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  For loop                                                           */
/*  ghuro (purno i = 0; i < 5; i++) { ... }                           */
/* ------------------------------------------------------------------ */

for_stmt
    : FOR_KW '(' for_init ';' expr ';' for_update ')' block
        { $$ = make_for_stmt($3, $5, $7, $9, line_no); }
    ;

for_init
    : type_spec IDENTIFIER '=' expr
        { $$ = make_var_decl($1, $2, $4, line_no); }
    | IDENTIFIER '=' expr
        { $$ = make_assign(OP_ASSIGN, $1, $3, line_no); }
    | /* empty */
        { $$ = NULL; }
    ;

for_update
    : IDENTIFIER INC_OP
        { $$ = make_unary_expr(OP_INC, make_identifier($1, line_no), line_no); }
    | IDENTIFIER DEC_OP
        { $$ = make_unary_expr(OP_DEC, make_identifier($1, line_no), line_no); }
    | IDENTIFIER '=' expr
        { $$ = make_assign(OP_ASSIGN, $1, $3, line_no); }
    | IDENTIFIER ADD_ASSIGN expr
        { $$ = make_assign(OP_ADD_ASSIGN, $1, $3, line_no); }
    | IDENTIFIER SUB_ASSIGN expr
        { $$ = make_assign(OP_SUB_ASSIGN, $1, $3, line_no); }
    | /* empty */
        { $$ = NULL; }
    ;

/* ------------------------------------------------------------------ */
/*  Do-While loop                                                      */
/*  koro { ... } jotokhon (expr);                                      */
/* ------------------------------------------------------------------ */

do_while_stmt
    : DO_KW block WHILE_KW '(' expr ')' ';'
        { $$ = make_do_while_stmt($2, $5, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Switch-Case                                                        */
/*  bachai (expr) { khetre 1: ... tham; onnothay: ... }                */
/* ------------------------------------------------------------------ */

switch_stmt
    : SWITCH_KW '(' expr ')' '{' case_list '}'
        { $$ = make_switch_stmt($3, $6, line_no); }
    ;

case_list
    : /* empty */           { $$ = NULL; }
    | case_list case_clause { $$ = append_node($1, $2); }
    ;

case_clause
    : CASE_KW expr ':' stmt_list
        { $$ = make_case_clause($2, $4, line_no); }
    | DEFAULT_KW ':' stmt_list
        { $$ = make_default_clause($3, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Print                                                              */
/*  dekhao expr;    dekhao "hello";                                    */
/* ------------------------------------------------------------------ */

print_stmt
    : PRINT_KW expr ';'
        { $$ = make_print_stmt($2, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Input                                                              */
/*  neo varname;                                                       */
/* ------------------------------------------------------------------ */

input_stmt
    : INPUT_KW IDENTIFIER ';'
        { $$ = make_input_stmt($2, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Return                                                             */
/*  ferot;    ferot expr;                                              */
/* ------------------------------------------------------------------ */

return_stmt
    : RETURN_KW ';'
        { $$ = make_return_stmt(NULL, line_no); }
    | RETURN_KW expr ';'
        { $$ = make_return_stmt($2, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Break / Continue                                                   */
/* ------------------------------------------------------------------ */

break_stmt
    : BREAK_KW ';'
        { $$ = make_break_stmt(line_no); }
    ;

continue_stmt
    : CONTINUE_KW ';'
        { $$ = make_continue_stmt(line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Function definition                                                */
/*  shunno kaj myFunc() { ... }                                        */
/*  purno  kaj add(purno a, purno b) { ferot a + b; }                  */
/* ------------------------------------------------------------------ */

func_def
    : type_spec FUNC_KW IDENTIFIER '(' param_list ')' block
        { $$ = make_func_def($1, $3, $5, $7, line_no); }
    ;

param_list
    : /* empty */           { $$ = NULL; }
    | param                 { $$ = $1; }
    | param_list ',' param  { $$ = append_node($1, $3); }
    ;

param
    : type_spec IDENTIFIER
        { $$ = make_param($1, $2, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Expression statement (e.g. function calls as statements)           */
/* ------------------------------------------------------------------ */

expr_stmt
    : expr ';'
        { $$ = make_expr_stmt($1, line_no); }
    ;

/* ================================================================== */
/*  Expressions  (with operator precedence from %left/%right above)    */
/* ================================================================== */

expr
    : INT_LIT                { $$ = make_int_lit($1, line_no); }
    | FLOAT_LIT              { $$ = make_float_lit($1, line_no); }
    | STRING_LIT             { $$ = make_string_lit($1, line_no); }
    | CHAR_LIT               { $$ = make_char_lit($1, line_no); }
    | TRUE_KW                { $$ = make_bool_lit(1, line_no); }
    | FALSE_KW               { $$ = make_bool_lit(0, line_no); }
    | IDENTIFIER             { $$ = make_identifier($1, line_no); }
    | IDENTIFIER '(' arg_list ')'                          /* function call */
        { $$ = make_func_call($1, $3, line_no); }
    | '(' expr ')'           { $$ = $2; }                  /* grouped */
    | expr '+' expr          { $$ = make_binary_expr(OP_ADD, $1, $3, line_no); }
    | expr '-' expr          { $$ = make_binary_expr(OP_SUB, $1, $3, line_no); }
    | expr '*' expr          { $$ = make_binary_expr(OP_MUL, $1, $3, line_no); }
    | expr '/' expr          { $$ = make_binary_expr(OP_DIV, $1, $3, line_no); }
    | expr '%' expr          { $$ = make_binary_expr(OP_MOD, $1, $3, line_no); }
    | expr '<' expr          { $$ = make_binary_expr(OP_LT,  $1, $3, line_no); }
    | expr '>' expr          { $$ = make_binary_expr(OP_GT,  $1, $3, line_no); }
    | expr LE_OP expr        { $$ = make_binary_expr(OP_LE,  $1, $3, line_no); }
    | expr GE_OP expr        { $$ = make_binary_expr(OP_GE,  $1, $3, line_no); }
    | expr EQ_OP expr        { $$ = make_binary_expr(OP_EQ,  $1, $3, line_no); }
    | expr NE_OP expr        { $$ = make_binary_expr(OP_NE,  $1, $3, line_no); }
    | expr AND_OP expr       { $$ = make_binary_expr(OP_AND, $1, $3, line_no); }
    | expr OR_OP expr        { $$ = make_binary_expr(OP_OR,  $1, $3, line_no); }
    | '!' expr               { $$ = make_unary_expr(OP_NOT, $2, line_no); }
    | '-' expr   %prec '!'   { $$ = make_unary_expr(OP_NEG, $2, line_no); }
    | IDENTIFIER INC_OP                                     /* a++ inside expr */
        { $$ = make_unary_expr(OP_INC, make_identifier($1, line_no), line_no); }
    | IDENTIFIER DEC_OP                                     /* a-- inside expr */
        { $$ = make_unary_expr(OP_DEC, make_identifier($1, line_no), line_no); }
    ;

arg_list
    : /* empty */            { $$ = NULL; }
    | expr                   { $$ = $1; }
    | arg_list ',' expr      { $$ = append_node($1, $3); }
    ;

%%

/* ================================================================== */
/*  Error handler - reports in Banglish style                          */
/* ================================================================== */

void yyerror(const char *msg) {
    syntax_errors++;
    fprintf(out, "\n[SYNTAX ERROR] Line %d: %s\n", line_no, msg);
    fprintf(out, "  -> Banglish: '%s' er kache syntax bhul ache.\n", yytext);
    fprintf(out, "  -> Suggestion: Ei line-e statement thik kore likhun.\n\n");
    fprintf(stderr, "Syntax error at line %d near '%s': %s\n", line_no, yytext, msg);
}

/* ================================================================== */
/*  Main entry point                                                   */
/* ================================================================== */

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <source-file>\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    if (!yyin) {
        perror("Cannot open source file");
        return 1;
    }

    out = fopen("output.txt", "w");
    if (!out) {
        perror("Cannot open output.txt");
        return 1;
    }

    /* Token log header */
    fprintf(out, "%-16s | %-22s | %s\n", "TOKEN TYPE", "LEXEME", "LOCATION");
    fprintf(out, "%-16s | %-22s | %s\n",
            "----------------", "----------------------", "--------");

    /* Run the parser (which calls yylex internally) */
    int result = yyparse();

    /* Summaries */
    fprintf(out, "\n===== LEXICAL ANALYSIS SUMMARY =====\n");
    fprintf(out, "Keywords    : %d\n", keyword_count);
    fprintf(out, "Identifiers : %d\n", id_count);
    fprintf(out, "Literals    : %d\n", literal_count);
    fprintf(out, "Operators   : %d\n", operator_count);
    fprintf(out, "Delimiters  : %d\n", delimiter_count);
    fprintf(out, "Comments    : %d\n", comment_count);

    fprintf(out, "\n===== SYNTAX ANALYSIS SUMMARY =====\n");
    fprintf(out, "Syntax errors  : %d\n", syntax_errors);
    fprintf(out, "Lexical errors : %d\n", error_count);
    fprintf(out, "Warnings       : %d\n", warning_count);

    if (result == 0 && syntax_errors == 0) {
        fprintf(out, "\n>> Compilation: SUCCESSFUL (No errors)\n");
        printf("Parsing successful. See output.txt\n");
    } else {
        fprintf(out, "\n>> Compilation: FAILED (%d syntax error(s))\n", syntax_errors);
        printf("Parsing failed with %d error(s). See output.txt\n", syntax_errors);
    }

    /* ---- Pretty-print the AST ---- */
    if (ast_root) {
        fprintf(out, "\n===== ABSTRACT SYNTAX TREE =====\n\n");
        print_ast(out, ast_root, 0);
        free_ast(ast_root);
        ast_root = NULL;
    }

    fclose(yyin);
    fclose(out);
    return result;
}
