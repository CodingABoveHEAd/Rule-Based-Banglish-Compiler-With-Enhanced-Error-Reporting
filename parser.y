%{
/*
 * Banglish Compiler - Parser (Bison)
 * -----------------------------------
 * Parses Banglish source code and prints syntax status.
 * Grammar covers: declarations, assignments, if-else, while,
 * for, do-while, switch-case, print, input, return, functions,
 * break, continue, and expressions with proper precedence.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symtab.h"

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

/* Semantic counters from symtab.c */
extern int semantic_errors;
extern int semantic_warnings;

/* Parser error count */
int syntax_errors = 0;

/* Track current declaration type for semantic actions */
static DataType current_decl_type = TYPE_UNKNOWN;
static int      current_is_const  = 0;

/* Track expression types for type checking */
static DataType last_expr_type = TYPE_UNKNOWN;

/* Loop/switch depth for break/continue validation */
static int loop_depth   = 0;
static int switch_depth = 0;

/* Current function context for return-type checking */
static DataType current_func_return = TYPE_UNKNOWN;
static int      inside_function     = 0;

/* Parameter & argument counting */
static int param_counter = 0;
static int arg_counter   = 0;

void yyerror(const char *msg);
%}

/* ------------------------------------------------------------------ */
/*  Semantic value union                                               */
/* ------------------------------------------------------------------ */
%union {
    int    ival;
    double fval;
    char   sval[256];
    int    dtype;    /* DataType enum for type tracking */
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

/* Type for nonterminals that carry a DataType */
%type <dtype> type_spec
%type <dtype> expr

/* Resolve dangling-else: ELSE_KW binds tighter than "no else" */
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE_KW

%%

/* ================================================================== */
/*  Top-level: a program is a list of statements/declarations          */
/* ================================================================== */

program
    : stmt_list             { fprintf(out, "\n[PARSER] Program parsed successfully.\n"); }
    ;

stmt_list
    : /* empty */
    | stmt_list stmt
    ;

/* ================================================================== */
/*  Statements                                                         */
/* ================================================================== */

stmt
    : decl_stmt
    | assign_stmt
    | if_stmt
    | while_stmt
    | for_stmt
    | do_while_stmt
    | switch_stmt
    | print_stmt
    | input_stmt
    | return_stmt
    | break_stmt
    | continue_stmt
    | func_def
    | block
    | expr_stmt
    | error ';'             { yyerrok; /* panic-mode recovery */ }
    ;

/* ------------------------------------------------------------------ */
/*  Block: { stmt_list }                                               */
/* ------------------------------------------------------------------ */

block
    : '{' { symtab_enter_scope(); } stmt_list '}' { symtab_exit_scope(out); }
    ;

/* ------------------------------------------------------------------ */
/*  Declarations                                                       */
/*  purno x = 10;   dosomik y = 3.14;   torkik flag = shotti;         */
/*  doshomik a;                                               */
/*  dhrubo purno PI = 3;                                               */
/* ------------------------------------------------------------------ */

decl_stmt
    : type_spec IDENTIFIER '=' expr ';'
        {
            fprintf(out, "[PARSER] Declaration: %s (with init) at line %d\n", $2, line_no);
            if ((DataType)$1 == TYPE_SHUNNO) {
                fprintf(out, "[SEMANTIC ERROR] 'shunno' (void) type er variable declare kora jabe na: '%s' (line %d).\n", $2, line_no);
                fprintf(out, "  -> Suggestion: 'purno', 'dosomik', ba 'torkik' type byabohar korun.\n");
                semantic_errors++;
            } else {
                Symbol *s = symtab_insert($2, (DataType)$1, current_is_const, 1, line_no, out);
                if (s && !types_compatible((DataType)$1, (DataType)$4)) {
                    fprintf(out,
                        "[SEMANTIC ERROR] Type mismatch: '%s' er type '%s' kintu assign kora hocche '%s' type er value (line %d).\n",
                        $2, type_to_banglish((DataType)$1), type_to_banglish((DataType)$4), line_no);
                    fprintf(out, "  -> Suggestion: Sothik type er value assign korun.\n");
                    semantic_errors++;
                }
            }
            current_is_const = 0;
        }
    | type_spec IDENTIFIER ';'
        {
            fprintf(out, "[PARSER] Declaration: %s (no init) at line %d\n", $2, line_no);
            if ((DataType)$1 == TYPE_SHUNNO) {
                fprintf(out, "[SEMANTIC ERROR] 'shunno' (void) type er variable declare kora jabe na: '%s' (line %d).\n", $2, line_no);
                semantic_errors++;
            } else {
                symtab_insert($2, (DataType)$1, current_is_const, 0, line_no, out);
            }
            current_is_const = 0;
        }
    | CONST_KW type_spec IDENTIFIER '=' expr ';'
        {
            fprintf(out, "[PARSER] Const declaration: %s at line %d\n", $3, line_no);
            if ((DataType)$2 == TYPE_SHUNNO) {
                fprintf(out, "[SEMANTIC ERROR] 'shunno' (void) type er dhrubo declare kora jabe na: '%s' (line %d).\n", $3, line_no);
                semantic_errors++;
            } else {
                current_is_const = 1;
                Symbol *s = symtab_insert($3, (DataType)$2, 1, 1, line_no, out);
                if (s && !types_compatible((DataType)$2, (DataType)$5)) {
                    fprintf(out,
                        "[SEMANTIC ERROR] Type mismatch: dhrubo '%s' er type '%s' kintu value '%s' type (line %d).\n",
                        $3, type_to_banglish((DataType)$2), type_to_banglish((DataType)$5), line_no);
                    semantic_errors++;
                }
            }
            current_is_const = 0;
        }
    ;

type_spec
    : INT_KW    { $$ = TYPE_PURNO;   current_decl_type = TYPE_PURNO;   }
    | FLOAT_KW  { $$ = TYPE_DOSOMIK; current_decl_type = TYPE_DOSOMIK; }
    | BOOL_KW   { $$ = TYPE_TORKIK;  current_decl_type = TYPE_TORKIK;  }
    | VOID_KW   { $$ = TYPE_SHUNNO;  current_decl_type = TYPE_SHUNNO;  }
    ;

/* ------------------------------------------------------------------ */
/*  Assignment                                                         */
/*  a = 10;   a += 2;   a++;                                          */
/* ------------------------------------------------------------------ */

assign_stmt
    : IDENTIFIER '=' expr ';'
        {
            fprintf(out, "[PARSER] Assignment: %s at line %d\n", $1, line_no);
            Symbol *s = symtab_lookup($1);
            if (!s) {
                fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni. Age declare korun (line %d).\n", $1, line_no);
                semantic_errors++;
            } else {
                if (s->is_const) {
                    fprintf(out, "[SEMANTIC ERROR] '%s' ekta dhrubo (const). Notun value assign kora jabe na (line %d).\n", $1, line_no);
                    semantic_errors++;
                }
                if (!types_compatible(s->type, (DataType)$3)) {
                    fprintf(out, "[SEMANTIC ERROR] Type mismatch: '%s' er type '%s' kintu '%s' type assign hocche (line %d).\n",
                        $1, type_to_banglish(s->type), type_to_banglish((DataType)$3), line_no);
                    semantic_errors++;
                }
                s->is_initialized = 1;
                s->is_used = 1;
            }
        }
    | IDENTIFIER ADD_ASSIGN expr ';'
        {
            fprintf(out, "[PARSER] Assignment: %s += at line %d\n", $1, line_no);
            Symbol *s = symtab_lookup($1);
            if (!s) { fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni (line %d).\n", $1, line_no); semantic_errors++; }
            else if (s->is_const) { fprintf(out, "[SEMANTIC ERROR] '%s' dhrubo (const), modify kora jabe na (line %d).\n", $1, line_no); semantic_errors++; }
            else { s->is_used = 1; }
        }
    | IDENTIFIER SUB_ASSIGN expr ';'
        {
            fprintf(out, "[PARSER] Assignment: %s -= at line %d\n", $1, line_no);
            Symbol *s = symtab_lookup($1);
            if (!s) { fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni (line %d).\n", $1, line_no); semantic_errors++; }
            else if (s->is_const) { fprintf(out, "[SEMANTIC ERROR] '%s' dhrubo (const), modify kora jabe na (line %d).\n", $1, line_no); semantic_errors++; }
            else { s->is_used = 1; }
        }
    | IDENTIFIER MUL_ASSIGN expr ';'
        {
            fprintf(out, "[PARSER] Assignment: %s *= at line %d\n", $1, line_no);
            Symbol *s = symtab_lookup($1);
            if (!s) { fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni (line %d).\n", $1, line_no); semantic_errors++; }
            else if (s->is_const) { fprintf(out, "[SEMANTIC ERROR] '%s' dhrubo (const), modify kora jabe na (line %d).\n", $1, line_no); semantic_errors++; }
            else { s->is_used = 1; }
        }
    | IDENTIFIER DIV_ASSIGN expr ';'
        {
            fprintf(out, "[PARSER] Assignment: %s /= at line %d\n", $1, line_no);
            Symbol *s = symtab_lookup($1);
            if (!s) { fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni (line %d).\n", $1, line_no); semantic_errors++; }
            else if (s->is_const) { fprintf(out, "[SEMANTIC ERROR] '%s' dhrubo (const), modify kora jabe na (line %d).\n", $1, line_no); semantic_errors++; }
            else { s->is_used = 1; }
        }
    | IDENTIFIER INC_OP ';'
        {
            fprintf(out, "[PARSER] Increment: %s at line %d\n", $1, line_no);
            Symbol *s = symtab_lookup($1);
            if (!s) { fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni (line %d).\n", $1, line_no); semantic_errors++; }
            else if (s->is_const) { fprintf(out, "[SEMANTIC ERROR] '%s' dhrubo (const), modify kora jabe na (line %d).\n", $1, line_no); semantic_errors++; }
            else { s->is_used = 1; }
        }
    | IDENTIFIER DEC_OP ';'
        {
            fprintf(out, "[PARSER] Decrement: %s at line %d\n", $1, line_no);
            Symbol *s = symtab_lookup($1);
            if (!s) { fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni (line %d).\n", $1, line_no); semantic_errors++; }
            else if (s->is_const) { fprintf(out, "[SEMANTIC ERROR] '%s' dhrubo (const), modify kora jabe na (line %d).\n", $1, line_no); semantic_errors++; }
            else { s->is_used = 1; }
        }
    ;

/* ------------------------------------------------------------------ */
/*  If / Else                                                          */
/*  jodi (expr) { ... }   nahole { ... }                               */
/* ------------------------------------------------------------------ */

if_stmt
    : IF_KW '(' expr ')' block  %prec LOWER_THAN_ELSE
        {
            fprintf(out, "[PARSER] If statement at line %d\n", line_no);
            if ((DataType)$3 != TYPE_TORKIK && (DataType)$3 != TYPE_UNKNOWN) {
                fprintf(out, "[SEMANTIC WARNING] 'jodi' er condition e '%s' type ache, 'torkik' (boolean) howa uchit (line %d).\n",
                    type_to_banglish((DataType)$3), line_no);
                semantic_warnings++;
            }
        }
    | IF_KW '(' expr ')' block ELSE_KW block
        {
            fprintf(out, "[PARSER] If-Else statement at line %d\n", line_no);
            if ((DataType)$3 != TYPE_TORKIK && (DataType)$3 != TYPE_UNKNOWN) {
                fprintf(out, "[SEMANTIC WARNING] 'jodi' er condition e '%s' type ache, 'torkik' (boolean) howa uchit (line %d).\n",
                    type_to_banglish((DataType)$3), line_no);
                semantic_warnings++;
            }
        }
    | IF_KW '(' expr ')' block ELSE_KW if_stmt
        {
            fprintf(out, "[PARSER] If-Else-If chain at line %d\n", line_no);
        }
    ;

/* ------------------------------------------------------------------ */
/*  While loop                                                         */
/*  jotokhon (expr) { ... }                                            */
/* ------------------------------------------------------------------ */

while_stmt
    : WHILE_KW '(' expr ')' { loop_depth++; } block
        {
            loop_depth--;
            fprintf(out, "[PARSER] While loop at line %d\n", line_no);
            if ((DataType)$3 != TYPE_TORKIK && (DataType)$3 != TYPE_UNKNOWN) {
                fprintf(out, "[SEMANTIC WARNING] 'jotokhon' er condition e '%s' type ache, 'torkik' (boolean) howa uchit (line %d).\n",
                    type_to_banglish((DataType)$3), line_no);
                semantic_warnings++;
            }
        }
    ;

/* ------------------------------------------------------------------ */
/*  For loop                                                           */
/*  ghuro (purno i = 0; i < 5; i++) { ... }                           */
/* ------------------------------------------------------------------ */

for_stmt
    : FOR_KW '(' for_init ';' expr ';' for_update ')' { loop_depth++; } block
        {
            loop_depth--;
            fprintf(out, "[PARSER] For loop at line %d\n", line_no);
        }
    ;

for_init
    : type_spec IDENTIFIER '=' expr
        {
            symtab_insert($2, (DataType)$1, 0, 1, line_no, out);
        }
    | IDENTIFIER '=' expr
        {
            Symbol *s = symtab_lookup($1);
            if (!s) { fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni (line %d).\n", $1, line_no); semantic_errors++; }
            else { s->is_initialized = 1; s->is_used = 1; }
        }
    | /* empty */
    ;

for_update
    : IDENTIFIER INC_OP
        { Symbol *s = symtab_lookup($1); if (s) s->is_used = 1; }
    | IDENTIFIER DEC_OP
        { Symbol *s = symtab_lookup($1); if (s) s->is_used = 1; }
    | IDENTIFIER '=' expr
        { Symbol *s = symtab_lookup($1); if (s) { s->is_used = 1; s->is_initialized = 1; } }
    | IDENTIFIER ADD_ASSIGN expr
        { Symbol *s = symtab_lookup($1); if (s) s->is_used = 1; }
    | IDENTIFIER SUB_ASSIGN expr
        { Symbol *s = symtab_lookup($1); if (s) s->is_used = 1; }
    | /* empty */
    ;

/* ------------------------------------------------------------------ */
/*  Do-While loop                                                      */
/*  koro { ... } jotokhon (expr);                                      */
/* ------------------------------------------------------------------ */

do_while_stmt
    : DO_KW { loop_depth++; } block WHILE_KW '(' expr ')' ';'
        {
            loop_depth--;
            fprintf(out, "[PARSER] Do-While loop at line %d\n", line_no);
        }
    ;

/* ------------------------------------------------------------------ */
/*  Switch-Case                                                        */
/*  bachai (expr) { khetre 1: ... tham; onnothay: ... }                */
/* ------------------------------------------------------------------ */

switch_stmt
    : SWITCH_KW '(' expr ')' '{' { switch_depth++; } case_list '}'
        {
            switch_depth--;
            fprintf(out, "[PARSER] Switch statement at line %d\n", line_no);
        }
    ;

case_list
    : /* empty */
    | case_list case_clause
    ;

case_clause
    : CASE_KW expr ':' stmt_list
        { fprintf(out, "[PARSER] Case clause at line %d\n", line_no); }
    | DEFAULT_KW ':' stmt_list
        { fprintf(out, "[PARSER] Default clause at line %d\n", line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Print                                                              */
/*  dekhao expr;    dekhao "hello";                                    */
/* ------------------------------------------------------------------ */

print_stmt
    : PRINT_KW expr ';'
        { fprintf(out, "[PARSER] Print statement at line %d\n", line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Input                                                              */
/*  neo varname;                                                       */
/* ------------------------------------------------------------------ */

input_stmt
    : INPUT_KW IDENTIFIER ';'
        {
            fprintf(out, "[PARSER] Input statement: %s at line %d\n", $2, line_no);
            Symbol *s = symtab_lookup($2);
            if (!s) {
                fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni. 'neo' er age declare korun (line %d).\n", $2, line_no);
                semantic_errors++;
            } else {
                s->is_initialized = 1;
                s->is_used = 1;
            }
        }
    ;

/* ------------------------------------------------------------------ */
/*  Return                                                             */
/*  ferot;    ferot expr;                                              */
/* ------------------------------------------------------------------ */

return_stmt
    : RETURN_KW ';'
        {
            fprintf(out, "[PARSER] Return (void) at line %d\n", line_no);
            if (inside_function && current_func_return != TYPE_SHUNNO && current_func_return != TYPE_UNKNOWN) {
                fprintf(out, "[SEMANTIC ERROR] Function er return type '%s' kintu kono value ferot kora hoyni (line %d).\n",
                    type_to_banglish(current_func_return), line_no);
                fprintf(out, "  -> Suggestion: 'ferot <value>;' likhun ba function er type 'shunno' korun.\n");
                semantic_errors++;
            }
        }
    | RETURN_KW expr ';'
        {
            fprintf(out, "[PARSER] Return (with value) at line %d\n", line_no);
            if (inside_function) {
                if (current_func_return == TYPE_SHUNNO) {
                    fprintf(out, "[SEMANTIC ERROR] 'shunno' (void) function theke value ferot kora jabe na (line %d).\n", line_no);
                    fprintf(out, "  -> Suggestion: Function er return type change korun ba 'ferot;' likhun.\n");
                    semantic_errors++;
                } else if (!types_compatible(current_func_return, (DataType)$2)) {
                    fprintf(out, "[SEMANTIC WARNING] Return type mismatch: function er type '%s' kintu '%s' ferot hocche (line %d).\n",
                        type_to_banglish(current_func_return), type_to_banglish((DataType)$2), line_no);
                    semantic_warnings++;
                }
            }
        }
    ;

/* ------------------------------------------------------------------ */
/*  Break / Continue                                                   */
/* ------------------------------------------------------------------ */

break_stmt
    : BREAK_KW ';'
        {
            fprintf(out, "[PARSER] Break at line %d\n", line_no);
            if (loop_depth == 0 && switch_depth == 0) {
                fprintf(out, "[SEMANTIC ERROR] 'tham' (break) shudhu loop ba switch er bhitore byabohar kora jay (line %d).\n", line_no);
                fprintf(out, "  -> Suggestion: 'tham' ke loop (jotokhon/ghuro/koro) ba bachai er bhitore rakhun.\n");
                semantic_errors++;
            }
        }
    ;

continue_stmt
    : CONTINUE_KW ';'
        {
            fprintf(out, "[PARSER] Continue at line %d\n", line_no);
            if (loop_depth == 0) {
                fprintf(out, "[SEMANTIC ERROR] 'chaliejao' (continue) shudhu loop er bhitore byabohar kora jay (line %d).\n", line_no);
                fprintf(out, "  -> Suggestion: 'chaliejao' ke loop (jotokhon/ghuro/koro) er bhitore rakhun.\n");
                semantic_errors++;
            }
        }
    ;

/* ------------------------------------------------------------------ */
/*  Function definition                                                */
/*  shunno kaj myFunc() { ... }                                        */
/*  purno  kaj add(purno a, purno b) { ferot a + b; }                  */
/* ------------------------------------------------------------------ */

func_def
    : type_spec FUNC_KW IDENTIFIER '('
        {
            /* Save function context before parsing body */
            current_func_return = (DataType)$1;
            inside_function = 1;
            param_counter = 0;
        }
      param_list ')' block
        {
            fprintf(out, "[PARSER] Function def: %s at line %d\n", $3, line_no);
            symtab_insert_func($3, (DataType)$1, param_counter, line_no, out);
            inside_function = 0;
            current_func_return = TYPE_UNKNOWN;
        }
    ;

param_list
    : /* empty */
    | param
    | param_list ',' param
    ;

param
    : type_spec IDENTIFIER
        {
            symtab_insert($2, (DataType)$1, 0, 1, line_no, out);
            param_counter++;
        }
    ;

/* ------------------------------------------------------------------ */
/*  Expression statement (e.g. function calls as statements)           */
/* ------------------------------------------------------------------ */

expr_stmt
    : expr ';'
    ;

/* ================================================================== */
/*  Expressions  (with operator precedence from %left/%right above)    */
/* ================================================================== */

expr
    : INT_LIT           { $$ = TYPE_PURNO; }
    | FLOAT_LIT         { $$ = TYPE_DOSOMIK; }
    | STRING_LIT        { $$ = TYPE_UNKNOWN; }
    | CHAR_LIT          { $$ = TYPE_UNKNOWN; }
    | TRUE_KW           { $$ = TYPE_TORKIK; }
    | FALSE_KW          { $$ = TYPE_TORKIK; }
    | IDENTIFIER
        {
            Symbol *s = symtab_lookup($1);
            if (!s) {
                fprintf(out, "[SEMANTIC ERROR] '%s' declare kora hoyni (line %d).\n", $1, line_no);
                semantic_errors++;
                $$ = TYPE_UNKNOWN;
            } else {
                s->is_used = 1;
                if (!s->is_initialized && !s->is_function) {
                    fprintf(out, "[SEMANTIC WARNING] '%s' initialize na kore use kora hocche (line %d).\n", $1, line_no);
                    semantic_warnings++;
                }
                $$ = s->type;
            }
        }
    | IDENTIFIER '(' { arg_counter = 0; } arg_list ')'
        {
            Symbol *s = symtab_lookup($1);
            if (!s) {
                fprintf(out, "[SEMANTIC ERROR] '%s' function declare kora hoyni (line %d).\n", $1, line_no);
                semantic_errors++;
                $$ = TYPE_UNKNOWN;
            } else {
                s->is_used = 1;
                if (s->is_function && s->param_count >= 0 && arg_counter != s->param_count) {
                    fprintf(out, "[SEMANTIC ERROR] '%s' function e %d ta parameter dorkar kintu %d ta dewa hoyeche (line %d).\n",
                        $1, s->param_count, arg_counter, line_no);
                    fprintf(out, "  -> Suggestion: Sothik shongkha argument diye function call korun.\n");
                    semantic_errors++;
                }
                $$ = s->type;
            }
        }
    | '(' expr ')'                      { $$ = $2; }
    | expr '+' expr                     { $$ = ($1 == TYPE_DOSOMIK || $3 == TYPE_DOSOMIK) ? TYPE_DOSOMIK : $1; }
    | expr '-' expr                     { $$ = ($1 == TYPE_DOSOMIK || $3 == TYPE_DOSOMIK) ? TYPE_DOSOMIK : $1; }
    | expr '*' expr                     { $$ = ($1 == TYPE_DOSOMIK || $3 == TYPE_DOSOMIK) ? TYPE_DOSOMIK : $1; }
    | expr '/' expr                     { $$ = ($1 == TYPE_DOSOMIK || $3 == TYPE_DOSOMIK) ? TYPE_DOSOMIK : $1; }
    | expr '%' expr
        {
            if ((DataType)$1 == TYPE_DOSOMIK || (DataType)$3 == TYPE_DOSOMIK) {
                fprintf(out, "[SEMANTIC ERROR] Modulus (%%) operator 'dosomik' (float) er shate byabohar kora jabe na (line %d).\n", line_no);
                fprintf(out, "  -> Suggestion: Shudhu 'purno' (int) value er moddhe modulus byabohar korun.\n");
                semantic_errors++;
            }
            $$ = TYPE_PURNO;
        }
    | expr '<' expr                     { $$ = TYPE_TORKIK; }
    | expr '>' expr                     { $$ = TYPE_TORKIK; }
    | expr LE_OP expr                   { $$ = TYPE_TORKIK; }
    | expr GE_OP expr                   { $$ = TYPE_TORKIK; }
    | expr EQ_OP expr                   { $$ = TYPE_TORKIK; }
    | expr NE_OP expr                   { $$ = TYPE_TORKIK; }
    | expr AND_OP expr                  { $$ = TYPE_TORKIK; }
    | expr OR_OP expr                   { $$ = TYPE_TORKIK; }
    | '!' expr                          { $$ = TYPE_TORKIK; }
    | '-' expr   %prec '!'              { $$ = $2; }
    | IDENTIFIER INC_OP
        {
            Symbol *s = symtab_lookup($1);
            if (s) s->is_used = 1;
            $$ = s ? s->type : TYPE_UNKNOWN;
        }
    | IDENTIFIER DEC_OP
        {
            Symbol *s = symtab_lookup($1);
            if (s) s->is_used = 1;
            $$ = s ? s->type : TYPE_UNKNOWN;
        }
    ;

arg_list
    : /* empty */
    | expr                  { arg_counter = 1; }
    | arg_list ',' expr     { arg_counter++; }
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

    /* Initialize symbol table */
    symtab_init();

    /* Token log header */
    fprintf(out, "%-16s | %-22s | %s\n", "TOKEN TYPE", "LEXEME", "LOCATION");
    fprintf(out, "%-16s | %-22s | %s\n",
            "----------------", "----------------------", "--------");

    /* Run the parser (which calls yylex internally) */
    int result = yyparse();

    /* Dump symbol table (global scope remains) */
    symtab_dump(out);

    /* Exit global scope (reports unused globals) */
    symtab_exit_scope(out);

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

    fprintf(out, "\n===== SEMANTIC ANALYSIS SUMMARY =====\n");
    fprintf(out, "Semantic errors   : %d\n", semantic_errors);
    fprintf(out, "Semantic warnings : %d\n", semantic_warnings);
    fprintf(out, "Lexer warnings    : %d\n", warning_count);

    int total_errors = syntax_errors + error_count + semantic_errors;
    if (result == 0 && total_errors == 0) {
        fprintf(out, "\n>> Compilation: SUCCESSFUL (No errors)\n");
        printf("Parsing successful. See output.txt\n");
    } else {
        fprintf(out, "\n>> Compilation: FAILED (%d error(s) total)\n", total_errors);
        printf("Compilation failed with %d error(s). See output.txt\n", total_errors);
    }

    fclose(yyin);
    fclose(out);
    symtab_free();
    return (total_errors > 0) ? 1 : 0;
}
