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

void yyerror(const char *msg);
%}

/* ------------------------------------------------------------------ */
/*  Semantic value union                                               */
/* ------------------------------------------------------------------ */
%union {
    int    ival;
    double fval;
    char   sval[256];
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
    : '{' stmt_list '}'
    ;

/* ------------------------------------------------------------------ */
/*  Declarations                                                       */
/*  purno x = 10;   dosomik y = 3.14;   torkik flag = shotti;         */
/*  doshomik a;                                               */
/*  dhrubo purno PI = 3;                                               */
/* ------------------------------------------------------------------ */

decl_stmt
    : type_spec IDENTIFIER '=' expr ';'
        { fprintf(out, "[PARSER] Declaration: %s (with init) at line %d\n", $2, line_no); }
    | type_spec IDENTIFIER ';'
        { fprintf(out, "[PARSER] Declaration: %s (no init) at line %d\n", $2, line_no); }
    | CONST_KW type_spec IDENTIFIER '=' expr ';'
        { fprintf(out, "[PARSER] Const declaration: %s at line %d\n", $3, line_no); }
    ;

type_spec
    : INT_KW
    | FLOAT_KW
    | BOOL_KW
    | VOID_KW
    ;

/* ------------------------------------------------------------------ */
/*  Assignment                                                         */
/*  a = 10;   a += 2;   a++;                                          */
/* ------------------------------------------------------------------ */

assign_stmt
    : IDENTIFIER '=' expr ';'
        { fprintf(out, "[PARSER] Assignment: %s at line %d\n", $1, line_no); }
    | IDENTIFIER ADD_ASSIGN expr ';'
        { fprintf(out, "[PARSER] Assignment: %s += at line %d\n", $1, line_no); }
    | IDENTIFIER SUB_ASSIGN expr ';'
        { fprintf(out, "[PARSER] Assignment: %s -= at line %d\n", $1, line_no); }
    | IDENTIFIER MUL_ASSIGN expr ';'
        { fprintf(out, "[PARSER] Assignment: %s *= at line %d\n", $1, line_no); }
    | IDENTIFIER DIV_ASSIGN expr ';'
        { fprintf(out, "[PARSER] Assignment: %s /= at line %d\n", $1, line_no); }
    | IDENTIFIER INC_OP ';'
        { fprintf(out, "[PARSER] Increment: %s at line %d\n", $1, line_no); }
    | IDENTIFIER DEC_OP ';'
        { fprintf(out, "[PARSER] Decrement: %s at line %d\n", $1, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  If / Else                                                          */
/*  jodi (expr) { ... }   nahole { ... }                               */
/* ------------------------------------------------------------------ */

if_stmt
    : IF_KW '(' expr ')' block  %prec LOWER_THAN_ELSE
        { fprintf(out, "[PARSER] If statement at line %d\n", line_no); }
    | IF_KW '(' expr ')' block ELSE_KW block
        { fprintf(out, "[PARSER] If-Else statement at line %d\n", line_no); }
    | IF_KW '(' expr ')' block ELSE_KW if_stmt
        { fprintf(out, "[PARSER] If-Else-If chain at line %d\n", line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  While loop                                                         */
/*  jotokhon (expr) { ... }                                            */
/* ------------------------------------------------------------------ */

while_stmt
    : WHILE_KW '(' expr ')' block
        { fprintf(out, "[PARSER] While loop at line %d\n", line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  For loop                                                           */
/*  ghuro (purno i = 0; i < 5; i++) { ... }                           */
/* ------------------------------------------------------------------ */

for_stmt
    : FOR_KW '(' for_init ';' expr ';' for_update ')' block
        { fprintf(out, "[PARSER] For loop at line %d\n", line_no); }
    ;

for_init
    : type_spec IDENTIFIER '=' expr
    | IDENTIFIER '=' expr
    | /* empty */
    ;

for_update
    : IDENTIFIER INC_OP
    | IDENTIFIER DEC_OP
    | IDENTIFIER '=' expr
    | IDENTIFIER ADD_ASSIGN expr
    | IDENTIFIER SUB_ASSIGN expr
    | /* empty */
    ;

/* ------------------------------------------------------------------ */
/*  Do-While loop                                                      */
/*  koro { ... } jotokhon (expr);                                      */
/* ------------------------------------------------------------------ */

do_while_stmt
    : DO_KW block WHILE_KW '(' expr ')' ';'
        { fprintf(out, "[PARSER] Do-While loop at line %d\n", line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Switch-Case                                                        */
/*  bachai (expr) { khetre 1: ... tham; onnothay: ... }                */
/* ------------------------------------------------------------------ */

switch_stmt
    : SWITCH_KW '(' expr ')' '{' case_list '}'
        { fprintf(out, "[PARSER] Switch statement at line %d\n", line_no); }
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
        { fprintf(out, "[PARSER] Input statement: %s at line %d\n", $2, line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Return                                                             */
/*  ferot;    ferot expr;                                              */
/* ------------------------------------------------------------------ */

return_stmt
    : RETURN_KW ';'
        { fprintf(out, "[PARSER] Return (void) at line %d\n", line_no); }
    | RETURN_KW expr ';'
        { fprintf(out, "[PARSER] Return (with value) at line %d\n", line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Break / Continue                                                   */
/* ------------------------------------------------------------------ */

break_stmt
    : BREAK_KW ';'
        { fprintf(out, "[PARSER] Break at line %d\n", line_no); }
    ;

continue_stmt
    : CONTINUE_KW ';'
        { fprintf(out, "[PARSER] Continue at line %d\n", line_no); }
    ;

/* ------------------------------------------------------------------ */
/*  Function definition                                                */
/*  shunno kaj myFunc() { ... }                                        */
/*  purno  kaj add(purno a, purno b) { ferot a + b; }                  */
/* ------------------------------------------------------------------ */

func_def
    : type_spec FUNC_KW IDENTIFIER '(' param_list ')' block
        { fprintf(out, "[PARSER] Function def: %s at line %d\n", $3, line_no); }
    ;

param_list
    : /* empty */
    | param
    | param_list ',' param
    ;

param
    : type_spec IDENTIFIER
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
    : INT_LIT
    | FLOAT_LIT
    | STRING_LIT
    | CHAR_LIT
    | TRUE_KW
    | FALSE_KW
    | IDENTIFIER
    | IDENTIFIER '(' arg_list ')'       /* function call */
    | '(' expr ')'                      /* grouped */
    | expr '+' expr
    | expr '-' expr
    | expr '*' expr
    | expr '/' expr
    | expr '%' expr
    | expr '<' expr
    | expr '>' expr
    | expr LE_OP expr
    | expr GE_OP expr
    | expr EQ_OP expr
    | expr NE_OP expr
    | expr AND_OP expr
    | expr OR_OP expr
    | '!' expr
    | '-' expr   %prec '!'             /* unary minus */
    | IDENTIFIER INC_OP                /* a++ inside expr */
    | IDENTIFIER DEC_OP                /* a-- inside expr */
    ;

arg_list
    : /* empty */
    | expr
    | arg_list ',' expr
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

    fclose(yyin);
    fclose(out);
    return result;
}
