%{
#include <stdio.h>

int line_no = 1;
%}

%option noyywrap

%%
[ \t]+              ;
\n                  { line_no++; }

purno               { printf("INT_KW at line %d\n", line_no); }
dosomik             { printf("FLOAT_KW at line %d\n", line_no); }
jodi                { printf("IF_KW at line %d\n", line_no); }
nahole              { printf("ELSE_KW at line %d\n", line_no); }
jotokhon            { printf("WHILE_KW at line %d\n", line_no); }
dekhao              { printf("PRINT_KW at line %d\n", line_no); }

[0-9]+\.[0-9]+      { printf("FLOAT_LITERAL %s at line %d\n", yytext, line_no); }
[0-9]+              { printf("INT_LITERAL %s at line %d\n", yytext, line_no); }

[a-zA-Z_][a-zA-Z0-9_]* { printf("ID %s at line %d\n", yytext, line_no); }

[+*/=<>;(){}-]      { printf("OPERATOR %s at line %d\n", yytext, line_no); }

.                   { printf("UNKNOWN %s at line %d\n", yytext, line_no); }
%%

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    if (!yyin) {
        perror("File error");
        return 1;
    }

    yylex();
    return 0;
}
