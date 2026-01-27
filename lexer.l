%{
#include <stdio.h>
#include <stdlib.h>

int line_no = 1;

// Token types
enum TokenType {
    INT_KW,
    FLOAT_KW,
    IF_KW,
    ELSE_KW,
    WHILE_KW,
    PRINT_KW,
    RETURN_KW,
    ID,
    INT_LITERAL,
    FLOAT_LITERAL,
    OPERATOR
};

%}

%option noyywrap

%%

[ \t]+           { /* skip whitespace */ }

\n               { line_no++; }


\"purno\"         { printf("Token: INT_KW, Line: %d\n", line_no); return INT_KW; }
\"dosomik\"       { printf("Token: FLOAT_KW, Line: %d\n", line_no); return FLOAT_KW; }
\"jodi\"          { printf("Token: IF_KW, Line: %d\n", line_no); return IF_KW; }
\"nahole\"        { printf("Token: ELSE_KW, Line: %d\n", line_no); return ELSE_KW; }
\"jotokhon\"      { printf("Token: WHILE_KW, Line: %d\n", line_no); return WHILE_KW; }
\"dekhao\"        { printf("Token: PRINT_KW, Line: %d\n", line_no); return PRINT_KW; }
\"ferot\"         { printf("Token: RETURN_KW, Line: %d\n", line_no); return RETURN_KW; }


[a-zA-Z_][a-zA-Z0-9_]* { printf("Token: ID (%s), Line: %d\n", yytext, line_no); return ID; }

[0-9]+           { printf("Token: INT_LITERAL (%s), Line: %d\n", yytext, line_no); return INT_LITERAL; }

[0-9]+"."[0-9]+  { printf("Token: FLOAT_LITERAL (%s), Line: %d\n", yytext, line_no); return FLOAT_LITERAL; }

[\+\-\*/=<>(){};] { printf("Token: OPERATOR (%s), Line: %d\n", yytext, line_no); return OPERATOR; }

.                { printf("Unknown character: %s at line %d\n", yytext, line_no); }

%%
int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <source-file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (!file) {
        perror("Cannot open file");
        return 1;
    }

    yyin = file;  // Set Flex input
    yylex();      // Start lexical analysis
    fclose(file);

    return 0;
}
