%{
#include <stdio.h>

extern FILE *yyin;


int line_no = 1;

int keyword_count = 0;
int id_count = 0;
int literal_count = 0;
int operator_count = 0;
int error_count = 0;

int comment_start_line = 0;
%}

%x COMMENT
%option noyywrap


%%

[ \t]+                  ;
\n                      { line_no++; }


"//".*                  ;


"/*"                    { comment_start_line = line_no; BEGIN(COMMENT); }

<COMMENT>{
    "*/"                { BEGIN(INITIAL); }
    \n                  { line_no++; }
    .                   ;
    <<EOF>>             {
                            printf("Lexical Error: Unterminated multi-line comment starting at line %d\n",
                                   comment_start_line);
                            error_count++;
                            BEGIN(INITIAL);
                        }
}


purno                   { printf("INT_KW at line %d\n", line_no); keyword_count++; }
dosomik                 { printf("FLOAT_KW at line %d\n", line_no); keyword_count++; }
jodi                    { printf("IF_KW at line %d\n", line_no); keyword_count++; }
nahole                  { printf("ELSE_KW at line %d\n", line_no); keyword_count++; }
jotokhon                { printf("WHILE_KW at line %d\n", line_no); keyword_count++; }
dekhao                  { printf("PRINT_KW at line %d\n", line_no); keyword_count++; }
ferot                   { printf("RETURN_KW at line %d\n", line_no); keyword_count++; }


"<="|">="|"=="|"!="|"&&"|"||"   {
                            printf("OPERATOR %s at line %d\n", yytext, line_no);
                            operator_count++;
                        }


[0-9]+[a-zA-Z_][a-zA-Z0-9_]* {
                            printf("Lexical Error: Invalid identifier '%s' at line %d\n",
                                   yytext, line_no);
                            error_count++;
                        }


[0-9]+\.[0-9]+\.[0-9]+ {
                            printf("Lexical Error: Invalid numeric constant '%s' at line %d\n",
                                   yytext, line_no);
                            error_count++;
                        }


[0-9]+\.[0-9]+          {
                            printf("FLOAT_LITERAL %s at line %d\n", yytext, line_no);
                            literal_count++;
                        }

[0-9]+                  {
                            printf("INT_LITERAL %s at line %d\n", yytext, line_no);
                            literal_count++;
                        }


[a-zA-Z_][a-zA-Z0-9_]*  {
                            printf("ID %s at line %d\n", yytext, line_no);
                            id_count++;
                        }


[+*/=<>;(){}-]          {
                            printf("OPERATOR %s at line %d\n", yytext, line_no);
                            operator_count++;
                        }


.                       {
                            printf("Lexical Error: Illegal symbol '%s' at line %d\n",
                                   yytext, line_no);
                            error_count++;
                        }
%%

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <source-file>\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    if (!yyin) {
        perror("File open error");
        return 1;
    }

    yylex();
    fclose(yyin);

    printf("\n===== LEXICAL ANALYSIS SUMMARY =====\n");
    printf("Keywords    : %d\n", keyword_count);
    printf("Identifiers : %d\n", id_count);
    printf("Literals    : %d\n", literal_count);
    printf("Operators   : %d\n", operator_count);
    printf("Errors      : %d\n", error_count);

    return 0;
}
