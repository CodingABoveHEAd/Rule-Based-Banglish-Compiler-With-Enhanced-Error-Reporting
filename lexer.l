%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

 FILE *yyin,*out;


int line_no = 1;

int keyword_count = 0;
int id_count = 0;
int literal_count = 0;
int operator_count = 0;
int delimiter_count = 0;
int comment_count = 0;
int warning_count = 0;
int error_count = 0;
int division_seen = 0;


int comment_start_line = 0;
%}

%x COMMENT
%option noyywrap


%%

[ \t]+                  ;
\n                      { line_no++; }


"//".*                  { comment_count++; }


"/*"                    { comment_start_line = line_no; BEGIN(COMMENT); }

<COMMENT>{
    "*/"                { comment_count++; BEGIN(INITIAL); }
    \n                  { line_no++; }
    .                   ;
    <<EOF>>             {
                            fprintf(out,"Lexical Error: Unterminated multi-line comment starting at line %d\n",
                                   comment_start_line);
                            error_count++;
                            BEGIN(INITIAL);
                        }
}


purno                   { fprintf(out,"INT_KW at line %d\n", line_no); keyword_count++; }
dosomik                 { fprintf(out,"FLOAT_KW at line %d\n", line_no); keyword_count++; }
jodi                    { fprintf(out,"IF_KW at line %d\n", line_no); keyword_count++; }
nahole                  { fprintf(out,"ELSE_KW at line %d\n", line_no); keyword_count++; }
jotokhon                { fprintf(out,"WHILE_KW at line %d\n", line_no); keyword_count++; }
dekhao                  { fprintf(out,"PRINT_KW at line %d\n", line_no); keyword_count++; }
ferot                   { fprintf(out,"RETURN_KW at line %d\n", line_no); keyword_count++; }
ghuro                   { fprintf(out, "FOR_KW at line %d\n", line_no); keyword_count++;}

shotti                  { fprintf(out,"TRUE_KW at line %d\n", line_no); keyword_count++; }
mithya                  { fprintf(out,"FALSE_KW at line %d\n", line_no); keyword_count++; }
torkik                  { fprintf(out,"BOOL_KW at line %d\n", line_no); keyword_count++; }
shunno                  { fprintf(out,"VOID_KW at line %d\n", line_no); keyword_count++; }
kaj                     { fprintf(out,"FUNC_KW at line %d\n", line_no); keyword_count++; }
tham                    { fprintf(out,"BREAK_KW at line %d\n", line_no); keyword_count++; }
chaliejao               { fprintf(out,"CONTINUE_KW at line %d\n", line_no); keyword_count++; }
bachai                  { fprintf(out,"SWITCH_KW at line %d\n", line_no); keyword_count++; }
khetre                  { fprintf(out,"CASE_KW at line %d\n", line_no); keyword_count++; }
onnothay                { fprintf(out,"DEFAULT_KW at line %d\n", line_no); keyword_count++; }
koro                    { fprintf(out,"DO_KW at line %d\n", line_no); keyword_count++; }
dhrubo                  { fprintf(out,"CONST_KW at line %d\n", line_no); keyword_count++; }
neo                     { fprintf(out,"INPUT_KW at line %d\n", line_no); keyword_count++; }
   


"<="|">="|"=="|"!="|"&&"|"||"   {
                            fprintf(out,"OPERATOR %s at line %d\n", yytext, line_no);
                            operator_count++;
                            division_seen = 0;
                        }


"++"|"--"|"+="|"-="|"*="|"/="      {
                            fprintf(out,"OPERATOR %s at line %d\n", yytext, line_no);
                            operator_count++;
                            division_seen = 0;
                        }


[0-9]+[a-zA-Z_][a-zA-Z0-9_]* {
                            fprintf(out,"Lexical Error: Invalid identifier '%s' at line %d\n",
                                   yytext, line_no);
                            error_count++;
                        }


[0-9]+\.[0-9]+\.[0-9]+ {
                            fprintf(out,"Lexical Error: Invalid numeric constant '%s' at line %d\n",
                                   yytext, line_no);
                            error_count++;
                        }


[0-9]+\.[0-9]+          {
                            fprintf(out,"FLOAT_LITERAL %s at line %d\n", yytext, line_no);
                            literal_count++;

                            if (division_seen && strtod(yytext, NULL) == 0.0) {
                                fprintf(out,
                                    "Semantic Error: Division by zero detected at line %d\n",
                                    line_no);
                                error_count++;
                            }
                            division_seen = 0;
                        }

[0-9]+                  {
                            fprintf(out,"INT_LITERAL %s at line %d\n", yytext, line_no);
                            literal_count++;

                            if (division_seen && strcmp(yytext, "0") == 0) {
                                fprintf(out,
                                    "Semantic Error: Division by zero detected at line %d\n",
                                    line_no);
                                error_count++;
                            }
                            division_seen = 0;

                        }


\"([^\\\n]|\\.)*\"      {
                            fprintf(out,"STRING_LITERAL %s at line %d\n", yytext, line_no);
                            literal_count++;
                            division_seen = 0;
                        }


'([^\\\n]|\\.)'       {
                            fprintf(out,"CHAR_LITERAL %s at line %d\n", yytext, line_no);
                            literal_count++;
                            division_seen = 0;
                        }


\"[^\"\n]*                 {
                            fprintf(out,"Lexical Error: Unterminated string literal at line %d\n", line_no);
                            error_count++;
                            division_seen = 0;
                        }


[a-zA-Z_][a-zA-Z0-9_]*  {
                            fprintf(out,"ID %s at line %d\n", yytext, line_no);
                            id_count++;

                            if (strlen(yytext) > 31) {
                                fprintf(out,
                                    "Warning: Identifier '%s' is too long at line %d\n",
                                    yytext, line_no);
                                warning_count++;
                            }
                            division_seen = 0;
                        }


[;:,(){}\[\]]           {
                            fprintf(out,"DELIMITER %s at line %d\n", yytext, line_no);
                            delimiter_count++;
                            division_seen = 0;
                        }


[+\-*/%=<>!]            {
                            fprintf(out,"OPERATOR %s at line %d\n", yytext, line_no);
                            operator_count++;
                            if (strcmp(yytext, "/") == 0) {
                              division_seen = 1;
                            } else {
                              division_seen = 0;
                            }

                        }


.                       {
                            fprintf(out,"Lexical Error: Illegal symbol '%s' at line %d\n",
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

     out = fopen("output.txt", "w");
    if (!out) {
        perror("Output file open error");
        return 1;
    }

    yylex();
    

    fprintf(out,"\n===== LEXICAL ANALYSIS SUMMARY =====\n");
    fprintf(out,"Keywords    : %d\n", keyword_count);
    fprintf(out,"Identifiers : %d\n", id_count);
    fprintf(out,"Literals    : %d\n", literal_count);
    fprintf(out,"Operators   : %d\n", operator_count);
    fprintf(out,"Delimiters  : %d\n", delimiter_count);
    fprintf(out,"Comments    : %d\n", comment_count);
    fprintf(out,"Warnings    : %d\n", warning_count);
    fprintf(out,"Errors      : %d\n", error_count);
    fclose(yyin);
    fclose(out);
    return 0;
}