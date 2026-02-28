%{
/*
 * Banglish Compiler - Lexer (Flex)
 * Integrated with Bison: token codes and YYSTYPE come from parser.tab.h
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "parser.tab.h"

/* Shared globals */
FILE *out;

int line_no        = 1;
int keyword_count  = 0;
int id_count       = 0;
int literal_count  = 0;
int operator_count = 0;
int delimiter_count= 0;
int comment_count  = 0;
int warning_count  = 0;
int error_count    = 0;
int division_seen  = 0;
int comment_start_line = 0;

/* Log token to output.txt and return its code */
static int emit(int code, const char *label) {
    fprintf(out, "%-16s | %-22s | line %d\n", label, yytext, line_no);
    return code;
}
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


purno       { keyword_count++; return emit(INT_KW,      "INT_KW");     }
dosomik     { keyword_count++; return emit(FLOAT_KW,    "FLOAT_KW");   }
torkik      { keyword_count++; return emit(BOOL_KW,     "BOOL_KW");    }
shunno      { keyword_count++; return emit(VOID_KW,     "VOID_KW");    }
dhrubo      { keyword_count++; return emit(CONST_KW,    "CONST_KW");   }
shotti      { keyword_count++; yylval.ival = 1; return emit(TRUE_KW,   "TRUE_KW");  }
mithya      { keyword_count++; yylval.ival = 0; return emit(FALSE_KW,  "FALSE_KW"); }
jodi        { keyword_count++; return emit(IF_KW,       "IF_KW");      }
nahole      { keyword_count++; return emit(ELSE_KW,     "ELSE_KW");    }
jotokhon    { keyword_count++; return emit(WHILE_KW,    "WHILE_KW");   }
ghuro       { keyword_count++; return emit(FOR_KW,      "FOR_KW");     }
koro        { keyword_count++; return emit(DO_KW,       "DO_KW");      }
bachai      { keyword_count++; return emit(SWITCH_KW,   "SWITCH_KW");  }
khetre      { keyword_count++; return emit(CASE_KW,     "CASE_KW");    }
onnothay    { keyword_count++; return emit(DEFAULT_KW,  "DEFAULT_KW"); }
tham        { keyword_count++; return emit(BREAK_KW,    "BREAK_KW");   }
chaliejao   { keyword_count++; return emit(CONTINUE_KW, "CONTINUE_KW");}
ferot       { keyword_count++; return emit(RETURN_KW,   "RETURN_KW");  }
dekhao      { keyword_count++; return emit(PRINT_KW,    "PRINT_KW");   }
neo         { keyword_count++; return emit(INPUT_KW,    "INPUT_KW");   }
kaj         { keyword_count++; return emit(FUNC_KW,     "FUNC_KW");    }
   


"<="   { operator_count++; division_seen=0; return emit(LE_OP,     "LE_OP");     }
">="   { operator_count++; division_seen=0; return emit(GE_OP,     "GE_OP");     }
"=="   { operator_count++; division_seen=0; return emit(EQ_OP,     "EQ_OP");     }
"!="   { operator_count++; division_seen=0; return emit(NE_OP,     "NE_OP");     }
"&&"   { operator_count++; division_seen=0; return emit(AND_OP,    "AND_OP");    }
"||"   { operator_count++; division_seen=0; return emit(OR_OP,     "OR_OP");     }
"++"   { operator_count++; division_seen=0; return emit(INC_OP,    "INC_OP");    }
"--"   { operator_count++; division_seen=0; return emit(DEC_OP,    "DEC_OP");    }
"+="   { operator_count++; division_seen=0; return emit(ADD_ASSIGN, "ADD_ASSIGN"); }
"-="   { operator_count++; division_seen=0; return emit(SUB_ASSIGN, "SUB_ASSIGN"); }
"*="   { operator_count++; division_seen=0; return emit(MUL_ASSIGN, "MUL_ASSIGN"); }
"/="   { operator_count++; division_seen=0; return emit(DIV_ASSIGN, "DIV_ASSIGN"); }


[0-9]+[a-zA-Z_][a-zA-Z0-9_]* {
    fprintf(out, "LEXICAL ERROR     | Invalid identifier '%-8s| line %d\n", yytext, line_no);
    error_count++;
}
[0-9]+\.[0-9]+\.[0-9]+ {
    fprintf(out, "LEXICAL ERROR     | Invalid number '%-11s| line %d\n", yytext, line_no);
    error_count++;
}
[0-9]+\.[0-9]+ {
    literal_count++;
    yylval.fval = strtod(yytext, NULL);
    if (division_seen && yylval.fval == 0.0) {
        fprintf(out, "SEMANTIC WARNING  | Division by zero            | line %d\n", line_no);
        warning_count++;
    }
    division_seen = 0;
    return emit(FLOAT_LIT, "FLOAT_LIT");
}
[0-9]+ {
    literal_count++;
    yylval.ival = atoi(yytext);
    if (division_seen && yylval.ival == 0) {
        fprintf(out, "SEMANTIC WARNING  | Division by zero            | line %d\n", line_no);
        warning_count++;
    }
    division_seen = 0;
    return emit(INT_LIT, "INT_LIT");
}
\"([^\\\n]|\\.)*\" {
    literal_count++;
    int len = (int)strlen(yytext) - 2;
    if (len < 0) len = 0;
    if (len > 255) len = 255;
    strncpy(yylval.sval, yytext + 1, len);
    yylval.sval[len] = '\0';
    division_seen = 0;
    return emit(STRING_LIT, "STRING_LIT");
}

'([^\\\n]|\\.)' {       
    literal_count++;
    yylval.sval[0] = yytext[1];
    yylval.sval[1] = '\0';
    division_seen = 0;
    return emit(CHAR_LIT, "CHAR_LIT");
}
\"[^\"\n]* {
    fprintf(out, "LEXICAL ERROR     | Unterminated string         | line %d\n", line_no);
    error_count++;
    division_seen = 0;
}


[a-zA-Z_][a-zA-Z0-9_]* {
    id_count++;
    strncpy(yylval.sval, yytext, 255);
    yylval.sval[255] = '\0';
    if (strlen(yytext) > 31) {
        fprintf(out, "WARNING           | Identifier too long         | line %d\n", line_no);
        warning_count++;
    }
    division_seen = 0;
    return emit(IDENTIFIER, "IDENTIFIER");
}
"/" {
    operator_count++;
    division_seen = 1;
    fprintf(out, "%-16s | %-22s | line %d\n", "OPERATOR", yytext, line_no);
    return '/';
}

[+\-*%=<>!] {
    operator_count++;
    division_seen = 0;
    fprintf(out, "%-16s | %-22s | line %d\n", "OPERATOR", yytext, line_no);
    return (int)yytext[0];
}

[;:,(){}\[\]] {
    delimiter_count++;
    division_seen = 0;
    fprintf(out, "%-16s | %-22s | line %d\n", "DELIMITER", yytext, line_no);
    return (int)yytext[0];
}
. {
    fprintf(out, "LEXICAL ERROR     | Illegal symbol '%-8s| line %d\n", yytext, line_no);
    error_count++;
}
%%


/* main() is now in parser.y — Bison's yyparse() drives yylex() */