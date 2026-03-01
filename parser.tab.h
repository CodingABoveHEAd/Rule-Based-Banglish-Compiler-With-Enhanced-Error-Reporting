/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSER_TAB_H_INCLUDED
# define YY_YY_PARSER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    INT_KW = 258,                  /* INT_KW  */
    FLOAT_KW = 259,                /* FLOAT_KW  */
    BOOL_KW = 260,                 /* BOOL_KW  */
    VOID_KW = 261,                 /* VOID_KW  */
    CONST_KW = 262,                /* CONST_KW  */
    TRUE_KW = 263,                 /* TRUE_KW  */
    FALSE_KW = 264,                /* FALSE_KW  */
    IF_KW = 265,                   /* IF_KW  */
    ELSE_KW = 266,                 /* ELSE_KW  */
    WHILE_KW = 267,                /* WHILE_KW  */
    FOR_KW = 268,                  /* FOR_KW  */
    DO_KW = 269,                   /* DO_KW  */
    SWITCH_KW = 270,               /* SWITCH_KW  */
    CASE_KW = 271,                 /* CASE_KW  */
    DEFAULT_KW = 272,              /* DEFAULT_KW  */
    BREAK_KW = 273,                /* BREAK_KW  */
    CONTINUE_KW = 274,             /* CONTINUE_KW  */
    RETURN_KW = 275,               /* RETURN_KW  */
    PRINT_KW = 276,                /* PRINT_KW  */
    INPUT_KW = 277,                /* INPUT_KW  */
    FUNC_KW = 278,                 /* FUNC_KW  */
    INT_LIT = 279,                 /* INT_LIT  */
    FLOAT_LIT = 280,               /* FLOAT_LIT  */
    STRING_LIT = 281,              /* STRING_LIT  */
    CHAR_LIT = 282,                /* CHAR_LIT  */
    IDENTIFIER = 283,              /* IDENTIFIER  */
    LE_OP = 284,                   /* LE_OP  */
    GE_OP = 285,                   /* GE_OP  */
    EQ_OP = 286,                   /* EQ_OP  */
    NE_OP = 287,                   /* NE_OP  */
    AND_OP = 288,                  /* AND_OP  */
    OR_OP = 289,                   /* OR_OP  */
    INC_OP = 290,                  /* INC_OP  */
    DEC_OP = 291,                  /* DEC_OP  */
    ADD_ASSIGN = 292,              /* ADD_ASSIGN  */
    SUB_ASSIGN = 293,              /* SUB_ASSIGN  */
    MUL_ASSIGN = 294,              /* MUL_ASSIGN  */
    DIV_ASSIGN = 295,              /* DIV_ASSIGN  */
    LOWER_THAN_ELSE = 296          /* LOWER_THAN_ELSE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 65 "parser.y"

    int    ival;
    double fval;
    char   sval[256];
    int    dtype;    /* DataType enum for type tracking */

#line 112 "parser.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_PARSER_TAB_H_INCLUDED  */
