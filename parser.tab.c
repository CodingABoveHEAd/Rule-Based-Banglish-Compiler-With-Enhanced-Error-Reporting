/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "parser.y"

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

#line 108 "parser.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "parser.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_INT_KW = 3,                     /* INT_KW  */
  YYSYMBOL_FLOAT_KW = 4,                   /* FLOAT_KW  */
  YYSYMBOL_BOOL_KW = 5,                    /* BOOL_KW  */
  YYSYMBOL_VOID_KW = 6,                    /* VOID_KW  */
  YYSYMBOL_CONST_KW = 7,                   /* CONST_KW  */
  YYSYMBOL_TRUE_KW = 8,                    /* TRUE_KW  */
  YYSYMBOL_FALSE_KW = 9,                   /* FALSE_KW  */
  YYSYMBOL_IF_KW = 10,                     /* IF_KW  */
  YYSYMBOL_ELSE_KW = 11,                   /* ELSE_KW  */
  YYSYMBOL_WHILE_KW = 12,                  /* WHILE_KW  */
  YYSYMBOL_FOR_KW = 13,                    /* FOR_KW  */
  YYSYMBOL_DO_KW = 14,                     /* DO_KW  */
  YYSYMBOL_SWITCH_KW = 15,                 /* SWITCH_KW  */
  YYSYMBOL_CASE_KW = 16,                   /* CASE_KW  */
  YYSYMBOL_DEFAULT_KW = 17,                /* DEFAULT_KW  */
  YYSYMBOL_BREAK_KW = 18,                  /* BREAK_KW  */
  YYSYMBOL_CONTINUE_KW = 19,               /* CONTINUE_KW  */
  YYSYMBOL_RETURN_KW = 20,                 /* RETURN_KW  */
  YYSYMBOL_PRINT_KW = 21,                  /* PRINT_KW  */
  YYSYMBOL_INPUT_KW = 22,                  /* INPUT_KW  */
  YYSYMBOL_FUNC_KW = 23,                   /* FUNC_KW  */
  YYSYMBOL_INT_LIT = 24,                   /* INT_LIT  */
  YYSYMBOL_FLOAT_LIT = 25,                 /* FLOAT_LIT  */
  YYSYMBOL_STRING_LIT = 26,                /* STRING_LIT  */
  YYSYMBOL_CHAR_LIT = 27,                  /* CHAR_LIT  */
  YYSYMBOL_IDENTIFIER = 28,                /* IDENTIFIER  */
  YYSYMBOL_LE_OP = 29,                     /* LE_OP  */
  YYSYMBOL_GE_OP = 30,                     /* GE_OP  */
  YYSYMBOL_EQ_OP = 31,                     /* EQ_OP  */
  YYSYMBOL_NE_OP = 32,                     /* NE_OP  */
  YYSYMBOL_AND_OP = 33,                    /* AND_OP  */
  YYSYMBOL_OR_OP = 34,                     /* OR_OP  */
  YYSYMBOL_INC_OP = 35,                    /* INC_OP  */
  YYSYMBOL_DEC_OP = 36,                    /* DEC_OP  */
  YYSYMBOL_ADD_ASSIGN = 37,                /* ADD_ASSIGN  */
  YYSYMBOL_SUB_ASSIGN = 38,                /* SUB_ASSIGN  */
  YYSYMBOL_MUL_ASSIGN = 39,                /* MUL_ASSIGN  */
  YYSYMBOL_DIV_ASSIGN = 40,                /* DIV_ASSIGN  */
  YYSYMBOL_41_ = 41,                       /* '='  */
  YYSYMBOL_42_ = 42,                       /* '<'  */
  YYSYMBOL_43_ = 43,                       /* '>'  */
  YYSYMBOL_44_ = 44,                       /* '+'  */
  YYSYMBOL_45_ = 45,                       /* '-'  */
  YYSYMBOL_46_ = 46,                       /* '*'  */
  YYSYMBOL_47_ = 47,                       /* '/'  */
  YYSYMBOL_48_ = 48,                       /* '%'  */
  YYSYMBOL_49_ = 49,                       /* '!'  */
  YYSYMBOL_LOWER_THAN_ELSE = 50,           /* LOWER_THAN_ELSE  */
  YYSYMBOL_51_ = 51,                       /* ';'  */
  YYSYMBOL_52_ = 52,                       /* '{'  */
  YYSYMBOL_53_ = 53,                       /* '}'  */
  YYSYMBOL_54_ = 54,                       /* '('  */
  YYSYMBOL_55_ = 55,                       /* ')'  */
  YYSYMBOL_56_ = 56,                       /* ':'  */
  YYSYMBOL_57_ = 57,                       /* ','  */
  YYSYMBOL_YYACCEPT = 58,                  /* $accept  */
  YYSYMBOL_program = 59,                   /* program  */
  YYSYMBOL_stmt_list = 60,                 /* stmt_list  */
  YYSYMBOL_stmt = 61,                      /* stmt  */
  YYSYMBOL_block = 62,                     /* block  */
  YYSYMBOL_decl_stmt = 63,                 /* decl_stmt  */
  YYSYMBOL_type_spec = 64,                 /* type_spec  */
  YYSYMBOL_assign_stmt = 65,               /* assign_stmt  */
  YYSYMBOL_if_stmt = 66,                   /* if_stmt  */
  YYSYMBOL_while_stmt = 67,                /* while_stmt  */
  YYSYMBOL_for_stmt = 68,                  /* for_stmt  */
  YYSYMBOL_for_init = 69,                  /* for_init  */
  YYSYMBOL_for_update = 70,                /* for_update  */
  YYSYMBOL_do_while_stmt = 71,             /* do_while_stmt  */
  YYSYMBOL_switch_stmt = 72,               /* switch_stmt  */
  YYSYMBOL_case_list = 73,                 /* case_list  */
  YYSYMBOL_case_clause = 74,               /* case_clause  */
  YYSYMBOL_print_stmt = 75,                /* print_stmt  */
  YYSYMBOL_input_stmt = 76,                /* input_stmt  */
  YYSYMBOL_return_stmt = 77,               /* return_stmt  */
  YYSYMBOL_break_stmt = 78,                /* break_stmt  */
  YYSYMBOL_continue_stmt = 79,             /* continue_stmt  */
  YYSYMBOL_func_def = 80,                  /* func_def  */
  YYSYMBOL_param_list = 81,                /* param_list  */
  YYSYMBOL_param = 82,                     /* param  */
  YYSYMBOL_expr_stmt = 83,                 /* expr_stmt  */
  YYSYMBOL_expr = 84,                      /* expr  */
  YYSYMBOL_arg_list = 85                   /* arg_list  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   781

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  58
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  28
/* YYNRULES -- Number of rules.  */
#define YYNRULES  96
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  196

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   296


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    49,     2,     2,     2,    48,     2,     2,
      54,    55,    46,    44,    57,    45,     2,    47,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    56,    51,
      42,    41,    43,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    52,     2,    53,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    50
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   102,   102,   106,   107,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   138,   148,   150,   152,   157,   158,   159,   160,   169,
     171,   173,   175,   177,   179,   181,   191,   193,   195,   205,
     215,   220,   221,   222,   226,   227,   228,   229,   230,   231,
     240,   250,   255,   256,   260,   262,   272,   282,   292,   294,
     303,   308,   319,   324,   325,   326,   330,   338,   346,   347,
     348,   349,   350,   351,   352,   353,   354,   355,   356,   357,
     358,   359,   360,   361,   362,   363,   364,   365,   366,   367,
     368,   369,   370,   371,   375,   376,   377
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "INT_KW", "FLOAT_KW",
  "BOOL_KW", "VOID_KW", "CONST_KW", "TRUE_KW", "FALSE_KW", "IF_KW",
  "ELSE_KW", "WHILE_KW", "FOR_KW", "DO_KW", "SWITCH_KW", "CASE_KW",
  "DEFAULT_KW", "BREAK_KW", "CONTINUE_KW", "RETURN_KW", "PRINT_KW",
  "INPUT_KW", "FUNC_KW", "INT_LIT", "FLOAT_LIT", "STRING_LIT", "CHAR_LIT",
  "IDENTIFIER", "LE_OP", "GE_OP", "EQ_OP", "NE_OP", "AND_OP", "OR_OP",
  "INC_OP", "DEC_OP", "ADD_ASSIGN", "SUB_ASSIGN", "MUL_ASSIGN",
  "DIV_ASSIGN", "'='", "'<'", "'>'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "'!'", "LOWER_THAN_ELSE", "';'", "'{'", "'}'", "'('", "')'", "':'",
  "','", "$accept", "program", "stmt_list", "stmt", "block", "decl_stmt",
  "type_spec", "assign_stmt", "if_stmt", "while_stmt", "for_stmt",
  "for_init", "for_update", "do_while_stmt", "switch_stmt", "case_list",
  "case_clause", "print_stmt", "input_stmt", "return_stmt", "break_stmt",
  "continue_stmt", "func_def", "param_list", "param", "expr_stmt", "expr",
  "arg_list", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-48)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-56)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -48,     3,    88,   -48,   -47,   -48,   -48,   -48,   -48,   140,
     -48,   -48,   -33,   -26,   -23,   -19,   -20,   -12,    -9,    -2,
     313,    23,   -48,   -48,   -48,   -48,    45,   313,   313,   -48,
     313,   -48,   -48,   -48,     4,   -48,   -48,   -48,   -48,   -48,
     -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   453,   -48,
      51,   313,   313,    32,    33,   313,   -48,   -48,    -6,   -48,
     476,   499,    36,    39,    53,   313,   313,   313,   313,   313,
     313,   -48,   -48,   282,   346,    83,   -36,   313,   313,   313,
     313,   313,   313,   313,   313,   313,   313,   313,   313,   313,
     -48,    78,   373,   393,    80,    96,    71,    73,   413,   -48,
     -48,   -48,   -48,   -48,   -48,   -48,   522,   545,   568,   591,
     614,   706,   -45,   -48,   -48,    74,   313,   -48,    17,    17,
     269,   269,   733,   713,    17,    17,   102,   102,   -48,   -48,
     -48,   313,   -19,   -19,   313,    95,   313,   313,    86,   -48,
     -48,   -48,   -48,   -48,   -48,   313,   140,   637,   660,   141,
     -48,   706,   313,   683,   433,   -48,   706,   123,   -37,   -48,
     -48,   -48,     1,   706,   126,   104,    -3,   -48,   -19,   140,
     -48,   -48,    94,   101,   -48,   313,   103,   -48,   -48,   -48,
     -48,   -48,   -48,   313,   313,   313,   -19,   339,   -48,   706,
     706,   706,   -48,   -48,   174,   228
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       3,     0,     0,     1,     0,    25,    26,    27,    28,     0,
      72,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    68,    69,    70,    71,    74,     0,     0,     3,
       0,     4,    18,     5,     0,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    19,     0,    20,
       0,     0,     0,    43,     0,     0,    60,    61,    74,    58,
       0,     0,     0,    92,    93,     0,     0,     0,     0,     0,
      94,    91,    90,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,     0,     0,     0,     0,     0,     0,     0,     0,    92,
      93,    59,    56,    57,    34,    35,     0,     0,     0,     0,
       0,    95,     0,    21,    76,     0,     0,    23,    84,    85,
      86,    87,    88,    89,    82,    83,    77,    78,    79,    80,
      81,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    29,    75,     0,    63,     0,     0,    36,
      39,    42,     0,     0,     0,    52,    96,     0,     0,    64,
      22,    24,     0,    41,    49,     0,     0,    66,     0,     0,
      37,    38,     0,     0,    50,     0,     0,    51,    53,    62,
      65,    44,    45,     0,     0,     0,     0,     0,     3,    47,
      48,    46,    40,     3,     0,     0
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -48,   -48,   -28,   -48,   -15,   -48,    -7,   -48,    -5,   -48,
     -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,   -48,
     -48,   -48,   -48,   -48,    -8,   -48,   -11,   -48
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     1,     2,    31,    32,    33,    34,    35,    36,    37,
      38,    96,   173,    39,    40,   166,   178,    41,    42,    43,
      44,    45,    46,   158,   159,    47,    48,   112
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      54,    73,    50,     3,    49,   116,    10,    11,    60,    61,
     144,    12,   145,   175,   176,   117,    71,    72,   168,    74,
     169,    51,    22,    23,    24,    25,    58,    75,    52,    99,
     100,    53,    76,    29,    55,     5,     6,     7,     8,    56,
      92,    93,    57,    27,    98,    97,    95,    28,    70,    59,
     177,    62,    30,    29,   106,   107,   108,   109,   110,   111,
      94,    85,    86,    87,    88,    89,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    91,
      63,    64,    65,    66,    67,    68,    69,   103,    -2,     4,
     104,     5,     6,     7,     8,     9,    10,    11,    12,    70,
      13,    14,    15,    16,   105,   147,    17,    18,    19,    20,
      21,   115,    22,    23,    24,    25,    26,   149,   150,   131,
     148,   134,   136,   151,   135,   153,   154,   137,   146,   181,
     182,   183,   184,    27,   156,   185,   152,    28,   155,   157,
      29,   163,    30,     5,     6,     7,     8,   170,    87,    88,
      89,   167,   162,   179,   172,   174,   186,   171,     0,   188,
     194,   180,   157,     0,   187,   195,     0,     0,     0,     0,
       0,   192,   189,   190,   191,     4,     0,     5,     6,     7,
       8,     9,    10,    11,    12,     0,    13,    14,    15,    16,
     -55,   -55,    17,    18,    19,    20,    21,     0,    22,    23,
      24,    25,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    27,
       0,     0,     0,    28,     0,     0,    29,   -55,    30,     4,
       0,     5,     6,     7,     8,     9,    10,    11,    12,     0,
      13,    14,    15,    16,   -54,   -54,    17,    18,    19,    20,
      21,     0,    22,    23,    24,    25,    26,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    27,     0,     0,     0,    28,     0,     0,
      29,   -54,    30,     4,     0,     5,     6,     7,     8,     9,
      10,    11,    12,     0,    13,    14,    15,    16,    77,    78,
      17,    18,    19,    20,    21,     0,    22,    23,    24,    25,
      26,    83,    84,    85,    86,    87,    88,    89,     0,     0,
       0,    10,    11,     0,     0,     0,     0,    27,     0,     0,
       0,    28,     0,     0,    29,   113,    30,    22,    23,    24,
      25,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    27,     0,
       0,     0,    28,     0,     0,     0,     0,    30,    77,    78,
      79,    80,    81,    82,     0,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    83,    84,
      85,    86,    87,    88,    89,   193,     0,     0,     0,     0,
       0,   114,    77,    78,    79,    80,    81,    82,     0,     0,
       0,     0,     0,     0,     0,    83,    84,    85,    86,    87,
      88,    89,    77,    78,    79,    80,    81,    82,   132,     0,
       0,     0,     0,     0,     0,    83,    84,    85,    86,    87,
      88,    89,    77,    78,    79,    80,    81,    82,   133,     0,
       0,     0,     0,     0,     0,    83,    84,    85,    86,    87,
      88,    89,    77,    78,    79,    80,    81,    82,   138,     0,
       0,     0,     0,     0,     0,    83,    84,    85,    86,    87,
      88,    89,    77,    78,    79,    80,    81,    82,   165,     0,
       0,     0,     0,     0,     0,    83,    84,    85,    86,    87,
      88,    89,     0,     0,    90,    77,    78,    79,    80,    81,
      82,     0,     0,     0,     0,     0,     0,     0,    83,    84,
      85,    86,    87,    88,    89,     0,     0,   101,    77,    78,
      79,    80,    81,    82,     0,     0,     0,     0,     0,     0,
       0,    83,    84,    85,    86,    87,    88,    89,     0,     0,
     102,    77,    78,    79,    80,    81,    82,     0,     0,     0,
       0,     0,     0,     0,    83,    84,    85,    86,    87,    88,
      89,     0,     0,   139,    77,    78,    79,    80,    81,    82,
       0,     0,     0,     0,     0,     0,     0,    83,    84,    85,
      86,    87,    88,    89,     0,     0,   140,    77,    78,    79,
      80,    81,    82,     0,     0,     0,     0,     0,     0,     0,
      83,    84,    85,    86,    87,    88,    89,     0,     0,   141,
      77,    78,    79,    80,    81,    82,     0,     0,     0,     0,
       0,     0,     0,    83,    84,    85,    86,    87,    88,    89,
       0,     0,   142,    77,    78,    79,    80,    81,    82,     0,
       0,     0,     0,     0,     0,     0,    83,    84,    85,    86,
      87,    88,    89,     0,     0,   143,    77,    78,    79,    80,
      81,    82,     0,     0,     0,     0,     0,     0,     0,    83,
      84,    85,    86,    87,    88,    89,     0,     0,   160,    77,
      78,    79,    80,    81,    82,     0,     0,     0,     0,     0,
       0,     0,    83,    84,    85,    86,    87,    88,    89,     0,
       0,   161,    77,    78,    79,    80,    81,    82,     0,     0,
       0,     0,     0,     0,     0,    83,    84,    85,    86,    87,
      88,    89,     0,     0,   164,    77,    78,    79,    80,    81,
      82,     0,    77,    78,    79,    80,    81,     0,    83,    84,
      85,    86,    87,    88,    89,    83,    84,    85,    86,    87,
      88,    89,    77,    78,    79,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,    85,    86,    87,
      88,    89
};

static const yytype_int16 yycheck[] =
{
      15,    29,     9,     0,    51,    41,     8,     9,    19,    20,
      55,    10,    57,    16,    17,    51,    27,    28,    55,    30,
      57,    54,    24,    25,    26,    27,    28,    23,    54,    35,
      36,    54,    28,    52,    54,     3,     4,     5,     6,    51,
      51,    52,    51,    45,    55,    12,    53,    49,    54,    51,
      53,    28,    54,    52,    65,    66,    67,    68,    69,    70,
      28,    44,    45,    46,    47,    48,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    28,
      35,    36,    37,    38,    39,    40,    41,    51,     0,     1,
      51,     3,     4,     5,     6,     7,     8,     9,    10,    54,
      12,    13,    14,    15,    51,   116,    18,    19,    20,    21,
      22,    28,    24,    25,    26,    27,    28,   132,   133,    41,
     131,    41,    51,   134,    28,   136,   137,    54,    54,    35,
      36,    37,    38,    45,   145,    41,    41,    49,    52,   146,
      52,   152,    54,     3,     4,     5,     6,   162,    46,    47,
      48,    28,    11,   168,    28,    51,    55,   162,    -1,    56,
     188,   169,   169,    -1,   175,   193,    -1,    -1,    -1,    -1,
      -1,   186,   183,   184,   185,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    -1,    -1,    49,    -1,    -1,    52,    53,    54,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    -1,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    -1,    -1,    49,    -1,    -1,
      52,    53,    54,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    29,    30,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    42,    43,    44,    45,    46,    47,    48,    -1,    -1,
      -1,     8,     9,    -1,    -1,    -1,    -1,    45,    -1,    -1,
      -1,    49,    -1,    -1,    52,    53,    54,    24,    25,    26,
      27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      -1,    -1,    49,    -1,    -1,    -1,    -1,    54,    29,    30,
      31,    32,    33,    34,    -1,    29,    30,    31,    32,    33,
      34,    42,    43,    44,    45,    46,    47,    48,    42,    43,
      44,    45,    46,    47,    48,    56,    -1,    -1,    -1,    -1,
      -1,    55,    29,    30,    31,    32,    33,    34,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    29,    30,    31,    32,    33,    34,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    29,    30,    31,    32,    33,    34,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    29,    30,    31,    32,    33,    34,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    29,    30,    31,    32,    33,    34,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    -1,    -1,    51,    29,    30,    31,    32,    33,
      34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    -1,    -1,    51,    29,    30,
      31,    32,    33,    34,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    -1,    -1,
      51,    29,    30,    31,    32,    33,    34,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    -1,    -1,    51,    29,    30,    31,    32,    33,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    43,    44,
      45,    46,    47,    48,    -1,    -1,    51,    29,    30,    31,
      32,    33,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    -1,    -1,    51,
      29,    30,    31,    32,    33,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      -1,    -1,    51,    29,    30,    31,    32,    33,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,
      46,    47,    48,    -1,    -1,    51,    29,    30,    31,    32,
      33,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    -1,    -1,    51,    29,
      30,    31,    32,    33,    34,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    43,    44,    45,    46,    47,    48,    -1,
      -1,    51,    29,    30,    31,    32,    33,    34,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    -1,    -1,    51,    29,    30,    31,    32,    33,
      34,    -1,    29,    30,    31,    32,    33,    -1,    42,    43,
      44,    45,    46,    47,    48,    42,    43,    44,    45,    46,
      47,    48,    29,    30,    31,    32,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    59,    60,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    12,    13,    14,    15,    18,    19,    20,
      21,    22,    24,    25,    26,    27,    28,    45,    49,    52,
      54,    61,    62,    63,    64,    65,    66,    67,    68,    71,
      72,    75,    76,    77,    78,    79,    80,    83,    84,    51,
      64,    54,    54,    54,    62,    54,    51,    51,    28,    51,
      84,    84,    28,    35,    36,    37,    38,    39,    40,    41,
      54,    84,    84,    60,    84,    23,    28,    29,    30,    31,
      32,    33,    34,    42,    43,    44,    45,    46,    47,    48,
      51,    28,    84,    84,    28,    64,    69,    12,    84,    35,
      36,    51,    51,    51,    51,    51,    84,    84,    84,    84,
      84,    84,    85,    53,    55,    28,    41,    51,    84,    84,
      84,    84,    84,    84,    84,    84,    84,    84,    84,    84,
      84,    41,    55,    55,    41,    28,    51,    54,    55,    51,
      51,    51,    51,    51,    55,    57,    54,    84,    84,    62,
      62,    84,    41,    84,    84,    52,    84,    64,    81,    82,
      51,    51,    11,    84,    51,    55,    73,    28,    55,    57,
      62,    66,    28,    70,    51,    16,    17,    53,    74,    62,
      82,    35,    36,    37,    38,    41,    55,    84,    56,    84,
      84,    84,    62,    56,    60,    60
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    58,    59,    60,    60,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    62,    63,    63,    63,    64,    64,    64,    64,    65,
      65,    65,    65,    65,    65,    65,    66,    66,    66,    67,
      68,    69,    69,    69,    70,    70,    70,    70,    70,    70,
      71,    72,    73,    73,    74,    74,    75,    76,    77,    77,
      78,    79,    80,    81,    81,    81,    82,    83,    84,    84,
      84,    84,    84,    84,    84,    84,    84,    84,    84,    84,
      84,    84,    84,    84,    84,    84,    84,    84,    84,    84,
      84,    84,    84,    84,    85,    85,    85
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     0,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     3,     5,     3,     6,     1,     1,     1,     1,     4,
       4,     4,     4,     4,     3,     3,     5,     7,     7,     5,
       9,     4,     3,     0,     2,     2,     3,     3,     3,     0,
       7,     7,     0,     2,     4,     3,     3,     3,     2,     3,
       2,     2,     7,     0,     1,     3,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     4,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       2,     2,     2,     2,     0,     1,     3
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: stmt_list  */
#line 102 "parser.y"
                            { fprintf(out, "\n[PARSER] Program parsed successfully.\n"); }
#line 1427 "parser.tab.c"
    break;

  case 20: /* stmt: error ';'  */
#line 130 "parser.y"
                            { yyerrok; /* panic-mode recovery */ }
#line 1433 "parser.tab.c"
    break;

  case 22: /* decl_stmt: type_spec IDENTIFIER '=' expr ';'  */
#line 149 "parser.y"
        { fprintf(out, "[PARSER] Declaration: %s (with init) at line %d\n", (yyvsp[-3].sval), line_no); }
#line 1439 "parser.tab.c"
    break;

  case 23: /* decl_stmt: type_spec IDENTIFIER ';'  */
#line 151 "parser.y"
        { fprintf(out, "[PARSER] Declaration: %s (no init) at line %d\n", (yyvsp[-1].sval), line_no); }
#line 1445 "parser.tab.c"
    break;

  case 24: /* decl_stmt: CONST_KW type_spec IDENTIFIER '=' expr ';'  */
#line 153 "parser.y"
        { fprintf(out, "[PARSER] Const declaration: %s at line %d\n", (yyvsp[-3].sval), line_no); }
#line 1451 "parser.tab.c"
    break;

  case 29: /* assign_stmt: IDENTIFIER '=' expr ';'  */
#line 170 "parser.y"
        { fprintf(out, "[PARSER] Assignment: %s at line %d\n", (yyvsp[-3].sval), line_no); }
#line 1457 "parser.tab.c"
    break;

  case 30: /* assign_stmt: IDENTIFIER ADD_ASSIGN expr ';'  */
#line 172 "parser.y"
        { fprintf(out, "[PARSER] Assignment: %s += at line %d\n", (yyvsp[-3].sval), line_no); }
#line 1463 "parser.tab.c"
    break;

  case 31: /* assign_stmt: IDENTIFIER SUB_ASSIGN expr ';'  */
#line 174 "parser.y"
        { fprintf(out, "[PARSER] Assignment: %s -= at line %d\n", (yyvsp[-3].sval), line_no); }
#line 1469 "parser.tab.c"
    break;

  case 32: /* assign_stmt: IDENTIFIER MUL_ASSIGN expr ';'  */
#line 176 "parser.y"
        { fprintf(out, "[PARSER] Assignment: %s *= at line %d\n", (yyvsp[-3].sval), line_no); }
#line 1475 "parser.tab.c"
    break;

  case 33: /* assign_stmt: IDENTIFIER DIV_ASSIGN expr ';'  */
#line 178 "parser.y"
        { fprintf(out, "[PARSER] Assignment: %s /= at line %d\n", (yyvsp[-3].sval), line_no); }
#line 1481 "parser.tab.c"
    break;

  case 34: /* assign_stmt: IDENTIFIER INC_OP ';'  */
#line 180 "parser.y"
        { fprintf(out, "[PARSER] Increment: %s at line %d\n", (yyvsp[-2].sval), line_no); }
#line 1487 "parser.tab.c"
    break;

  case 35: /* assign_stmt: IDENTIFIER DEC_OP ';'  */
#line 182 "parser.y"
        { fprintf(out, "[PARSER] Decrement: %s at line %d\n", (yyvsp[-2].sval), line_no); }
#line 1493 "parser.tab.c"
    break;

  case 36: /* if_stmt: IF_KW '(' expr ')' block  */
#line 192 "parser.y"
        { fprintf(out, "[PARSER] If statement at line %d\n", line_no); }
#line 1499 "parser.tab.c"
    break;

  case 37: /* if_stmt: IF_KW '(' expr ')' block ELSE_KW block  */
#line 194 "parser.y"
        { fprintf(out, "[PARSER] If-Else statement at line %d\n", line_no); }
#line 1505 "parser.tab.c"
    break;

  case 38: /* if_stmt: IF_KW '(' expr ')' block ELSE_KW if_stmt  */
#line 196 "parser.y"
        { fprintf(out, "[PARSER] If-Else-If chain at line %d\n", line_no); }
#line 1511 "parser.tab.c"
    break;

  case 39: /* while_stmt: WHILE_KW '(' expr ')' block  */
#line 206 "parser.y"
        { fprintf(out, "[PARSER] While loop at line %d\n", line_no); }
#line 1517 "parser.tab.c"
    break;

  case 40: /* for_stmt: FOR_KW '(' for_init ';' expr ';' for_update ')' block  */
#line 216 "parser.y"
        { fprintf(out, "[PARSER] For loop at line %d\n", line_no); }
#line 1523 "parser.tab.c"
    break;

  case 50: /* do_while_stmt: DO_KW block WHILE_KW '(' expr ')' ';'  */
#line 241 "parser.y"
        { fprintf(out, "[PARSER] Do-While loop at line %d\n", line_no); }
#line 1529 "parser.tab.c"
    break;

  case 51: /* switch_stmt: SWITCH_KW '(' expr ')' '{' case_list '}'  */
#line 251 "parser.y"
        { fprintf(out, "[PARSER] Switch statement at line %d\n", line_no); }
#line 1535 "parser.tab.c"
    break;

  case 54: /* case_clause: CASE_KW expr ':' stmt_list  */
#line 261 "parser.y"
        { fprintf(out, "[PARSER] Case clause at line %d\n", line_no); }
#line 1541 "parser.tab.c"
    break;

  case 55: /* case_clause: DEFAULT_KW ':' stmt_list  */
#line 263 "parser.y"
        { fprintf(out, "[PARSER] Default clause at line %d\n", line_no); }
#line 1547 "parser.tab.c"
    break;

  case 56: /* print_stmt: PRINT_KW expr ';'  */
#line 273 "parser.y"
        { fprintf(out, "[PARSER] Print statement at line %d\n", line_no); }
#line 1553 "parser.tab.c"
    break;

  case 57: /* input_stmt: INPUT_KW IDENTIFIER ';'  */
#line 283 "parser.y"
        { fprintf(out, "[PARSER] Input statement: %s at line %d\n", (yyvsp[-1].sval), line_no); }
#line 1559 "parser.tab.c"
    break;

  case 58: /* return_stmt: RETURN_KW ';'  */
#line 293 "parser.y"
        { fprintf(out, "[PARSER] Return (void) at line %d\n", line_no); }
#line 1565 "parser.tab.c"
    break;

  case 59: /* return_stmt: RETURN_KW expr ';'  */
#line 295 "parser.y"
        { fprintf(out, "[PARSER] Return (with value) at line %d\n", line_no); }
#line 1571 "parser.tab.c"
    break;

  case 60: /* break_stmt: BREAK_KW ';'  */
#line 304 "parser.y"
        { fprintf(out, "[PARSER] Break at line %d\n", line_no); }
#line 1577 "parser.tab.c"
    break;

  case 61: /* continue_stmt: CONTINUE_KW ';'  */
#line 309 "parser.y"
        { fprintf(out, "[PARSER] Continue at line %d\n", line_no); }
#line 1583 "parser.tab.c"
    break;

  case 62: /* func_def: type_spec FUNC_KW IDENTIFIER '(' param_list ')' block  */
#line 320 "parser.y"
        { fprintf(out, "[PARSER] Function def: %s at line %d\n", (yyvsp[-4].sval), line_no); }
#line 1589 "parser.tab.c"
    break;


#line 1593 "parser.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 380 "parser.y"


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
