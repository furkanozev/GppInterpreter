/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "gpp_parser.y" /* yacc.c:339  */

	#include <stdio.h>
	#include <string.h>

	int string_in(char *my_str, char string_list[1000][200], int num_strings)
	{
		int i;
	    for(i = 0; i < num_strings; i++ )
	        if (strcmp(my_str, string_list[i]) == 0 )
	            return i;

	    return -1;
	}

	void upper_string(char s[])
	{
   		int c = 0;
   
   		while (s[c] != '\0') {
      		if (s[c] >= 'a' && s[c] <= 'z') {
         		s[c] = s[c] - 32;
      		}
      		c++;
   		}
	}

	void copy(int a[], int b[], int size)
	{
		int i;
		for(i = 0; i < size && i < 999; i++)
			a[i] = b[i];
	}

	struct a1
	{ 
	   char iden[1000][200]; 
	   int values[1000]; 
	   int count;
	};

	struct a1 idens = {"", 0, 0};

	int i = 2;
	int y = 0;
	int z = 0;
	int sum = 0;


#line 115 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    KW_AND = 258,
    KW_OR = 259,
    KW_NOT = 260,
    KW_EQUAL = 261,
    KW_LESS = 262,
    KW_NIL = 263,
    KW_LIST = 264,
    KW_APPEND = 265,
    KW_CONCAT = 266,
    KW_SET = 267,
    KW_DEFFUN = 268,
    KW_DEFVAR = 269,
    KW_FOR = 270,
    KW_IF = 271,
    KW_EXIT = 272,
    KW_LOAD = 273,
    KW_DISP = 274,
    KW_TRUE = 275,
    KW_FALSE = 276,
    OP_PLUS = 277,
    OP_MINUS = 278,
    OP_DIV = 279,
    OP_DBLMULT = 280,
    OP_MULT = 281,
    OP_OP = 282,
    OP_CP = 283,
    OP_OC = 284,
    OP_CC = 285,
    OP_COMMA = 286,
    COMMENT = 287,
    IDENTIFIER = 288,
    VALUE = 289,
    CUT = 290,
    FILENAME = 291
  };
#endif
/* Tokens.  */
#define KW_AND 258
#define KW_OR 259
#define KW_NOT 260
#define KW_EQUAL 261
#define KW_LESS 262
#define KW_NIL 263
#define KW_LIST 264
#define KW_APPEND 265
#define KW_CONCAT 266
#define KW_SET 267
#define KW_DEFFUN 268
#define KW_DEFVAR 269
#define KW_FOR 270
#define KW_IF 271
#define KW_EXIT 272
#define KW_LOAD 273
#define KW_DISP 274
#define KW_TRUE 275
#define KW_FALSE 276
#define OP_PLUS 277
#define OP_MINUS 278
#define OP_DIV 279
#define OP_DBLMULT 280
#define OP_MULT 281
#define OP_OP 282
#define OP_CP 283
#define OP_OC 284
#define OP_CC 285
#define OP_COMMA 286
#define COMMENT 287
#define IDENTIFIER 288
#define VALUE 289
#define CUT 290
#define FILENAME 291

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 59 "gpp_parser.y" /* yacc.c:355  */

	struct {
		int flag;
        int number;
        char *string;
        int values[1000];
    };

#line 236 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 253 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

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

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
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


#if ! defined yyoverflow || YYERROR_VERBOSE

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
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
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
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   187

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  37
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  9
/* YYNRULES -- Number of rules.  */
#define YYNRULES  51
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  138

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   291

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      35,    36
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    70,    70,    70,    73,    97,   121,   124,   128,   134,
     139,   156,   159,   166,   179,   180,   181,   182,   190,   198,
     199,   200,   215,   219,   231,   243,   255,   267,   271,   279,
     284,   294,   302,   310,   318,   319,   329,   332,   335,   336,
     337,   338,   339,   340,   341,   342,   343,   344,   360,   368,
     376,   384
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "KW_AND", "KW_OR", "KW_NOT", "KW_EQUAL",
  "KW_LESS", "KW_NIL", "KW_LIST", "KW_APPEND", "KW_CONCAT", "KW_SET",
  "KW_DEFFUN", "KW_DEFVAR", "KW_FOR", "KW_IF", "KW_EXIT", "KW_LOAD",
  "KW_DISP", "KW_TRUE", "KW_FALSE", "OP_PLUS", "OP_MINUS", "OP_DIV",
  "OP_DBLMULT", "OP_MULT", "OP_OP", "OP_CP", "OP_OC", "OP_CC", "OP_COMMA",
  "COMMENT", "IDENTIFIER", "VALUE", "CUT", "FILENAME", "$accept", "START",
  "INPUT", "LISTVALUE", "VALUES", "IDENTIFIERS", "EXPI", "EXPB",
  "EXPLISTI", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291
};
# endif

#define YYPACT_NINF -26

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-26)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -26,    24,   -26,   -26,   -26,   -26,    71,   -26,   -26,   -26,
     -16,   -26,   -26,   -26,   -26,   -26,   128,   128,   128,   128,
     128,   -11,    34,   -25,     1,     2,     4,    12,   128,    13,
     -17,    34,   152,   152,   152,   152,   152,    34,   -11,   163,
     128,   128,    15,   128,   128,   -26,   109,   -25,   -25,    53,
     -25,    34,   -21,    34,    32,    34,   -26,    37,    42,    51,
     102,   152,   152,   152,   152,   152,    63,    64,   112,    70,
      72,   -26,    73,    74,   -26,   -26,   -26,    84,    85,    94,
      95,   106,    -6,   110,   116,   119,   152,   131,    25,   120,
     -26,   -26,   135,   143,   146,   147,   148,   -26,   -26,   -26,
     -26,   -26,   -26,   -26,   -26,   -26,   -26,   -26,   -26,   -26,
     -20,   -26,   -26,   -26,   152,   -26,   149,   -26,   150,   153,
     -26,   -26,   -26,   -26,   -26,    34,   -26,   154,   -26,   -26,
     -26,   155,   156,   -25,   -26,   -26,   159,   -26
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     1,    46,    44,    45,     0,     6,    47,    43,
       0,     3,    51,     4,    37,     5,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    11,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    34,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    40,     0,     0,     7,    10,     9,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      35,    36,     0,     0,     0,     0,     0,    20,    19,     8,
      38,    39,    41,    42,    50,    49,    48,    21,    22,    13,
       0,    33,    28,    29,     0,    25,     0,    23,     0,     0,
      14,    15,    17,    18,    16,     0,    12,     0,    26,    24,
      30,     0,     0,     0,    31,    32,     0,    27
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -26,   -26,   -26,   -26,   142,   -26,   -15,   113,   -22
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     1,    11,    12,    46,   110,    13,    14,    15
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      48,    50,    49,    21,    22,    23,    82,    47,   125,    59,
      10,    38,    57,   126,    10,    67,    58,    61,    62,    63,
      64,    65,    66,    45,     2,    77,    78,   109,    79,    81,
      83,    85,     3,    88,    51,    52,    80,    53,    84,    54,
      87,    56,     3,    71,     4,     5,    92,    93,    94,    95,
      96,     6,    49,   117,     4,     5,     7,     8,     9,    10,
      10,     6,    21,    22,    23,    86,   118,     8,     9,    10,
      90,   114,   116,    89,    16,    17,    18,    19,    20,    91,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    97,    98,    32,    33,    34,    35,    36,   100,   127,
     101,   102,   103,   132,    37,    16,    17,    18,    19,    20,
     131,   136,   104,   105,    24,    25,    26,    27,    28,    29,
      30,    31,   106,   107,    32,    33,    34,    35,    36,    40,
      41,    42,    43,    44,   108,    37,     3,    74,   111,     3,
      99,    55,    75,    76,   112,    75,    76,   113,     4,     5,
     119,     4,     5,    69,    70,    39,    72,    73,    60,   115,
       3,     8,     9,   120,     8,     9,    16,    17,    18,    19,
      20,   121,     4,     5,   122,   123,   124,   128,   129,    60,
      68,   130,   133,   134,   135,     8,     9,   137
};

static const yytype_uint8 yycheck[] =
{
      22,    23,    27,     9,    10,    11,    27,    22,    28,    31,
      35,    27,    29,    33,    35,    37,    31,    32,    33,    34,
      35,    36,    37,    34,     0,    47,    48,    33,    50,    51,
      52,    53,     8,    55,    33,    33,    51,    33,    53,    27,
      55,    28,     8,    28,    20,    21,    61,    62,    63,    64,
      65,    27,    27,    28,    20,    21,    32,    33,    34,    35,
      35,    27,     9,    10,    11,    33,    88,    33,    34,    35,
      28,    86,    87,    36,     3,     4,     5,     6,     7,    28,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    28,    28,    22,    23,    24,    25,    26,    28,   114,
      28,    28,    28,   125,    33,     3,     4,     5,     6,     7,
     125,   133,    28,    28,    12,    13,    14,    15,    16,    17,
      18,    19,    28,    28,    22,    23,    24,    25,    26,    16,
      17,    18,    19,    20,    28,    33,     8,    28,    28,     8,
      28,    28,    33,    34,    28,    33,    34,    28,    20,    21,
      30,    20,    21,    40,    41,    27,    43,    44,    27,    28,
       8,    33,    34,    28,    33,    34,     3,     4,     5,     6,
       7,    28,    20,    21,    28,    28,    28,    28,    28,    27,
      38,    28,    28,    28,    28,    33,    34,    28
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    38,     0,     8,    20,    21,    27,    32,    33,    34,
      35,    39,    40,    43,    44,    45,     3,     4,     5,     6,
       7,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    22,    23,    24,    25,    26,    33,    27,    27,
      44,    44,    44,    44,    44,    34,    41,    43,    45,    27,
      45,    33,    33,    33,    27,    44,    28,    29,    43,    45,
      27,    43,    43,    43,    43,    43,    43,    45,    41,    44,
      44,    28,    44,    44,    28,    33,    34,    45,    45,    45,
      43,    45,    27,    45,    43,    45,    33,    43,    45,    36,
      28,    28,    43,    43,    43,    43,    43,    28,    28,    28,
      28,    28,    28,    28,    28,    28,    28,    28,    28,    33,
      42,    28,    28,    28,    43,    28,    43,    28,    45,    30,
      28,    28,    28,    28,    28,    28,    33,    43,    28,    28,
      28,    43,    45,    28,    28,    28,    45,    28
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    37,    38,    38,    39,    39,    39,    40,    40,    41,
      41,    41,    42,    42,    43,    43,    43,    43,    43,    43,
      43,    43,    43,    43,    43,    43,    43,    43,    43,    43,
      43,    43,    43,    43,    43,    43,    43,    43,    44,    44,
      44,    44,    44,    44,    44,    44,    44,    44,    45,    45,
      45,    45
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     1,     4,     4,     2,
       2,     1,     2,     1,     5,     5,     5,     5,     5,     4,
       4,     5,     5,     5,     6,     5,     6,     9,     5,     5,
       6,     8,     8,     5,     3,     4,     4,     1,     5,     5,
       4,     5,     5,     1,     1,     1,     1,     1,     5,     5,
       5,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
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

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



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

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
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
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
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
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
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


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
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
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

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

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
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

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
| yyreduce -- Do a reduction.  |
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
        case 3:
#line 70 "gpp_parser.y" /* yacc.c:1646  */
    {printf("[%d]> ", i); i += 1; y = 0; z = 0; sum = 0;}
#line 1421 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 73 "gpp_parser.y" /* yacc.c:1646  */
    {
				if((yyval.flag) == 0)
				{printf("%d\n", (yyvsp[0].number)); (yyval.number) = (yyvsp[0].number); (yyval.flag) = 0;}
				else if((yyval.flag) == 1)
				{printf("%s\n", (yyvsp[0].string)); (yyval.string) = (yyvsp[0].string); (yyval.flag) = 1;}
				else if((yyval.flag) == 2)
				{
					if((yyval.number) == 1) printf("TRUE\n");
					else printf("FALSE\n");
					(yyval.number) = (yyvsp[0].number);
					(yyval.flag) = 2;
				}
				else if((yyval.flag) == 3)
				{
					int j;
					printf("(");
					for(j = 0; j < z; j += 1)
    					{printf("%d", (yyvsp[0].values)[j]);
    					if(j != z-1) printf(" ");}
    				printf(")\n");
    				copy((yyval.values), (yyvsp[0].values), z);
					(yyval.flag) = 3;
				}
			}
#line 1450 "y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 97 "gpp_parser.y" /* yacc.c:1646  */
    {
						if((yyval.flag) == 0)
						{printf("%d\n", (yyvsp[0].number)); (yyval.number) = (yyvsp[0].number); (yyval.flag) = 0;}
						else if((yyval.flag) == 1)
						{printf("%s\n", (yyvsp[0].string)); (yyval.string) = (yyvsp[0].string); (yyval.flag) = 1;}
						else if((yyval.flag) == 2)
						{
							if((yyval.number) == 1) printf("T\n");
							else printf("NIL\n");
							(yyval.number) = (yyvsp[0].number);
							(yyval.flag) = 2;
						}
						else if((yyval.flag) == 3)
						{
							int j;
							printf("(");
							for(j = 0; j < z; j += 1)
            					{printf("%d", (yyvsp[0].values)[j]);
            					if(j != z-1) printf(" ");}
            				printf(")\n");
            				copy((yyval.values), (yyvsp[0].values), z);
							(yyval.flag) = 3;
						}
					}
#line 1479 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 121 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.flag) = 5;}
#line 1485 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 124 "gpp_parser.y" /* yacc.c:1646  */
    {
										copy((yyval.values), (yyvsp[-1].values), y);
										(yyval.flag) = 3;
									}
#line 1494 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 128 "gpp_parser.y" /* yacc.c:1646  */
    {
										copy((yyval.values), (yyvsp[-1].values), y);
										(yyval.flag) = 3;
								 	}
#line 1503 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 134 "gpp_parser.y" /* yacc.c:1646  */
    {
						(yyval.values)[y] = (yyvsp[0].number);
						y += 1;
						(yyval.flag) = 3;
					 }
#line 1513 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 139 "gpp_parser.y" /* yacc.c:1646  */
    {
							upper_string((yyvsp[0].string));
							int res = string_in((yyvsp[0].string), idens.iden, idens.count);
							if(res != -1)
							{
								(yyvsp[0].number) = idens.values[res];
								(yyval.flag) = 0;
							}
							else
							{
								printf(" variable %s has no value\n", (yyvsp[0].string));
								exit(0);
							}
							(yyval.values)[y] = (yyvsp[0].number);
							y += 1;
							(yyval.flag) = 3;
						 }
#line 1535 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 156 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.values)[y] = (yyvsp[0].number); y += 1; (yyval.flag) = 3;}
#line 1541 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 159 "gpp_parser.y" /* yacc.c:1646  */
    {
								upper_string((yyvsp[-1].string));
								(yyval.string) = (yyvsp[-1].string);
								strcat((yyval.string), " ");
								strcat((yyval.string), (yyvsp[0].string));
								(yyval.flag) = 1;
							  }
#line 1553 "y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 166 "gpp_parser.y" /* yacc.c:1646  */
    {
						upper_string((yyvsp[0].string));
						int res = string_in((yyvsp[0].string), idens.iden, idens.count);
						if(res == -1)
						{
							strcpy(idens.iden[idens.count], (yyvsp[0].string));
							idens.values[idens.count] = -1;
							idens.count += 1;
						}
						(yyval.string) = (yyvsp[0].string); (yyval.flag) = 1;
					 }
#line 1569 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 179 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = (yyvsp[-2].number) + (yyvsp[-1].number); (yyval.flag) = 0;}
#line 1575 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 180 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = (yyvsp[-2].number) - (yyvsp[-1].number); (yyval.flag) = 0;}
#line 1581 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 181 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = (yyvsp[-2].number) * (yyvsp[-1].number); (yyval.flag) = 0;}
#line 1587 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 182 "gpp_parser.y" /* yacc.c:1646  */
    {
										if((yyvsp[-1].number) == 0)
											{printf("Can not divide by zero\n");
											exit(0);}
										else
											{(yyval.number) = (yyvsp[-2].number) / (yyvsp[-1].number);
											(yyval.flag) = 0;}
										}
#line 1600 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 190 "gpp_parser.y" /* yacc.c:1646  */
    {
											int x = (yyvsp[-2].number);
											int j;
											for(j = 1;j<(yyvsp[-1].number);j++)
												x *=(yyvsp[-2].number);
											(yyval.number) = x;
											(yyval.flag) = 0;
										   }
#line 1613 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 198 "gpp_parser.y" /* yacc.c:1646  */
    {upper_string((yyvsp[-2].string)); (yyval.string) = (yyvsp[-2].string); (yyval.flag) = 1;}
#line 1619 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 199 "gpp_parser.y" /* yacc.c:1646  */
    {upper_string((yyvsp[-2].string)); (yyval.string) = (yyvsp[-2].string); (yyval.flag) = 1;}
#line 1625 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 200 "gpp_parser.y" /* yacc.c:1646  */
    {
											upper_string((yyvsp[-2].string));
											int res = string_in((yyvsp[-2].string), idens.iden, idens.count);
											if(res == -1)
											{
												strcpy(idens.iden[idens.count], (yyvsp[-2].string));
												idens.values[idens.count] = (yyvsp[-1].number);
												idens.count += 1;
											}
											else
												{idens.values[res] = (yyvsp[-1].number);}

											(yyval.number) = (yyvsp[-1].number);
											(yyval.flag) = 0;
											}
#line 1645 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 215 "gpp_parser.y" /* yacc.c:1646  */
    {
													copy((yyval.values), (yyvsp[-1].values), z);
													(yyval.flag) = 3;
												}
#line 1654 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 219 "gpp_parser.y" /* yacc.c:1646  */
    {
											if((yyvsp[-2].number) == 1)
											{
												copy((yyval.values), (yyvsp[-1].values), z);
												(yyval.flag) = 3;
											}
											else
											{
												(yyval.number) = 0;
												(yyval.flag) = 3;
											}
										 }
#line 1671 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 231 "gpp_parser.y" /* yacc.c:1646  */
    {
													if((yyvsp[-3].number) == 1)
													{
														copy((yyval.values), (yyvsp[-2].values), z);
														(yyval.flag) = 3;
													}
													else
													{
														copy((yyval.values), (yyvsp[-1].values), z);
														(yyval.flag) = 3;
													}
												 }
#line 1688 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 243 "gpp_parser.y" /* yacc.c:1646  */
    {
										if((yyvsp[-2].number) == 1)
										{
											(yyval.number) = (yyvsp[-1].number);
											(yyval.flag) = 0;
										}
										else
										{
											(yyval.number) = 0;
											(yyval.flag) = 3;
										}
									 }
#line 1705 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 255 "gpp_parser.y" /* yacc.c:1646  */
    {
											if((yyvsp[-3].number) == 1)
											{
												(yyval.number) = (yyvsp[-2].number);;
												(yyval.flag) = 0;
											}
											else
											{
												(yyval.number) = (yyvsp[-1].number);
												(yyval.flag) = 0;
											}
										 }
#line 1722 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 267 "gpp_parser.y" /* yacc.c:1646  */
    {
																copy((yyval.values), (yyvsp[-1].values), z);
																(yyval.flag) = 3;
															 }
#line 1731 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 271 "gpp_parser.y" /* yacc.c:1646  */
    {
												upper_string((yyvsp[-2].string));
												strcpy(idens.iden[idens.count], (yyvsp[-2].string));
												idens.values[idens.count] = (yyvsp[-1].number);
												idens.count += 1;
												(yyval.string) = (yyvsp[-2].string);
												(yyval.flag) = 1;
										  		}
#line 1744 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 279 "gpp_parser.y" /* yacc.c:1646  */
    {
												upper_string((yyvsp[-2].string));
												(yyval.string) = (yyvsp[-2].string);
												(yyval.flag) = 1;
										  		}
#line 1754 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 284 "gpp_parser.y" /* yacc.c:1646  */
    {
														FILE* fp = fopen((yyvsp[-2].string), "r");
														(yyval.number) = 0;
														(yyval.flag) = 2;
														if(fp == NULL) printf("A file with name %s does not exist\n", (yyvsp[-2].string));
											    		else{
											    			printf(";; Loaded file %s\n", (yyvsp[-2].string));
											    			(yyval.number) = 1;
											    		}
												   }
#line 1769 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 294 "gpp_parser.y" /* yacc.c:1646  */
    {
																		upper_string((yyvsp[-5].string));
																		strcpy(idens.iden[idens.count], (yyvsp[-5].string));
																		idens.values[idens.count] = -1;
																		idens.count += 1;
																		(yyval.string) = (yyvsp[-5].string);
																		(yyval.flag) = 1;
																		}
#line 1782 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 302 "gpp_parser.y" /* yacc.c:1646  */
    {
																			upper_string((yyvsp[-5].string));
																			strcpy(idens.iden[idens.count], (yyvsp[-5].string));
																			idens.values[idens.count] = -1;
																			idens.count += 1;
																			(yyval.string) = (yyvsp[-5].string);
																			(yyval.flag) = 1;
																			}
#line 1795 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 310 "gpp_parser.y" /* yacc.c:1646  */
    {
														upper_string((yyvsp[-2].string));
														strcpy(idens.iden[idens.count], (yyvsp[-2].string));
														idens.values[idens.count] = -1;
														idens.count += 1;
														(yyval.string) = (yyvsp[-2].string);
														(yyval.flag) = 1;
													}
#line 1808 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 318 "gpp_parser.y" /* yacc.c:1646  */
    {printf("Bye.\n"); exit(1);}
#line 1814 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 319 "gpp_parser.y" /* yacc.c:1646  */
    {
										if((yyvsp[-1].flag) == 0)
										{(yyval.number) = (yyvsp[-1].number); (yyval.flag) = 0;}
										else if((yyvsp[-1].flag) == 1)
										{(yyval.string) = (yyvsp[-1].string); (yyval.flag) = 1;}
										else if((yyvsp[-1].flag) == 2)
										{(yyval.number) = (yyvsp[-1].number); (yyval.flag) = 2;}
										else if((yyvsp[-1].flag) == 3)
										{copy((yyval.values), (yyvsp[-1].values), z); (yyval.flag) = 3;}
									}
#line 1829 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 329 "gpp_parser.y" /* yacc.c:1646  */
    {
										copy((yyval.values), (yyvsp[-1].values), z); (yyval.flag) = 3;
									   }
#line 1837 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 332 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = (yyvsp[0].number); (yyval.flag) = (yyvsp[0].flag);}
#line 1843 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 335 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = (yyvsp[-2].number) && (yyvsp[-1].number); (yyval.flag) = 2;}
#line 1849 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 336 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = (yyvsp[-2].number) || (yyvsp[-1].number); (yyval.flag) = 2;}
#line 1855 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 337 "gpp_parser.y" /* yacc.c:1646  */
    {if((yyvsp[-1].number) == 0) (yyval.number) = 1; else (yyval.number) = 0; (yyval.flag) = 2;}
#line 1861 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 338 "gpp_parser.y" /* yacc.c:1646  */
    {if((yyvsp[-3].number) == (yyvsp[-2].number)) (yyval.number) = 0; else (yyval.number) = 1; (yyval.flag) = 2;}
#line 1867 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 339 "gpp_parser.y" /* yacc.c:1646  */
    {if((yyvsp[-2].number) < (yyvsp[-1].number)) (yyval.number) = 1; else (yyval.number) = 0; (yyval.flag) = 2;}
#line 1873 "y.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 340 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = (yyvsp[0].number); (yyval.flag) = 0;}
#line 1879 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 341 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = 1; (yyval.flag) = 2;}
#line 1885 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 342 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = 0; (yyval.flag) = 2;}
#line 1891 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 343 "gpp_parser.y" /* yacc.c:1646  */
    {(yyval.number) = 0; (yyval.flag) = 2;}
#line 1897 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 344 "gpp_parser.y" /* yacc.c:1646  */
    {
						upper_string((yyvsp[0].string));
						int res = string_in((yyvsp[0].string), idens.iden, idens.count);
						if(res != -1)
						{
							(yyval.number) = idens.values[res];
							(yyval.flag) = 0;
						}
						else
						{
							printf(" variable %s has no value\n", (yyvsp[0].string));
							exit(0);
						}
					 }
#line 1916 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 360 "gpp_parser.y" /* yacc.c:1646  */
    {
													int j, x = 0;
													for(j = sum - z; j < sum; j++, x++)
													{(yyvsp[-2].values)[j] = (yyvsp[-1].values)[x];}
													copy((yyval.values), (yyvsp[-2].values), sum);
													(yyval.flag) = 3;
													z = sum;
												 }
#line 1929 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 368 "gpp_parser.y" /* yacc.c:1646  */
    {
													int j, x = 0;
													for(j = sum - z; j < sum; j++, x++)
													{(yyvsp[-2].values)[j] = (yyvsp[-1].values)[x];}
													copy((yyval.values), (yyvsp[-2].values), sum);
													(yyval.flag) = 3;
													z = sum;
												  }
#line 1942 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 376 "gpp_parser.y" /* yacc.c:1646  */
    {
													int j;
													for(j = 0; j < z; j++)
													{(yyval.values)[j+1] = (yyvsp[-1].values)[j];}
													(yyval.values)[0] = (yyvsp[-2].number);
													(yyval.flag) = 3;
													z += 1;
											  }
#line 1955 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 384 "gpp_parser.y" /* yacc.c:1646  */
    {
						copy((yyval.values), (yyvsp[0].values), y);
						(yyval.flag) = 3;
						z = y;
						sum += y;
						y = 0;
					}
#line 1967 "y.tab.c" /* yacc.c:1646  */
    break;


#line 1971 "y.tab.c" /* yacc.c:1646  */
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
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
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

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

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

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
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
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
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
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 392 "gpp_parser.y" /* yacc.c:1906  */


int yyerror(char *msg) 
{ 
  printf("SYNTAX_ERROR Expression not recognized\n");
  exit(0); 
}

int yywrap(){
	return 1;
} 

int main(void) 
 {
 	printf("[1]> ");
  	yyparse(); 
 } 
