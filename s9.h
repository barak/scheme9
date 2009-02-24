/*
 * Scheme 9 from Empty Space
 * By Nils M Holm <nmh@t3x.org>, 2007,2008,2009
 */

/*
 * Tell later MSC compilers to let us use the standard CLIB API.
 * Blake McBride <blake@mcbride.name>
 */
#ifdef _MSC_VER
 #if _MSC_VER > 1200
  #ifndef _CRT_SECURE_NO_DEPRECATE
   #define _CRT_SECURE_NO_DEPRECATE
  #endif
 #endif
 #ifndef _POSIX_
  #define _POSIX_
 #endif
#endif

#ifdef plan9
 #include <u.h>
 #include <libc.h>
 #include <stdio.h>
 #include <ctype.h>
 #define NO_SIGNALS
 #define signal(sig, fn)
 #define exit(x) exits((x)? "error": NULL)
 #define ptrdiff_t int
#endif

#ifdef __unix
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __linux
 #ifndef unix
  #define unix
 #endif
 #ifndef _BSD_SOURCE
  #define _BSD_SOURCE
 #endif
 #ifndef _POSIX_SOURCE
  #define _POSIX_SOURCE
 #endif
#endif

#ifdef unix
 #include <stdlib.h>
 #include <stddef.h>
 #include <stdio.h>
 #include <string.h>
 #include <ctype.h>
 #ifdef NO_SIGNALS
  #define signal(sig, fn)
 #else
  #include <signal.h>
  #ifndef SIGQUIT
   #define SIGQUIT SIGINT
  #endif
 #endif
#endif

#ifndef DEFAULT_LIBRARY_PATH
 #define DEFAULT_LIBRARY_PATH	".:~/.s9fes:/usr/local/share/s9fes"
#endif

#ifndef IMAGEFILE
 #define IMAGEFILE	"s9.image"
#endif
#ifndef LIBRARY
 #define LIBRARY	"s9.scm"
#endif

#define TOKEN_LENGTH		1024
#define MAX_PORTS		32
#define INITIAL_SEGMENT_SIZE	32768
#define HASH_THRESHOLD		16
#define MAX_CALL_TRACE		10

/* Default memory limit in K-Nodes, 0 = none
 *
 * This peculiar default value is based on the way
 * in which segment sizes grow. Each time a new
 * memory segment is added, the segment size grows
 * by a factor of 3/2, so the allocation sequence
 * results in a pool size of 12689048 nodes when
 * growing the pool for the 12'th time.
 */
#define DEFAULT_LIMIT_KN	12392

/* A "cell" must be large enough to hold a pointer */
#define cell	ptrdiff_t
#define uint	unsigned int

/* Pick one ... */
/* #define BITS_PER_WORD_64 */
/* #define BITS_PER_WORD_32 */
/* #define BITS_PER_WORD_16 */

/* ... else assume a reasonable default */
#ifndef BITS_PER_WORD_16
 #ifndef BITS_PER_WORD_32
  #ifndef BITS_PER_WORD_64
   #define BITS_PER_WORD_32
  #endif
 #endif
#endif

/* N-bit arithmetics require sizeof(cell) >= N/8 */
#ifdef BITS_PER_WORD_64
 #define DIGITS_PER_WORD	18
 #define INT_SEG_LIMIT		1000000000000000000
#else
 #ifdef BITS_PER_WORD_32
  #define DIGITS_PER_WORD	9
  #define INT_SEG_LIMIT		1000000000
 #else
  #ifdef BITS_PER_WORD_16
   #define DIGITS_PER_WORD	4
   #define INT_SEG_LIMIT	10000
  #else
   #error "BITS_PER_WORD_* undefined (this should not happen)"
  #endif
 #endif
#endif

/* GC tags */
#define	ATOM_TAG	0x01	/* Atom, Car = type, CDR = next */
#define	MARK_TAG	0x02	/* Mark */
#define STATE_TAG	0x04	/* State */
#define VECTOR_TAG	0x08	/* Vector, Car = type, CDR = content */
#define PORT_TAG	0x10	/* Atom is an I/O port (with ATOM_TAG) */
#define USED_TAG	0x20	/* Port: used flag */
#define LOCK_TAG	0x40	/* Port: locked (do not close) */

enum EVAL_STATES {
	EV_ATOM,	/* Processing atom */
	EV_ARGS,	/* Processing argument list */
	EV_BETA,	/* Beta-reducing */
	EV_IF_PRED,	/* Processing predicate of IF */
	EV_SET_VAL,	/* Processing value of SET! */
	EV_DEFINE,	/* Processing value of DEFINE */
	EV_MACRO,	/* Processing value of DEFINE-MACRO */
	EV_BEGIN,	/* Processing BEGIN */
	EV_AND,		/* Processing arguments of AND */
	EV_OR,	 	/* Processing arguments of OR */
	EV_COND		/* Processing clauses of COND */
};

enum TYPES {
	T_NONE,
	T_BOOLEAN,
	T_CHAR,
	T_INPUT_PORT,
	T_INTEGER,
	T_OUTPUT_PORT,
	T_PAIR,
	T_PAIR_OR_NIL,
	T_PROCEDURE,
	T_STRING,
	T_SYMBOL,
	T_VECTOR
};

/*
 * Short cuts for primitive procedure definitions
 */
#define CHR T_CHAR
#define INP T_INPUT_PORT
#define INT T_INTEGER
#define LST T_PAIR_OR_NIL
#define OUP T_OUTPUT_PORT
#define PAI T_PAIR
#define PRC T_PROCEDURE
#define STR T_STRING
#define SYM T_SYMBOL
#define VEC T_VECTOR
#define ___ T_NONE

/*
 * Special objects
 */
#define special_value_p(x)	((x) < 0)
#define NIL			(-1)
#define TRUE			(-2)
#define FALSE			(-3)
#define ENDOFFILE		(-4)
#define UNDEFINED		(-5)
#define UNSPECIFIC		(-6)
#define	DOT			(-7)
#define	RPAREN			(-8)
#define NOEXPR			(-9)

struct Primitive_procedure {
	char	*name;
	cell	(*handler)(cell expr);
	int	min_args;
	int	max_args;	/* -1 = variadic */
	int	arg_types[3];
};

#define PRIM	struct Primitive_procedure

/*
 * Globals
 */
EXTERN int	Segment_size;
EXTERN int	Pool_size,
		Vpool_size;
EXTERN cell	*Car,
		*Cdr;
EXTERN char	*Tag;
extern cell	New;
EXTERN cell	Free_list;
EXTERN cell	*Vectors;
EXTERN cell	Free_vecs;
EXTERN long	Memory_limit_kn;
EXTERN cell	Stack,
		Stack_bottom;
EXTERN cell	State_stack;
EXTERN cell	Tmp_car,
		Tmp_cdr,
		Tmp;
EXTERN cell	Symbols;
EXTERN cell	Program;
EXTERN cell	Called_procedures[10];
EXTERN int	Proc_ptr;
EXTERN cell	Environment;
EXTERN cell	Acc;
EXTERN FILE	*Ports[MAX_PORTS];
EXTERN char	Port_flags[MAX_PORTS];
EXTERN int	Input_port,
		Output_port;
EXTERN int	Level;
EXTERN int	Load_level;
EXTERN int	Displaying;
EXTERN int	Quiet_mode;
EXTERN char	**Command_line;

EXTERN volatile int	Error_flag;

/*
 * Short cuts for accessing predefined symbols
 */
EXTERN cell	S_arrow, S_char, S_else, S_extensions, S_input_port,
		S_integer, S_latest, S_library_path, S_loading,
		S_output_port, S_primitive, S_procedure, S_quasiquote,
		S_quote, S_string, S_symbol, S_syntax, S_unquote,
		S_unquote_splicing, S_vector;
EXTERN cell	S_and, S_begin, S_cond, S_define, S_define_macro, S_if,
		S_lambda, S_or, S_set_b;

/*
 * I/O
 */
#define nl()		pr("\n")
#define reject(c)	ungetc(c, Ports[Input_port])

#define read_c()	getc(Ports[Input_port])
#define read_c_ci()	tolower(read_c())

/*
 * Access to values of atoms
 */
#define string(n)	((char *) &Vectors[Cdr[n]])
#define string_len(n)	(Vectors[Cdr[n] - 1])
#define vector_size(k)	(((k) + sizeof(cell)-1) / sizeof(cell) + 2)
#define vector(n)	(&Vectors[Cdr[n]])
#define vector_len(n)	(vector_size(string_len(n)) - 2)
#define port_no(n)	(cadr(n))
#define char_value(n)	(cadr(n))

/*
 * Nested lists
 */
#define car(x)		(Car[x])
#define cdr(x)		(Cdr[x])
#define caar(x)		(Car[Car[x]])
#define cadr(x)		(Car[Cdr[x]])
#define cdar(x)		(Cdr[Car[x]])
#define cddr(x)		(Cdr[Cdr[x]])
#define caaar(x)	(Car[Car[Car[x]]])
#define caadr(x)	(Car[Car[Cdr[x]]])
#define cadar(x)	(Car[Cdr[Car[x]]])
#define caddr(x)	(Car[Cdr[Cdr[x]]])
#define cdadr(x)	(Cdr[Car[Cdr[x]]])
#define cddar(x)	(Cdr[Cdr[Car[x]]])
#define cdddr(x)	(Cdr[Cdr[Cdr[x]]])
#define caaddr(x)	(Car[Car[Cdr[Cdr[x]]]])
#define caddar(x)	(Car[Cdr[Cdr[Car[x]]]])
#define cadadr(x)	(Car[Cdr[Car[Cdr[x]]]])
#define cadddr(x)	(Car[Cdr[Cdr[Cdr[x]]]])
#define cdddar(x)	(Cdr[Cdr[Cdr[Car[x]]]])
#define cddddr(x)	(Cdr[Cdr[Cdr[Cdr[x]]]])

/*
 * Type predicates
 */
#define null_p(n)	((n) == NIL)
#define eof_p(n)	((n) == ENDOFFILE)
#define undefined_p(n)	((n) == UNDEFINED)
#define unspecific_p(n)	((n) == UNSPECIFIC)

#define boolean_p(n)	((n) == TRUE || (n) == FALSE)

#define integer_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == S_integer)
#define primitive_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == S_primitive)
#define procedure_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == S_procedure)
#define special_p(n)	((n) == S_and    || \
			 (n) == S_begin  || \
			 (n) == S_cond   || \
			 (n) == S_define || \
			 (n) == S_define_macro || \
			 (n) == S_if     || \
			 (n) == S_lambda || \
			 (n) == S_or     || \
			 (n) == S_quote  || \
			 (n) == S_set_b)
#define char_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == S_char)
#define syntax_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == S_syntax)
#define input_port_p(n)	\
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && (Tag[n] & PORT_TAG) \
	 && Car[n] == S_input_port)
#define output_port_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && (Tag[n] & PORT_TAG) \
	 && Car[n] == S_output_port)

#define symbol_p(n) \
	(!special_value_p(n) && (Tag[n] & VECTOR_TAG) && Car[n] == S_symbol)
#define vector_p(n) \
	(!special_value_p(n) && (Tag[n] & VECTOR_TAG) && Car[n] == S_vector)
#define string_p(n) \
	(!special_value_p(n) && (Tag[n] & VECTOR_TAG) && Car[n] == S_string)

#define auto_quoting_p(n) \
	(boolean_p(n)    || \
	 char_p(n)       || \
	 eof_p(n)        || \
	 integer_p(n)    || \
	 string_p(n)     || \
	 undefined_p(n)  || \
	 unspecific_p(n) || \
	 procedure_p(n)  || \
	 primitive_p(n)  || \
	 vector_p(n))

#define atom_p(n) \
	(special_value_p(n) || (Tag[n] & ATOM_TAG) || (Tag[n] & VECTOR_TAG))

#define pair_p(x) (!atom_p(x))

/*
 * Rib structure
 */
#define rib_args(x)	(car(x))
#define rib_append(x)	(cadr(x))
#define rib_result(x)	(caddr(x))
#define rib_source(x)	(cadddr(x))

/*
 * Allocators
 */
#define alloc(pa, pd)	alloc3((pa), (pd), 0)

#define save(n)		(Stack = alloc((n), Stack))
#define save_state(v)	(State_stack = alloc3((v), State_stack, ATOM_TAG))

/*
 * Bignum arithmitcs
 */
#define bignum_negative_p(a) ((cadr(a)) < 0)
#define bignum_zero_p(a) ((cadr(a) == 0) && (cddr(a)) == NIL)

/*
 * Prototypes
 */
cell error(char *msg, cell expr);
cell make_integer(int i);
int integer_value(char *src, cell x);
cell alloc3(cell pcar, cell pcdr, int ptag);
cell make_string(char *s, int k);
cell unsave(int k);
cell add_symbol(char *s);
int alloc_port(void);
cell make_port(int portno, cell type);
void add_primitives(char *name, PRIM *p);
