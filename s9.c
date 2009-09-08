/*
 * Scheme 9 from Empty Space
 * By Nils M Holm <nmh@t3x.org>, 2007,2008,2009
 */

/*
 * Use -DNO_SIGNALS to disable POSIX signal handlers.
 * Use -DBITS_PER_WORD_64 on 64-bit systems.
 */

#define VERSION "2009-09-06"

#define EXTERN
#include "s9.h"
#undef EXTERN

#ifndef EXTENSIONS
 #define EXTENSIONS
#endif

int	Verbose_GC = 0;
cell	Debug;

cell	*GC_root[] = { &Program, &Symbols, &Environment, &Tmp,
			&Tmp_car, &Tmp_cdr, &Stack, &Stack_bottom,
			&State_stack, &Acc, &Trace_list, &File_list,
			&Debug, NULL };

/*----- Counter -----*/

int	Run_stats;

struct counter {
	int	n, n1k, n1m, n1g;
};

struct counter  Reductions,
		Allocations,
		Collections;

void reset_counter(struct counter *c) {
	c->n = 0;
	c->n1k = 0;
	c->n1m = 0;
	c->n1g = 0;
}

void count(struct counter *c) {
	c->n++;
	if (c->n >= 1000) {
		c->n -= 1000;
		c->n1k++;
		if (c->n1k >= 1000) {
			c->n1k -= 1000;
			c->n1m++;
			if (c->n1m >= 1000) {
				c->n1m -= 1000;
				c->n1g++;
				if (c->n1g >= 1000) {
					error("statistics counter overflow",
						NOEXPR);
				}
			}
		}
	}
}

char *counter_to_string(struct counter *c) {
	static char	buf[16];

	if (c->n1g != 0) {
		sprintf(buf, "%3d,%03d,%03d,%03d",
				c->n1g, c->n1m, c->n1k, c->n);
	}
	else if (c->n1m != 0) {
		sprintf(buf, "%7d,%03d,%03d", c->n1m, c->n1k, c->n);
	}
	else if (c->n1k != 0) {
		sprintf(buf, "%11d,%03d", c->n1k, c->n);
	}
	else {
		sprintf(buf, "%15d", c->n);
	}
	return buf;
}

/*----- Output -----*/

cell error(char *msg, cell expr);

void flush(void) {
	fflush(Ports[Output_port]);
}

void pr_raw(char *s, int k) {
	fwrite(s, 1, k, Ports[Output_port]);
	if (Output_port == 1 && s[k-1] == '\n') flush();
}

void pr(char *s) {
	if (Ports[Output_port] == NULL)
		error("output port is not open", NOEXPR);
	else
		pr_raw(s, strlen(s));
}

/*----- Error Handler -----*/

void print_form(cell n);

void print_calltrace(void) {
	int	i, j;

	for (i=0; i<MAX_CALL_TRACE; i++)
		if (Called_procedures[i] != NIL)
			break;
	if (i == MAX_CALL_TRACE) return;
	pr("call trace:");
	i = Proc_ptr;
	for (j=0; j<MAX_CALL_TRACE; j++) {
		if (i >= MAX_CALL_TRACE) i = 0;
		if (Called_procedures[i] != NIL) {
			pr(" ");
			print_form(Called_procedures[i]);
		}
		i++;
	}
	nl();
}

cell error(char *msg, cell expr) {
	int	oport;
	char	buf[100];

	if (Error_flag) return UNSPECIFIC;
	oport = Output_port;
	Output_port = 1;
	Error_flag = 1;
	pr("error: ");
	if (car(S_loading) == TRUE) {
		print_form(car(File_list));
		sprintf(buf, ": %d: ", Line_no);
		pr(buf);
	}
	pr(msg);
	if (expr != NOEXPR) {
		pr(": ");
		print_form(expr);
	}
	nl();
	print_calltrace();
	Output_port = oport;
	if (Quiet_mode) exit(1);
	return UNSPECIFIC;
}

void fatal(char *msg) {
	pr("fatal ");
	Error_flag = 0;
	error(msg, NOEXPR);
	exit(2);
}

/*----- Allocator -----*/

void new_segment(void) {
	Car = realloc(Car, sizeof(cell) * (Pool_size + Segment_size));
	Cdr = realloc(Cdr, sizeof(cell) * (Pool_size + Segment_size));
	Tag = realloc(Tag, Pool_size + Segment_size);
	Vectors = realloc(Vectors, sizeof(cell) * (Vpool_size + Segment_size));
	if (Car == NULL || Cdr == NULL || Tag == NULL || Vectors == NULL) {
		fatal("out of physical memory");
	}
	memset(&car(Pool_size), 0, Segment_size * sizeof(cell));
	memset(&cdr(Pool_size), 0, Segment_size * sizeof(cell));
	memset(&Tag[Pool_size], 0, Segment_size);
	memset(&Vectors[Vpool_size], 0, Segment_size * sizeof(cell));
	Pool_size += Segment_size;
	Vpool_size += Segment_size;
	Segment_size = Segment_size * 3 / 2;
}

/*
 * Mark nodes which can be accessed through N.
 * Using the Deutsch/Schorr/Waite (aka pointer reversal) algorithm.
 * S0: M==0 S==0 unvisited, process CAR (vectors: process 1st slot)
 * S1: M==1 S==1 CAR visited, process CDR (vectors: process next slot)
 * S2: M==1 S==0 completely visited, return to parent
 */
void mark(cell n) {
	cell	p, parent, *v;
	int	i;

	parent = NIL;	/* Initially, there is no parent node */
	while (1) {
		if (special_value_p(n) || Tag[n] & MARK_TAG) {
			if (parent == NIL) break;
			if (Tag[parent] & VECTOR_TAG) {	/* S1 --> S1|done */
				i = vector_index(parent);
				v = vector(parent);
				if (Tag[parent] & STATE_TAG &&
				    i+1 < vector_len(parent)
				) {			/* S1 --> S1 */
					p = v[i+1];
					v[i+1] = v[i];
					v[i] = n;
					n = p;
					vector_index(parent) = i+1;
				}
				else {			/* S1 --> done */
					p = parent;
					parent = v[i];
					v[i] = n;
					n = p;
				}
			}
			else if (Tag[parent] & STATE_TAG) {	/* S1 --> S2 */
				p = cdr(parent);
				cdr(parent) = car(parent);
				car(parent) = n;
				Tag[parent] &= ~STATE_TAG;
				Tag[parent] |=  MARK_TAG;
				n = p;
			}
			else {				/* S2 --> done */
				p = parent;
				parent = cdr(p);
				cdr(p) = n;
				n = p;
			}
		}
		else {
			if (Tag[n] & VECTOR_TAG) {	/* S0 --> S1|S2 */
				Tag[n] |= MARK_TAG;
				/* Tag[n] &= ~STATE_TAG; */
				vector_link(n) = n;
				if (car(n) == T_VECTOR && vector_len(n) != 0) {
					Tag[n] |= STATE_TAG;
					vector_index(n) = 0;
					v = vector(n);
					p = v[0];
					v[0] = parent;
					parent = n;
					n = p;
				}
			}
			else if (Tag[n] & ATOM_TAG) {	/* S0 --> S2 */
				if (input_port_p(n) || output_port_p(n))
					Port_flags[port_no(n)] |= USED_TAG;
				p = cdr(n);
				cdr(n) = parent;
				/*Tag[n] &= ~STATE_TAG;*/
				parent = n;
				n = p;
				Tag[parent] |= MARK_TAG;
			}
			else {				/* S0 --> S1 */
				p = car(n);
				car(n) = parent;
				Tag[n] |= MARK_TAG;
				parent = n;
				n = p;
				Tag[parent] |= STATE_TAG;
			}
		}
	}
}

/* Mark all vectors unused */
void unmark_vectors(void) {
	int	p, k, link;

	p = 0;
	while (p < Free_vecs) {
		link = p;
		k = Vectors[p + RAW_VECTOR_SIZE];
		p += vector_size(k);
		Vectors[link] = NIL;
	}
}

/* Mark and sweep GC. */
int gc(void) {
	int	i, k;
	char	buf[100];

	if (Run_stats) count(&Collections);
	for (i=0; i<MAX_PORTS; i++)
		if (Port_flags[i] & LOCK_TAG)
			Port_flags[i] |= USED_TAG;
		else
			Port_flags[i] &= ~USED_TAG;
	for (i=0; GC_root[i] != NULL; i++) mark(GC_root[i][0]);
	k = 0;
	Free_list = NIL;
	for (i=0; i<Pool_size; i++) {
		if (!(Tag[i] & MARK_TAG)) {
			cdr(i) = Free_list;
			Free_list = i;
			k++;
		}
		else {
			Tag[i] &= ~MARK_TAG;
		}
	}
	for (i=0; i<MAX_PORTS; i++) {
		if (!(Port_flags[i] & USED_TAG) && Ports[i] != NULL) {
			fclose(Ports[i]);
			Ports[i] = NULL;
		}
	}
	if (Verbose_GC > 1) {
		sprintf(buf, "GC: %d nodes reclaimed", k);
		pr(buf); nl();
	}
	return k;
}

/* Allocate a fresh node and initialize with PCAR,PCDR,PTAG. */
cell alloc3(cell pcar, cell pcdr, int ptag) {
	cell	n;
	int	k;
	char	buf[100];

	if (Run_stats) count(&Allocations);
	if (Free_list == NIL) {
		if (ptag == 0) Tmp_car = pcar;
		Tmp_cdr = pcdr;
		k = gc();
		/*
		 * Performance increases dramatically if we
		 * do not wait for the pool to run dry.
		 * In fact, don't even let it come close to that.
		 */
		if (k < Pool_size / 2) {
			if (	Memory_limit_kn &&
				Pool_size + Segment_size > Memory_limit_kn
			) {
				error("hit memory limit", NOEXPR);
			}
			else {
				new_segment();
				if (Verbose_GC) {
					sprintf(buf,
						"GC: new segment,"
						 " nodes = %d,"
						 " next segment = %d",
						Pool_size,
						Segment_size);
					pr(buf); nl();
				}
				gc();
			}
		}
		Tmp_car = Tmp_cdr = NIL;
	}
	if (Free_list == NIL) {
		fatal("alloc3: failed to recover from low memory condition");
	}
	n = Free_list;
	Free_list = cdr(Free_list);
	car(n) = pcar;
	cdr(n) = pcdr;
	Tag[n] = ptag;
	return n;
}

/* In situ vector pool garbage collection and compaction */
int gcv(void) {
	int	v, k, to, from;
	char	buf[100];

	unmark_vectors();
	gc();		/* re-mark live vectors */
	to = from = 0;
	while (from < Free_vecs) {
		v = Vectors[from + RAW_VECTOR_SIZE];
		k = vector_size(v);
		if (Vectors[from + RAW_VECTOR_LINK] != NIL) {
			if (to != from) {
				memmove(&Vectors[to], &Vectors[from],
					k * sizeof(cell));
				cdr(Vectors[to]) = to + RAW_VECTOR_DATA;
			}
			to += k;
		}
		from += k;
	}
	k = Free_vecs - to;
	if (Verbose_GC > 1) {
		sprintf(buf, "gcv: %d cells reclaimed", k);
		pr(buf); nl();
	}
	Free_vecs = to;
	return k;
}

/* Allocate vector from pool */
cell allocv(cell type, int size) {
	cell	n;
	int	v, wsize, k;
	char	buf[100];

	wsize = vector_size(size);
	if (Free_vecs + wsize >= Vpool_size) {
		k = gcv();
		if (	Free_vecs + wsize >=
			Vpool_size - Vpool_size / 2
		) {
			if (	Memory_limit_kn &&
				Pool_size + Segment_size > Memory_limit_kn
			) {
				error("hit memory limit", NOEXPR);
			}
			else {
				new_segment();
				gcv();
				if (Verbose_GC) {
					sprintf(buf,
						"allocv: new segment,"
						 " nodes = %d",
						Vpool_size);
					pr(buf); nl();
				}
			}
		}
	}
	if (Free_vecs + wsize >= Vpool_size) {
		fatal("allocv: failed to recover from low memory condition");
	}
	v = Free_vecs;
	Free_vecs += wsize;
	n = alloc3(type, v + RAW_VECTOR_DATA, VECTOR_TAG);
	Vectors[v + RAW_VECTOR_LINK] = n;
	Vectors[v + RAW_VECTOR_INDEX] = 0;
	Vectors[v + RAW_VECTOR_SIZE] = size;
	return n;
}

/* Pop K nodes off the Stack, return last one. */
cell unsave(int k) {
	cell	n = NIL; /*LINT*/

	while (k) {
		if (Stack == NIL) fatal("unsave: stack underflow");
		n = car(Stack);
		Stack = cdr(Stack);
		k--;
	}
	return n;
}

/*----- Reader -----*/

cell find_symbol(char *s) {
	cell	y;

	y = Symbols;
	while (y != NIL) {
		if (!strcmp(symbol_name(car(y)), s))
			return car(y);
		y = cdr(y);
	}
	return NIL;
}

cell make_symbol(char *s, int k) {
	cell	n;

	n = allocv(T_SYMBOL, k+1);
	strcpy(symbol_name(n), s);
	return n;
}

cell add_symbol(char *s) {
	cell	y, new;

	y = find_symbol(s);
	if (y != NIL) return y;
	new = make_symbol(s, (int) strlen(s));
	Symbols = alloc(new, Symbols);
	return car(Symbols);
}

cell read_form(int flags);

cell read_list(int flags) {
	cell	n,	/* Node read */
		m,	/* List */
		a;	/* Used to append nodes to m */
	int	c;	/* Member counter */
	cell	new;
	char	badpair[] = "malformed pair";

	Level++;
	m = alloc3(NIL, NIL, flags);	/* root */
	save(m);
	a = NIL;
	c = 0;
	while (1) {
		if (Error_flag) {
			unsave(1);
			return NIL;
		}
		n = read_form(flags);
		if (n == ENDOFFILE)  {
			if (Load_level) {
				unsave(1);
				return ENDOFFILE;
			}
			error("missing ')'", NOEXPR);
		}
		if (n == DOT) {
			if (c < 1) {
				error(badpair, NOEXPR);
				continue;
			}
			n = read_form(flags);
			cdr(a) = n;
			if (n == RPAREN || read_form(flags) != RPAREN) {
				error(badpair, NOEXPR);
				continue;
			}
			unsave(1);
			Level--;
			return m;
		}
		if (n == RPAREN) break;
		if (a == NIL)
			a = m;		/* First member: insert at root */
		else
			a = cdr(a);	/* Following members: append */
		car(a) = n;
		new = alloc3(NIL, NIL, flags); /* Space for next member */
		cdr(a) = new;
		c++;
	}
	Level--;
	if (a != NIL) cdr(a) = NIL;	/* Remove trailing empty node */
	unsave(1);
	return c? m: NIL;
}

cell quote(cell n, cell quotation) {
	cell	q;

	q = alloc(n, NIL);
	return alloc(quotation, q);
}

#define exponent_char_p(c) \
	(c == 'd' || c == 'D' || \
	 c == 'e' || c == 'E' || \
	 c == 'f' || c == 'F' || \
	 c == 'l' || c == 'L' || \
	 c == 's' || c == 'S')

int string_numeric_p(char *s) {
	int	i;
	int	got_d, got_e, got_dp, got_s;

	i = 0;
	got_s = 0;
	got_d = 0;
	got_dp = 0;
	got_e = 0;
	if (s[0] == '+' || s[0] == '-') {
		i = 1;
		got_s = 1;
	}
	if (!s[i]) return 0;
	while (s[i]) {
		if (exponent_char_p(s[i]) && got_d && !got_e) {
			if (isdigit(s[i+1]) || s[i+1] == '#') {
				got_e = 1;
			}
			else if ((s[i+1] == '+' || s[i+1] == '-') &&
				(isdigit(s[i+2]) || s[i+2] == '#')
			) {
				got_e = 1;
				i++;
			}
			else {
				return 0;
			}
		}
		else if (s[i] == '.' && !got_dp) {
			got_dp = 1;
		}
		else if (s[i] == '#' && (got_d || got_dp || got_s)) {
			got_d = 1;
		}
		else if (isdigit(s[i])) {
			got_d = 1;
		}
		else {
			return 0;
		}
		i++;
	}
	return 1;
}

char *copy_string(char *s) {
	char	*new;

	new = malloc(strlen(s)+1);
	if (s == NULL) fatal("copy_string(): out of memory");
	strcpy(new, s);
	return new;
}

char* translate(char *s, int old_c, int new_c) {
	char	*new;

	new = copy_string(s);
	s = new;
	while (*s) {
		if (*s == old_c) *s = new_c;
		s++;
	}
	return new;
}

cell make_real(int flags, cell exp, cell mant);
cell real_normalize(cell x, char *who);

cell string_to_bignum(char *numstr, int force_exact) {
	cell	n, v;
	int	k, j, sign, exact;
	char	*s, *buf;

	buf = translate(numstr, '#', '5');
	exact = strchr(numstr, '#') == NULL;
	s = buf;
	sign = 1;
	if (s[0] == '-') {
		s++;
		sign = -1;
	}
	else if (s[0] == '+') {
		s++;
	}
	k = (int) strlen(s);
	n = NIL;
	while (k) {
		j = k <= DIGITS_PER_WORD? k: DIGITS_PER_WORD;
		v = atol(&s[k-j]);
		s[k-j] = 0;
		k -= j;
		if (k == 0) v *= sign;
		n = alloc_atom(v, n);
	}
	if (exact || force_exact)
		return alloc_atom(T_INTEGER, n);
	car(n) = labs(car(n));
	n = make_real((sign<0? REAL_NEGATIVE: 0) | REAL_INEXACT, 0, n);
	return real_normalize(n, NULL);
}

cell make_integer(cell i);
cell bignum_shift_left(cell a, int fill);
cell bignum_add(cell a, cell b);

cell string_to_real(char *s) {
	cell	mantissa, n;
	cell	exponent;
	int	found_dp;
	int	neg = 0;
	int	i, j, v;
	int	exact;

	mantissa = make_integer(0);
	save(mantissa);
	exponent = 0;
	i = 0;
	if (s[i] == '+') {
		i++;
	}
	else if (s[i] == '-') {
		neg = 1;
		i++;
	}
	found_dp = 0;
	exact = 1;
	while (isdigit(s[i]) || s[i] == '#' || s[i] == '.') {
		if (s[i] == '.') {
			i++;
			found_dp = 1;
			continue;
		}
		if (found_dp) exponent--;
		mantissa = bignum_shift_left(mantissa, 0);
		car(Stack) = mantissa;
		if (s[i] == '#') {
			exact = 0;
			v = 5;
		}
		else {
			v = s[i]-'0';
		}
		mantissa = bignum_add(mantissa, make_integer(v));
		car(Stack) = mantissa;
		i++;
	}
	j = 0;
	for (n = cdr(mantissa); n != NIL; n = cdr(n))
		j++;
	if (exponent_char_p(s[i])) {
		i++;
		n = string_to_bignum(&s[i], 1);
		if (cddr(n) != NIL) {
			unsave(1);
			return error(
				"exponent too big in real number literal",
				make_string(s, strlen(s)));
		}
		exponent += integer_value("", n);
	}
	unsave(1);
	n = make_real((neg? REAL_NEGATIVE: 0) | (exact? 0: REAL_INEXACT),
			exponent, cdr(mantissa));
	return real_normalize(n, NULL);
}

cell string_to_number(char *s) {
	int	i;

	for (i=0; s[i]; i++) {
		if (s[i] == '.' || exponent_char_p(s[i]))
			return string_to_real(s);
	}
	return string_to_bignum(s, 0);
}

/* Create a character literal. */
cell make_char(int x) {
	cell n;

	n = alloc_atom(x, NIL);
	return alloc_atom(T_CHAR, n);
}

int strcmp_ci(char *s1, char *s2) {
	int	c1, c2;

	while (1) {
		c1 = tolower(*s1++);
		c2 = tolower(*s2++);
		if (!c1 || !c2 || c1 != c2) break;
	}
	return c1<c2? -1: c1>c2? 1: 0;
}

/* Read a character literal. */
cell character(void) {
	char	buf[10], msg[50];
	int	i, c = 0; /*LINT*/

	for (i=0; i<sizeof(buf)-1; i++) {
		c = read_c();
		if (i > 0 && !isalpha(c)) break;
		buf[i] = c;
	}
	reject(c);
	buf[i] = 0;
	if (i == 0) c = ' ';
	else if (i == 1) c = buf[0];
	else if (!strcmp_ci(buf, "space")) c = ' ';
	else if (!strcmp_ci(buf, "newline")) c = '\n';
	else {
		sprintf(msg, "unknown character: #\\%s", buf);
		error(msg, NOEXPR);
		c = 0;
	}
	return make_char(c);
}

/* Create a string; K = length */
cell make_string(char *s, int k) {
	cell	n;

	n = allocv(T_STRING, k+1);
	memcpy(string(n), s, k+1);
	return n;
}

/* Read a string literal. */
cell string_literal(void) {
	char	s[TOKEN_LENGTH+1];
	cell	n;
	int	c, i, q;
	int	inv;

	i = 0;
	q = 0;
	c = read_c();
	inv = 0;
	while (q || c != '"') {
		if (c == EOF)
			error("missing '\"' in string literal", NOEXPR);
		if (Error_flag) break;
		if (i >= TOKEN_LENGTH-2) {
			error("string literal too long", NOEXPR);
			i--;
		}
		if (q && c != '"' && c != '\\') {
			s[i++] = '\\';
			inv = 1;
		}
		s[i] = c;
		q = !q && c == '\\';
		if (!q) i++;
		c = read_c();
	}
	s[i] = 0;
	n = make_string(s, i);
	Tag[n] |= CONST_TAG;
	if (inv) error("invalid escape sequence in string", n);
	return n;
}

/* Report unreadable object */
cell unreadable(void) {
	int	c, i;
	char	buf[TOKEN_LENGTH];
	int	d;

	strcpy(buf, "#<");
	i = 2;
	while (1) {
		c = read_c_ci();
		if (c == '>' || c == '\n') {
			if (c == '\n') Line_no++;
			break;
		}
		if (i < TOKEN_LENGTH-2) buf[i++] = c;
	}
	buf[i++] = '>';
	buf[i] = 0;
	d = Displaying;
	Displaying = 1;
	error("unreadable object", make_string(buf, i));
	Displaying = d;
	return NIL;
}

#define separator(c) \
	((c) == ' '  || (c) == '\t' || (c) == '\n' || \
	 (c) == '\r' || (c) == '('  || (c) == ')'  || \
	 (c) == ';'  || (c) == '\'' || (c) == '`'  || \
	 (c) == ','  || (c) == '"'  || (c) == EOF)

cell symbol_or_number(int c) {
	char	s[TOKEN_LENGTH];
	int	i;

	i = 0;
	while (!separator(c)) {
		if (i >= TOKEN_LENGTH-2) {
			error("symbol too long", NOEXPR);
			i--;
		}
		s[i] = c;
		i++;
		c = read_c_ci();
	}
	s[i] = 0;
	reject(c);
	if (string_numeric_p(s)) return string_to_number(s);
	return add_symbol(s);
}

cell list_to_vector(cell m, char *msg, int flags) {
	cell	n, vec;
	int	k;
	cell	*p;

	k = 0;
	for (n = m; n != NIL; n = cdr(n)) {
		if (atom_p(n)) return error(msg, m);
		k++;
	}
	vec = allocv(T_VECTOR, k*sizeof(cell));
	Tag[vec] |= flags;
	p = vector(vec);
	for (n = m; n != NIL; n = cdr(n)) {
		*p = car(n);
		p++;
	}
	return vec;
}

cell read_vector(void) {
	cell	n;

	n = read_list(CONST_TAG);
	save(n);
	n = list_to_vector(n, "invalid vector syntax", CONST_TAG);
	Tag[n] |= CONST_TAG;
	unsave(1);
	return n;
}

cell bignum_read(char *pre, int radix);
cell bignum_abs(cell a);

cell read_number(int inexact) {
	cell	n, m;
	int	flags;
	char	buf[50];

	n = read_form(0);
	if (integer_p(n)) {
		if (!inexact) return n;
		flags = bignum_negative_p(n)? REAL_NEGATIVE: 0;
		m = bignum_abs(n);
		n = make_real(flags, 0, cdr(m));
		n = real_normalize(n, "numeric literal");
	}
	if (!real_p(n)) {
		sprintf(buf, "number expected after #%c, got",
			inexact? 'i': 'e');
		return error(buf, n);
	}
	if (inexact) real_flags(n) |= REAL_INEXACT;
	return n;
}

cell read_toplevel_form(int flags) {
	int	ll = Level;
	cell	n;

	Level = 0;
	n = read_form(flags);
	Level = ll;
	return n;
}

cell read_form(int flags) {
	char	buf[50];
	int	c, c2;

	c = read_c_ci();
	while (1) {	/* Skip spaces and comments */
		while (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
			if (c == '\n') Line_no++;
			if (Error_flag) return NIL;
			c = read_c_ci();
		}
		if (c == '#') {
			c = read_c_ci();
			if (c != '!') {
				reject(c);
				c = '#';
				break;
			}
		}
		else if (c != ';')
			break;
		while (c != '\n' && c != EOF) c = read_c_ci();
	}
	if (c == EOF) return ENDOFFILE;
	if (Error_flag) return NIL;
	if (c == '(') {
		return read_list(flags);
	}
	else if (c == '\'') {
		return quote(read_toplevel_form(CONST_TAG), S_quote);
	}
	else if (c == '`') {
		return quote(read_toplevel_form(CONST_TAG), S_quasiquote);
	}
	else if (c == ',') {
		c = read_c_ci();
		if (c == '@') {
			return quote(read_toplevel_form(0),
					S_unquote_splicing);
		}
		else {
			reject(c);
			return quote(read_toplevel_form(0), S_unquote);
		}
	}
	else if (c == '#') {
		c = read_c_ci();
		switch (c) {
		case 'f':	return FALSE;
		case 't':	return TRUE;
		case '\\':	return character();
		case '(':	return read_vector();
		case 'b':	return bignum_read("#b", 2);
		case 'd':	return bignum_read("#d", 10);
		case 'o':	return bignum_read("#o", 8);
		case 'x':	return bignum_read("#x", 16);
		case 'e':	return read_number(0);
		case 'i':	return read_number(1);
		case '<':	return unreadable();
		default:	sprintf(buf, "unknown # syntax: #%c", c);
				return error(buf, NOEXPR);
		}
	}
	else if (c == '"') {
		return string_literal();
	}
	else if (c == ')') {
		if (!Level) return error("unexpected ')'", NOEXPR);
		return RPAREN;
	}
	else if (c == '.') {
		c2 = read_c_ci();
		reject(c2);
		if (separator(c2)) {
			if (!Level) return error("unexpected '.'", NOEXPR);
			return DOT;
		}
		return symbol_or_number(c);
	}
	else {
		return symbol_or_number(c);
	}
}

cell xread(void) {
	if (Ports[Input_port] == NULL)
		return error("input port is not open", NOEXPR);
	Level = 0;
	return read_form(0);
}

/*----- Printer -----*/

char *ntoa(char *b, cell x, int w) {
	char	buf[40];
	int	i = 0, neg = 0;
	char	*p = &buf[sizeof(buf)-1];

	if (x < 0) {
		x = -x;
		neg = 1;
	}
	*p = 0;
	while (x || i == 0) {
		i++;
		if (i >= sizeof(buf)-1) fatal("ntoa: number too big");
		p--;
		*p = x % 10 + '0';
		x = x / 10;
	}
	while (i < (w-neg) && i < sizeof(buf)-1) {
		i++;
		p--;
		*p = '0';
	}
	if (neg) {
		if (i >= sizeof(buf)-1) fatal("ntoa: number too big");
		p--;
		*p = '-';
	}
	strcpy(b, p);
	return b;
}

/* Print bignum integer. */
int print_integer(cell n) {
	int	first;
	char	buf[DIGITS_PER_WORD+2];

	if (!integer_p(n)) return 0;
	n = cdr(n);
	first = 1;
	while (n != NIL) {
		pr(ntoa(buf, car(n), first? 0: DIGITS_PER_WORD));
		n = cdr(n);
		first = 0;
	}
	return 1;
}

void print_expanded_real(cell m, cell e, int n_digits, int neg) {
	char	buf[DIGITS_PER_WORD+3];
	int	k, first;
	int	dp_offset, old_offset;

	dp_offset = e+n_digits;
	if (neg) pr("-");
	if (dp_offset <= 0) pr("0");
	if (dp_offset < 0) pr(".");
	while (dp_offset < 0) {
		pr("0");
		dp_offset++;
	}
	dp_offset = e+n_digits;
	first = 1;
	while (m != NIL) {
		ntoa(buf, labs(car(m)), first? 0: DIGITS_PER_WORD);
		k = strlen(buf);
		old_offset = dp_offset;
		dp_offset -= k;
		if (dp_offset < 0 && old_offset >= 0) {
			memmove(&buf[k+dp_offset+1], &buf[k+dp_offset],
				-dp_offset+1);
			buf[k+dp_offset] = '.';
		}
		pr(buf);
		m = cdr(m);
		first = 0;
	}
	if (dp_offset >= 0) {
		while (dp_offset > 0) {
			pr("0");
			dp_offset--;
		}
		pr(".0");
	}
}

cell count_digits(cell m) {
	int	k = 0;
	cell	x;

	x = car(m);
	k = 0;
	while (x != 0) {
		x /= 10;
		k++;
	}
	k = k==0? 1: k;
	m = cdr(m);
	while (m != NIL) {
		k += DIGITS_PER_WORD;
		m = cdr(m);
	}
	return k;
}

/* Print real number. */
int print_real(cell n) {
	int	n_digits;
	cell	m, e;
	char	buf[DIGITS_PER_WORD+2];

	if (!real_p(n)) return 0;
	m = real_mantissa(n);
	n_digits = count_digits(m);
	e = real_exponent(n);
	if (e+n_digits > -4 && e+n_digits <= 9) {
		print_expanded_real(m, e, n_digits, real_negative_flag(n));
		return 1;
	}
	if (real_negative_flag(n)) pr("-");
	ntoa(buf, car(m), 0);
	pr_raw(buf, 1);
	pr(".");
	pr(buf[1] || cdr(m) != NIL? &buf[1]: "0");
	m = cdr(m);
	while (m != NIL) {
		pr(ntoa(buf, car(m), DIGITS_PER_WORD));
		m = cdr(m);
	}
	pr("e");
	if (e+n_digits-1 >= 0) pr("+");
	pr(ntoa(buf, e+n_digits-1, 0));
	return 1;
}

/* Print expressions of the form (QUOTE X) as 'X. */
int print_quoted(cell n) {
	if (	car(n) == S_quote &&
		cdr(n) != NIL &&
		cddr(n) == NIL
	) {
		pr("'");
		print_form(cadr(n));
		return 1;
	}
	return 0;
}

int print_procedure(cell n) {
	if (procedure_p(n)) {
		pr("#<procedure ");
		print_form(cadr(n));
		pr(">");
		return 1;
	}
	return 0;
}

int print_char(cell n) {
	char	b[1];
	int	c;

	if (!char_p(n)) return 0;
	if (!Displaying) pr("#\\");
	c = cadr(n);
	if (!Displaying && c == ' ') {
		pr("space");
	}
	else if (!Displaying && c == '\n') {
		pr("newline");
	}
	else {
		b[0] = c;
		pr_raw(b, 1);
	}
	return 1;
}

int print_string(cell n) {
	char	b[1];
	int	k;
	char	*s;

	if (!string_p(n)) return 0;
	if (!Displaying) pr("\"");
	s = string(n);
	k = string_len(n)-1;
	while (k) {
		b[0] = *s++;
		if (!Displaying && (b[0] == '"' || b[0] == '\\'))
			pr("\\");
		pr_raw(b, 1);
		k--;
	}
	if (!Displaying) pr("\"");
	return 1;
}

int print_symbol(cell n) {
	char	b[2];
	int	k;
	char	*s;

	if (!symbol_p(n)) return 0;
	s = symbol_name(n);
	k = symbol_len(n)-1;
	b[1] = 0;
	while (k) {
		b[0] = *s++;
		pr(b);
		k--;
	}
	return 1;
}

int print_primitive(cell n) {
	PRIM	*p;

	if (!primitive_p(n)) return 0;
	pr("#<primitive ");
	p = (PRIM *) cadr(n);
	pr(p->name);
	pr(">");
	return 1;
}

int print_syntax(cell n) {
	if (!syntax_p(n)) return 0;
	pr("#<syntax>");
	return 1;
}

int print_vector(cell n) {
	cell	*p;
	int	k;

	if (!vector_p(n)) return 0;
	pr("#(");
	p = vector(n);
	k = vector_len(n);
	while (k--) {
		print_form(*p++);
		if (k) pr(" ");
	}
	pr(")");
	return 1;
}

int print_port(cell n) {
	char	buf[100];

	if (!input_port_p(n) && !output_port_p(n)) return 0;
	sprintf(buf, "#<%s-port %d>",
		input_port_p(n)? "input": "output",
		(int) port_no(n));
	pr(buf);
	return 1;
}

void print_form(cell n) {
	if (Ports[Output_port] == NULL) {
		error("output port is not open", NOEXPR);
		return;
	}
	if (n == NIL) {
		pr("()");
	}
	else if (n == NAN) {
		pr("#<NaN>");
	}
	else if (eof_p(n)) {
		pr("#<eof>");
	}
	else if (n == FALSE) {
		pr("#f");
	}
	else if (n == TRUE) {
		pr("#t");
	}
	else if (undefined_p(n)) {
		pr("#<undefined>");
	}
	else if (unspecific_p(n)) {
		pr("#<unspecific>");
	}
	else {
		if (print_char(n)) return;
		if (print_procedure(n)) return;
		if (print_integer(n)) return;
		if (print_real(n)) return;
		if (print_primitive(n)) return;
		if (print_quoted(n)) return;
		if (print_string(n)) return;
		if (print_symbol(n)) return;
		if (print_syntax(n)) return;
		if (print_vector(n)) return;
		if (print_port(n)) return;
		pr("(");
		while (n != NIL) {
			print_form(car(n));
			n = cdr(n);
			if (n != NIL && atom_p(n)) {
				pr(" . ");
				print_form(n);
				n = NIL;
			}
			if (n != NIL) pr(" ");
		}
		pr(")");
	}
}

/*----- Miscellanea -----*/

int length(cell n) {
	int	k = 0;

	while (n != NIL) {
		k++;
		n = cdr(n);
	}
	return k;
}

cell append_b(cell a, cell b) {
	cell	p, last = NIL;

	if (a == NIL) return b;
	p = a;
	while (p != NIL) {
		if (atom_p(p)) fatal("append!: improper list");
		last = p;
		p = cdr(p);
	}
	cdr(last) = b;
	return a;
}

cell flat_copy(cell n, cell *lastp) {
	cell	a, m, last, new;

	if (n == NIL) {
		if (lastp != NULL) lastp[0] = NIL;
		return NIL;
	}
	m = alloc3(NIL, NIL, Tag[n]);
	save(m);
	a = m;
	last = m;
	while (n != NIL) {
		car(a) = car(n);
		last = a;
		n = cdr(n);
		if (n != NIL) {
			new = alloc3(NIL, NIL, Tag[n]);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	unsave(1);
	if (lastp != NULL) lastp[0] = last;
	return m;
}

int argument_list_p(cell n) {
	if (n == NIL || symbol_p(n)) return 1;
	if (atom_p(n)) return 0;
	while (!atom_p(n)) {
		if (!symbol_p(car(n))) return 0;
		n = cdr(n);
	}
	return n == NIL || symbol_p(n);
}

#define hash(s, h) \
	do {					\
		h = 0;				\
		while (*s) h = (h<<3) ^ *s++;	\
	} while (0)

int hash_size(int n) {
	if (n < 5) return 5;
	if (n < 11) return 11;
	if (n < 23) return 23;
	if (n < 47) return 47;
	if (n < 97) return 97;
	if (n < 199) return 199;
	if (n < 499) return 499;
	if (n < 997) return 997;
	return 9973;
}

void rehash(cell e) {
	unsigned int	i;
	cell		p, *v, new;
	unsigned int	h, k = hash_size(length(e));
	char		*s;

	if (Program == NIL || k < HASH_THRESHOLD) return;
	new = allocv(T_VECTOR, k * sizeof(cell));
	car(e) = new;
	v = vector(car(e));
	for (i=0; i<k; i++) v[i] = NIL;
	p = cdr(e);
	while (p != NIL) {
		s = symbol_name(caar(p));
		h = 0;
		hash(s, h);
		new = alloc(car(p), v[h%k]);
		v = vector(car(e));
		v[h%k] = new;
		p = cdr(p);
	}
}

cell extend(cell v, cell a, cell e) {
	cell	n, new;

	Tmp = v;
	n = alloc(a, NIL);
	n = alloc(v, n);
	Tmp = NIL;
	new = alloc(n, cdr(e));
	cdr(e) = new;
	if (car(S_loading) == FALSE) rehash(e);
	return e;
}

cell make_env(cell rib, cell env) {
	cell	e;

	Tmp = env;
	rib = alloc(NIL, rib);
	e = alloc(rib, env);
	Tmp = NIL;
	if (length(rib) >= HASH_THRESHOLD) {
		save(e);
		rehash(rib);
		unsave(1);
	}
	return e;
}

cell try_hash(cell v, cell e) {
	cell		*hv, p;
	unsigned int	h, k;
	char		*s;

	if (e == NIL || car(e) == NIL) return NIL;
	hv = vector(car(e));
	k = vector_len(car(e));
	s = symbol_name(v);
	hash(s, h);
	p = hv[h%k];
	while (p != NIL) {
		if (caar(p) == v) return car(p);
		p = cdr(p);
	}
	return NIL;
}

/*----- Evaluator -----*/

cell lookup(cell v, cell env) {
	cell	e, n;

	while (env != NIL) {
		e = car(env);
		n = try_hash(v, e);
		if (n != NIL) return n;
		if (e != NIL) e = cdr(e);
		while (e != NIL) {
			if (v == caar(e)) return car(e);
			e = cdr(e);
		}
		env = cdr(env);
	}
	return NIL;
}

cell location_of(cell v, cell env) {
	cell	n;

	n = lookup(v, env);
	if (n == NIL) {
		if (special_p(v))
			error("invalid syntax", v);
		else
			error("symbol not bound", v);
		return NIL;
	}
	return cdr(n);
}

cell value_of(cell v, cell env) {
	cell	n;

	n = location_of(v, env);
	n = n == NIL? NIL: car(n);
	if (undefined_p(n)) error("symbol not bound", v);
	return n;
}

/*----- Specials -----*/

cell too_few_args(int n) {
	return error("too few arguments", n);
}

cell too_many_args(int n) {
	return error("too many arguments", n);
}

/* Set up sequence for AND, BEGIN, OR. */
cell make_sequence(int state, cell neutral, cell x, int *pc, int *ps) {
	if (cdr(x) == NIL) {
		return neutral;
	}
	else if (cddr(x) == NIL) {
		*pc = 1;
		return cadr(x);
	}
	else {
		*pc = 2;
		*ps = state;
		save(cdr(x));
		return cadr(x);
	}
}

#define sf_and(x, pc, ps) \
	make_sequence(EV_AND, TRUE, x, pc, ps)

#define sf_begin(x, pc, ps) \
	make_sequence(EV_BEGIN, UNSPECIFIC, x, pc, ps)

cell sf_cond(cell x, int *pc, int *ps) {
	cell	clauses, p;

	clauses = cdr(x);
	p = clauses;
	while (p != NIL) {
		if (atom_p(p) || atom_p(car(p)))
			return error("cond: invalid syntax", p);
		p = cdr(p);
	}
	if (clauses == NIL) return UNSPECIFIC;
	if (caar(clauses) == S_else && cdr(clauses) == NIL) {
		p = alloc(TRUE, cdar(clauses));
		clauses = alloc(p, cdr(clauses));
	}
	save(clauses);
	*pc = 2;
	*ps = EV_COND;
	return caar(clauses);
}

cell sf_if(cell x, int *pc, int *ps) {
	cell	m, new;

	m = cdr(x);
	if (m == NIL || cdr(m) == NIL)
		return too_few_args(x);
	if (cddr(m) != NIL && cdddr(m) != NIL)
		return too_many_args(x);
	if (cddr(m) == NIL) {
		new = alloc(UNSPECIFIC, NIL);
		cddr(m) = new;
	}
	save(m);
	*pc = 2;
	*ps = EV_IF_PRED;
	return car(m);
}

cell gensym(char *prefix);

cell make_temporaries(int x) {
	cell	n, v;

	n = NIL;
	save(n);
	while (x != NIL) {
		v = gensym("g");
		n = alloc(v, n);
		car(Stack) = n;
		x = cdr(x);
	}
	unsave(1);
	return n;
}

cell make_assignments(cell x, cell t) {
	cell	n, asg;

	n = NIL;
	save(n);
	while (x != NIL) {
		asg = alloc(car(t), NIL);
		asg = alloc(car(x), asg);
		asg = alloc(S_set_b, asg);
		n = alloc(asg, n);
		car(Stack) = n;
		x = cdr(x);
		t = cdr(t);
	}
	unsave(1);
	return alloc(S_begin, n);
}

cell make_undefineds(int x) {
	cell	n;

	n = NIL;
	while (x != NIL) {
		n = alloc(UNDEFINED, n);
		x = cdr(x);
	}
	return n;
}

/* Returns ((lambda (v0 ...)
 *            ((lambda (t0 ...)
 *               (begin (set! v0 t0)
 *                      ...
 *                      body))
 *             a0 ...))
 *          #<undefined> ...)
 */
cell make_recursive_lambda(int v, int a, int body) {
	cell	t, n;

	t = make_temporaries(v);
	save(t);
	body = append_b(make_assignments(v, t), body);
	body = alloc(body, NIL);
	n = alloc(t, body);
	n = alloc(S_lambda, n);
	n = alloc(n, a);
	n = alloc(n, NIL);
	n = alloc(v, n);
	n = alloc(S_lambda, n);
	save(n);
	n = alloc(n, make_undefineds(v));
	unsave(2);
	return n;
}

enum { VARS, ARGS };

/* Extract variables or arguments from a set of DEFINEs. */
cell extract_from_defines(cell x, int part, cell *restp) {
	cell	a, n, new;

	a = NIL;
	while (x != NIL) {
		if (atom_p(x) || atom_p(car(x)) || caar(x) != S_define)
			break;
		n = car(x);
		if (length(n) < 3)
			return error("define: invalid syntax", n);
		if (pair_p(cadr(n))) {
			/* (define (proc vars) ...) */
			if (part == VARS) {
				a = alloc(caadr(n), a);
			}
			else {
				a = alloc(NIL, a);
				save(a);
				new = alloc(cdadr(n), cddr(n));
				new = alloc(S_lambda, new);
				car(a) = new;
				unsave(1);
			}
		}
		else {
			a = alloc(part==VARS? cadr(n): caddr(n), a);
		}
		x = cdr(x);
	}
	*restp = x;
	return a;
}

/* Rewrite local defines using LAMBDA and SET! */
cell resolve_local_defines(int x) {
	cell	v, a, n, rest;

	a = extract_from_defines(x, ARGS, &rest);
	if (Error_flag) return NIL;
	save(a);
	v = extract_from_defines(x, VARS, &rest);
	save(v);
	if (rest == NIL) rest = alloc(UNSPECIFIC, NIL);
	save(rest);
	n = make_recursive_lambda(v, a, rest);
	unsave(3);
	return n;
}

cell sf_lambda(cell x) {
	cell	n;
	int	k;

	k = length(x);
	if (k < 3) return too_few_args(x);
	if (!argument_list_p(cadr(x)))
		return error("malformed argument list", cadr(x));
	if (pair_p(caddr(x)) && caaddr(x) == S_define)
		n = resolve_local_defines(cddr(x));
	else if (k > 3)
		n = alloc(S_begin, cddr(x));
	else
		n = caddr(x);
	n = alloc(n, Environment);
	n = alloc(cadr(x), n);
	return alloc_atom(T_PROCEDURE, n);
}

cell sf_quote(cell x) {
	if (cdr(x) == NIL) return too_few_args(x);
	if (cddr(x) != NIL) return too_many_args(x);
	return cadr(x);
}

#define sf_or(x, pc, ps) \
	make_sequence(EV_OR, FALSE, x, pc, ps)

cell sf_set_b(cell x, int *pc, int *ps) {
	cell	n;
	int	k;

	k = length(x);
	if (k < 3) return too_few_args(x);
	if (k > 3) return too_many_args(x);
	if (!symbol_p(cadr(x)))
		return error("set!: expected symbol, got", cadr(x));
	n = location_of(cadr(x), Environment);
	if (Error_flag) return NIL;
	save(n);
	*pc = 2;
	*ps = EV_SET_VAL;
	return caddr(x);
}

cell find_local_variable(cell v, cell e) {
	if (e == NIL) return NIL;
	e = cdr(e);
	while (e != NIL) {
		if (v == caar(e)) return car(e);
		e = cdr(e);
	}
	return NIL;
}

cell sf_define(cell x, int *pc, int *ps) {
	cell	v, a, n, new;
	int	k;

	if (car(State_stack) == EV_ARGS)
		return error("define: invalid context", x);
	k = length(x);
	if (k < 3) return too_few_args(x);
	if (symbol_p(cadr(x)) && k > 3) return too_many_args(x);
	if (!argument_list_p(cadr(x)))
		return error("define: expected symbol or list, got", cadr(x));
	if (!symbol_p(cadr(x))) {
		a = cddr(x);
		a = alloc(cdadr(x), a);
		a = alloc(S_lambda, a);
		save(a);
		n = caadr(x);
	}
	else {
		save(NIL);
		a = caddr(x);
		n = cadr(x);
	}
	v = find_local_variable(n, car(Environment));
	if (v == NIL) {
		new = extend(n, UNDEFINED, car(Environment));
		car(Environment) = new;
		v = cadar(Environment);
	}
	car(Stack) = cdr(v);
	*pc = 2;
	if (!atom_p(a) && car(a) == S_lambda)
		*ps = EV_DEFINE;	/* use dynamic scoping */
	else
		*ps = EV_SET_VAL;
	return a;
}

cell sf_define_macro(cell x, int *pc, int *ps) {
	cell	a, n, v, new;
	int	k = length(x);

	if (k < 3) return too_few_args(x);
	if (k > 3) return too_many_args(x);
	if (!argument_list_p(cadr(x)))
		return error("define-macro: expected symbol or list, got",
				cadr(x));
	if (!symbol_p(cadr(x))) {
		a = cddr(x);
		a = alloc(cdadr(x), a);
		a = alloc(S_lambda, a);
		save(a);
		n = caadr(x);
	}
	else {
		save(NIL);
		a = caddr(x);
		n = cadr(x);
	}
	v = lookup(n, Environment);
	if (v == NIL) {
		new = extend(n, UNDEFINED, car(Environment));
		car(Environment) = new;
		v = cadar(Environment);
	}
	car(Stack) = cdr(v);
	*pc = 2;
	*ps = EV_MACRO;
	return a;
}

/*----- Bignums -----*/

cell make_integer(cell i) {
	cell	n;

	n = alloc_atom(i, NIL);
	return alloc_atom(T_INTEGER, n);
}

cell integer_argument(char *who, cell x);

int integer_value(char *src, cell x) {
	char	msg[100];

	x = integer_argument(src, x);
	if (x == NIL) return 0;
	if (cddr(x) != NIL) {
		sprintf(msg, "%s: integer argument too big", src);
		error(msg, x);
		return 0;
	}
	return cadr(x);
}

cell bignum_abs(cell a) {
	cell	n;

	n = alloc_atom(labs(cadr(a)), cddr(a));
	return alloc_atom(T_INTEGER, n);
}

cell bignum_negate(cell a) {
	cell	n;

	n = alloc_atom(-cadr(a), cddr(a));
	return alloc_atom(T_INTEGER, n);
}

cell reverse_segments(cell n) {
	cell	m;

	m = NIL;
	while (n != NIL) {
		m = alloc_atom(car(n), m);
		n = cdr(n);
	}
	return m;
}

cell bignum_add(cell a, cell b);
cell bignum_subtract(cell a, cell b);

cell _bignum_add(cell a, cell b) {
	cell	fa, fb, result, r;
	int	carry;

	if (bignum_negative_p(a)) {
		if (bignum_negative_p(b)) {
			/* -A+-B --> -(|A|+|B|) */
			a = bignum_abs(a);
			save(a);
			a = bignum_add(a, bignum_abs(b));
			unsave(1);
			return bignum_negate(a);
		}
		else {
			/* -A+B --> B-|A| */
			return bignum_subtract(b, bignum_abs(a));
		}
	}
	else if (bignum_negative_p(b)) {
		/* A+-B --> A-|B| */
		return bignum_subtract(a, bignum_abs(b));
	}
	/* A+B */
	a = reverse_segments(cdr(a));
	save(a);
	b = reverse_segments(cdr(b));
	save(b);
	carry = 0;
	result = NIL;
	save(result);
	while (a != NIL || b != NIL || carry) {
		fa = a == NIL? 0: car(a);
		fb = b == NIL? 0: car(b);
		r = fa + fb + carry;
		carry = 0;
		if (r >= INT_SEG_LIMIT) {
			r -= INT_SEG_LIMIT;
			carry = 1;
		}
		result = alloc_atom(r, result);
		car(Stack) = result;
		if (a != NIL) a = cdr(a);
		if (b != NIL) b = cdr(b);
	}
	unsave(3);
	return alloc_atom(T_INTEGER, result);
}

cell bignum_add(cell a, cell b) {
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	a = _bignum_add(a, b);
	unsave(2);
	return a;
}

cell bignum_less_p(cell a, cell b) {
	int	ka, kb, neg_a, neg_b;

	neg_a = bignum_negative_p(a);
	neg_b = bignum_negative_p(b);
	if (neg_a && !neg_b) return 1;
	if (!neg_a && neg_b) return 0;
	ka = length(a);
	kb = length(b);
	if (ka < kb) return neg_a? 0: 1;
	if (ka > kb) return neg_a? 1: 0;
	Tmp = b;
	a = bignum_abs(a);
	save(a);
	b = bignum_abs(b);
	unsave(1);
	Tmp = NIL;
	a = cdr(a);
	b = cdr(b);
	while (a != NIL) {
		if (car(a) < car(b)) return neg_a? 0: 1;
		if (car(a) > car(b)) return neg_a? 1: 0;
		a = cdr(a);
		b = cdr(b);
	}
	return 0;
}

cell bignum_equal_p(cell a, cell b) {
	a = cdr(a);
	b = cdr(b);
	while (a != NIL && b != NIL) {
		if (car(a) != car(b)) return 0;
		a = cdr(a);
		b = cdr(b);
	}
	return a == NIL && b == NIL;
}

cell _bignum_subtract(cell a, cell b) {
	cell	fa, fb, result, r;
	int	borrow;

	if (bignum_negative_p(a)) {
		if (bignum_negative_p(b)) {
			/* -A--B --> -A+|B| --> |B|-|A| */
			a = bignum_abs(a);
			save(a);
			a = bignum_subtract(bignum_abs(b), a);
			unsave(1);
			return a;
		}
		else {
			/* -A-B --> -(|A|+B) */
			return bignum_negate(bignum_add(bignum_abs(a), b));
		}
	}
	else if (bignum_negative_p(b)) {
		/* A--B --> A+|B| */
		return bignum_add(a, bignum_abs(b));
	}
	/* A-B, A<B --> -(B-A) */
	if (bignum_less_p(a, b))
		return bignum_negate(bignum_subtract(b, a));
	/* A-B, A>=B */
	a = reverse_segments(cdr(a));
	save(a);
	b = reverse_segments(cdr(b));
	save(b);
	borrow = 0;
	result = NIL;
	save(result);
	while (a != NIL || b != NIL || borrow) {
		fa = a == NIL? 0: car(a);
		fb = b == NIL? 0: car(b);
		r = fa - fb - borrow;
		borrow = 0;
		if (r < 0) {
			r += INT_SEG_LIMIT;
			borrow = 1;
		}
		result = alloc_atom(r, result);
		car(Stack) = result;
		if (a != NIL) a = cdr(a);
		if (b != NIL) b = cdr(b);
	}
	unsave(3);
	while (car(result) == 0 && cdr(result) != NIL)
		result = cdr(result);
	return alloc_atom(T_INTEGER, result);
}

cell bignum_subtract(cell a, cell b) {
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	a = _bignum_subtract(a, b);
	unsave(2);
	return a;
}

cell bignum_shift_left(cell a, int fill) {
	cell	r, c, result;
	int	carry;

	a = reverse_segments(cdr(a));
	save(a);
	carry = fill;
	result = NIL;
	save(result);
	while (a != NIL) {
		if (car(a) >= INT_SEG_LIMIT/10) {
			c = car(a) / (INT_SEG_LIMIT/10);
			r = car(a) % (INT_SEG_LIMIT/10) * 10;
			r += carry;
			carry = c;
		}
		else {
			r = car(a) * 10 + carry;
			carry = 0;
		}
		result = alloc_atom(r, result);
		car(Stack) = result;
		a = cdr(a);
	}
	if (carry) result = alloc_atom(carry, result);
	unsave(2);
	return alloc_atom(T_INTEGER, result);
}

/* Result: (a/10 . a%10) */
cell bignum_shift_right(cell a) {
	cell	r, c, result;
	int	carry;

	a = cdr(a);
	save(a);
	carry = 0;
	result = NIL;
	save(result);
	while (a != NIL) {
		c = car(a) % 10;
		r = car(a) / 10;
		r += carry * (INT_SEG_LIMIT/10);
		carry = c;
		result = alloc_atom(r, result);
		car(Stack) = result;
		a = cdr(a);
	}
	result = reverse_segments(result);
	if (car(result) == 0 && cdr(result) != NIL) result = cdr(result);
	result = alloc_atom(T_INTEGER, result);
	car(Stack) = result;
	carry = make_integer(carry);
	unsave(2);
	return alloc(result, carry);
}

cell bignum_multiply(cell a, cell b) {
	int	neg;
	cell	r, i, result;

	neg = bignum_negative_p(a) != bignum_negative_p(b);
	a = bignum_abs(a);
	save(a);
	b = bignum_abs(b);
	save(b);
	result = make_integer(0);
	save(result);
	while (!bignum_zero_p(a)) {
		if (Error_flag) break;
		r = bignum_shift_right(a);
		i = caddr(r);
		a = car(r);
		caddr(Stack) = a;
		while (i) {
			result = bignum_add(result, b);
			car(Stack) = result;
			i--;
		}
		b = bignum_shift_left(b, 0);
		cadr(Stack) = b;
	}
	if (neg) result = bignum_negate(result);
	unsave(3);
	return result;
}

cell bignum_equalize(cell a, cell b) {
	cell	r, f, r0, f0;

	r0 = a;
	save(r0);
	f0 = make_integer(1);
	save(f0);
	r = r0;
	save(r);
	f = f0;
	save(f);
	while (bignum_less_p(r, b)) {
		cadddr(Stack) = r0 = r;
		caddr(Stack) = f0 = f;
		r = bignum_shift_left(r, 0);
		cadr(Stack) = r;
		f = bignum_shift_left(f, 0);
		car(Stack) = f;
	}
	unsave(4);
	return alloc(r0, f0);
}

/* Result: (a/b . a%b) */
cell _bignum_divide(cell a, cell b) {
	int	neg, neg_a;
	cell	result, f;
	int	i;
	cell	c, c0;

	neg_a = bignum_negative_p(a);
	neg = neg_a != bignum_negative_p(b);
	a = bignum_abs(a);
	save(a);
	b = bignum_abs(b);
	save(b);
	if (bignum_less_p(a, b)) {
		if (neg_a) a = bignum_negate(a);
		f = make_integer(0);
		unsave(2);
		return alloc(f, a);
	}
	b = bignum_equalize(b, a);
	cadr(Stack) = b; /* cadddddr */
	car(Stack) = a;	/* caddddr */
	c = NIL;
	save(c);	/* cadddr */
	c0 = NIL;
	save(c0);	/* caddr */
	f = cdr(b);
	b = car(b);
	cadddr(Stack) = b;
	save(f);	/* cadr */
	result = make_integer(0);
	save(result);	/* car */
	while (!bignum_zero_p(f)) {
		if (Error_flag) break;
		c = make_integer(0);
		cadddr(Stack) = c;
		caddr(Stack) = c0 = c;
		i = 0;
		while (!bignum_less_p(a, c)) {
			if (Error_flag) break;
			caddr(Stack) = c0 = c;
			c = bignum_add(c, b);
			cadddr(Stack) = c;
			i++;
		}
		result = bignum_shift_left(result, i-1);
		car(Stack) = result;
		a = bignum_subtract(a, c0);
		car(cddddr(Stack)) = a;
		f = car(bignum_shift_right(f));
		cadr(Stack) = f;
		b = car(bignum_shift_right(b));
		cadr(cddddr(Stack)) = b;
	}
	if (neg) result = bignum_negate(result);
	car(Stack) = result;
	if (neg_a) a = bignum_negate(a);
	unsave(6);
	return alloc(result, a);
}

cell bignum_divide(cell x, cell a, cell b) {
	if (bignum_zero_p(b))
		return error("divide by zero", x);
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	a = _bignum_divide(a, b);
	unsave(2);
	return a;
}

cell bignum_read(char *pre, int radix) {
	char	digits[] = "0123456789abcdef";
	char	buf[100];
	cell	base, num;
	int	c, s, p, nd;

	base = make_integer(radix);
	save(base);
	num = make_integer(0);
	save(num);
	c = read_c_ci();
	s = 0;
	if (c == '-') {
		s = 1;
		c = read_c_ci();
	}
	else if (c == '+') {
		c = read_c_ci();
	}
	nd = 0;
	while (!separator(c)) {
		p = 0;
		while (digits[p] && digits[p] != c) p++;
		if (p >= radix) {
			sprintf(buf, "invalid digit in %s number: %c",
				pre, c);
			unsave(2);
			return error(buf, NOEXPR);
		}
		num = bignum_multiply(num, base);
		car(Stack) = num;
		num = bignum_add(num, make_integer(p));
		car(Stack) = num;
		nd++;
		c = read_c_ci();
	}
	unsave(2);
	if (!nd) {
		sprintf(buf, "digits expected after %s", pre);
		return error(buf, NOEXPR);
	}
	reject(c);
	return s? bignum_negate(num): num;
}

/*----- Floating Point Arithmetics -----*/

cell real_to_integer(cell x);

cell integer_argument(char *who, cell x) {
	cell	n;
	char	msg[100];

	if (real_p(x)) {
		n = real_to_integer(x);
		if (n == NIL || real_inexact_flag(x)) {
			sprintf(msg, "%s: expected integer, got (inexact)",
				who);
			error(msg, x);
			return NIL;
		}
		return n;
	}
	return x;
}

cell make_real(int flags, cell exp, cell mant) {
	cell	n;

	Tmp = mant;
	n = alloc_atom(exp, mant);
	n = alloc_atom(flags, n);
	Tmp = NIL;
	return alloc_atom(T_REAL, n);
}

/*
 * Remove trailing zeros and move the decimal
 * point to the END of the mantissa, e.g.:
 * real_normalize(1.234e0) --> 1234e-3
 *
 * Limit the mantissa to MANTISSA_SEGMENTS
 * machine words. When doing so causes a loss
 * of precision, mark the resulting number as
 * inexact.
 *
 * Also handle numeric overflow/underflow.
 */
cell real_normalize(cell x, char *who) {
	cell	m, e, r;
	int	dgs, inexact;
	char	buf[50];

	save(x);
	e = real_exponent(x);
	m = alloc_atom(T_INTEGER, real_mantissa(x));
	save(m);
	dgs = count_digits(cdr(m));
	inexact = real_inexact_flag(x);
	while (dgs > MANTISSA_SIZE) {
		r = bignum_shift_right(m);
		if (!bignum_zero_p(cdr(r)))
			inexact = REAL_INEXACT;
		m = car(r);
		car(Stack) = m;
		dgs--;
		e++;
	}
	while (!bignum_zero_p(m)) {
		r = bignum_shift_right(m);
		if (!bignum_zero_p(cdr(r))) break;
		m = car(r);
		car(Stack) = m;
		e++;
	}
	if (bignum_zero_p(m)) e = 0;
	r = alloc_atom(e, NIL);
	unsave(2);
	if (count_digits(r) > DIGITS_PER_WORD) {
		sprintf(buf, "%s: floating point %sflow",
			who? who: "internal",
			e<0? "under": "over");
		error(buf, NOEXPR);
	}
	return make_real(real_flags(x) | inexact, e, cdr(m));
}

cell bignum_to_real(cell a) {
	int	nseg, e, flags, i;
	int	inexact;
	cell	x, n;

	x = flat_copy(a, NULL);
	cadr(x) = labs(cadr(x));
	save(x);
	nseg = length(cdr(x));
	inexact = 0;
	if (nseg > MANTISSA_SEGMENTS) {
		n = x;
		for (i=0; i < MANTISSA_SEGMENTS; i++)
			n = cdr(n);
		cdr(n) = NIL;
		e = (nseg-MANTISSA_SEGMENTS) * DIGITS_PER_WORD;
		inexact = REAL_INEXACT;
	}
	else {
		e = 0;
	}
	unsave(1);
	flags = inexact | (bignum_negative_p(a)? REAL_NEGATIVE: 0);
	x = make_real(flags, e, cdr(x));
	return real_normalize(x, NULL);
}

int real_equal_p(cell a, cell b) {
	cell	ma, mb;

	if (integer_p(a) && integer_p(b))
		return bignum_equal_p(a, b);
	if (integer_p(a))
		a = bignum_to_real(a);
	if (integer_p(b)) {
		save(a);
		b = bignum_to_real(b);
		unsave(1);
	}
	if (real_exponent(a) != real_exponent(b)) return 0;
	if (real_zero_p(a) && real_zero_p(b)) return 1;
	if (real_negative_p(a) != real_negative_p(b)) return 0;
	ma = real_mantissa(a);
	mb = real_mantissa(b);
	while (ma != NIL && mb != NIL) {
		if (car(ma) != car(mb)) return 0;
		ma = cdr(ma);
		mb = cdr(mb);
	}
	if (ma != mb) return 0;
	return 1;
}

/*
 * Scale the number R so that it gets exponent DESIRED_E
 * without changing its value. When there is not enough
 * room for scaling the mantissa of R, return NIL.
 * E.g.: scale_mantissa(1.0e0, -2, *) --> 100.0e-2
 *
 * Allow the mantissa to grow to MAX_SIZE segments.
 */
cell scale_mantissa(cell r, cell desired_e, int max_size) {
	int	dgs;
	cell	n, e;

	dgs = count_digits(real_mantissa(r));
	if (max_size - dgs < real_exponent(r) - desired_e)
		return NIL;
	n = alloc_atom(T_INTEGER, flat_copy(real_mantissa(r), NULL));
	save(n);
	e = real_exponent(r);
	while (e > desired_e) {
		n = bignum_shift_left(n, 0);
		car(Stack) = n;
		e--;
	}
	unsave(1);
	return make_real(real_flags(r), e, cdr(n));
}

void autoscale(cell *pa, cell *pb) {
	if (real_exponent(*pa) < real_exponent(*pb)) {
		*pb = scale_mantissa(*pb, real_exponent(*pa), MANTISSA_SIZE*2);
		return;
	}
	if (real_exponent(*pa) > real_exponent(*pb)) {
		*pa = scale_mantissa(*pa, real_exponent(*pb), MANTISSA_SIZE*2);
	}
}

int real_less_p(cell a, cell b) {
	cell	ma, mb;
	int	ka, kb, neg;
	int	dpa, dpb;

	if (integer_p(a) && integer_p(b))
		return bignum_less_p(a, b);
	if (integer_p(a))
		a = bignum_to_real(a);
	if (integer_p(b)) {
		save(a);
		b = bignum_to_real(b);
		unsave(1);
	}
	if (real_negative_p(a) && real_positive_p(b)) return 1;
	if (real_negative_p(b) && real_positive_p(a)) return 0;
	if (real_positive_p(a) && real_zero_p(b)) return 0;
	if (real_zero_p(a) && real_positive_p(a)) return 1;
	neg = real_negative_p(a);
	dpa = count_digits(real_mantissa(a)) + real_exponent(a);
	dpb = count_digits(real_mantissa(b)) + real_exponent(b);
	if (dpa < dpb) return neg? 0: 1;
	if (dpa > dpb) return neg? 1: 0;
	if (real_exponent(a) < real_exponent(b)) {
		Tmp = b;
		save(a);
		save(b);
		Tmp = NIL;
		b = scale_mantissa(b, real_exponent(a), MANTISSA_SIZE);
		unsave(2);
		if (b == NIL) return neg? 0: 1;
	}
	if (real_exponent(b) < real_exponent(a)) {
		Tmp = b;
		save(a);
		save(b);
		Tmp = NIL;
		a = scale_mantissa(a, real_exponent(b), MANTISSA_SIZE);
		unsave(2);
		if (a == NIL) return neg? 1: 0;
	}
	ma = real_mantissa(a);
	mb = real_mantissa(b);
	ka = length(ma);
	kb = length(mb);
	if (ka < kb) return 1;
	if (ka > kb) return 0;
	while (ma != NIL) {
		if (car(ma) < car(mb)) return neg? 0: 1;
		if (car(ma) > car(mb)) return neg? 1: 0;
		ma = cdr(ma);
		mb = cdr(mb);
	}
	return 0;
}

cell real_add(cell a, cell b) {
	cell	r, m, e;
	int	flags, nega, negb, inexact;

	if (integer_p(a) && integer_p(b))
		return bignum_add(a, b);
	if (integer_p(a)) a = bignum_to_real(a);
	save(a);
	if (integer_p(b)) b = bignum_to_real(b);
	save(b);
	inexact = real_inexact_flag(a) | real_inexact_flag(b);
	autoscale(&a, &b);
	if (a == NIL || b == NIL) {
		b = unsave(1);
		a = unsave(1);
		return real_less_p(a, b)?
			real_to_inexact(b):
			real_to_inexact(a);
	}
	cadr(Stack) = a;
	car(Stack) = b;
	e = real_exponent(a);
	nega = real_negative_p(a);
	negb = real_negative_p(b);
	a = alloc_atom(T_INTEGER, real_mantissa(a));
	if (nega) a = bignum_negate(a);
	cadr(Stack) = a;
	b = alloc_atom(T_INTEGER, real_mantissa(b));
	if (negb) b = bignum_negate(b);
	car(Stack) = b;
	m = bignum_add(a, b);
	unsave(2);
	flags = inexact | (bignum_negative_p(m)? REAL_NEGATIVE: 0);
	r = make_real(flags, e, cdr(bignum_abs(m)));
	return real_normalize(r, "+");
}

cell real_subtract(cell a, cell b) {
	cell	r;

	if (integer_p(b))
		b = bignum_negate(b);
	else
		b = real_negate(b);
	save(b);
	r = real_add(a, b);
	unsave(1);
	return r;
}

cell real_multiply(cell a, cell b) {
	cell	r, m, e, ma, mb, ea, eb, neg;
	int	inexact;

	if (integer_p(a) && integer_p(b))
		return bignum_multiply(a, b);
	if (integer_p(a)) a = bignum_to_real(a);
	save(a);
	if (integer_p(b)) b = bignum_to_real(b);
	save(b);
	inexact = real_inexact_flag(a) | real_inexact_flag(b);
	neg = real_negative_flag(a) != real_negative_flag(b);
	ea = real_exponent(a);
	eb = real_exponent(b);
	ma = alloc_atom(T_INTEGER, real_mantissa(a));
	cadr(Stack) = ma;
	mb = alloc_atom(T_INTEGER, real_mantissa(b));
	car(Stack) = mb;
	e = ea + eb;
	m = bignum_multiply(ma, mb);
	unsave(2);
	r = make_real(inexact | (neg? REAL_NEGATIVE: 0), e, cdr(m));
	return real_normalize(r, "*");
}

cell real_divide(cell x, cell a, cell b) {
	cell	r, m, e, ma, mb, ea, eb, neg;
	int	inexact, nd, dd;

	if (integer_p(a)) a = bignum_to_real(a);
	if (real_zero_p(a)) {
		return make_real(0, 0, cdr(make_integer(0)));
	}
	save(a);
	if (integer_p(b)) b = bignum_to_real(b);
	save(b);
	inexact = real_inexact_flag(a) | real_inexact_flag(b);
	neg = real_negative_flag(a) != real_negative_flag(b);
	ea = real_exponent(a);
	eb = real_exponent(b);
	ma = alloc_atom(T_INTEGER, real_mantissa(a));
	cadr(Stack) = ma;
	mb = alloc_atom(T_INTEGER, real_mantissa(b));
	car(Stack) = mb;
	if (bignum_zero_p(mb)) {
		unsave(2);
		return NAN;
	}
	nd = count_digits(cdr(ma));
	dd = MANTISSA_SIZE + count_digits(cdr(mb));
	while (nd < dd) {
		ma = bignum_shift_left(ma, 0);
		cadr(Stack) = ma;
		nd++;
		ea--;
	}
	e = ea - eb;
	m = bignum_divide(NOEXPR, ma, mb);
	if (!bignum_zero_p(cdr(m))) inexact = REAL_INEXACT;
	unsave(2);
	r = make_real(inexact | (neg? REAL_NEGATIVE: 0), e, cdar(m));
	return real_normalize(r, "/");
}

cell real_to_integer(cell r) {
	cell	n;
	int	neg;

	if (real_exponent(r) >= 0) {
		neg = real_negative_p(r);
		n = scale_mantissa(r, 0, MANTISSA_SIZE);
		if (n == NIL) return NIL;
		n = alloc_atom(T_INTEGER, real_mantissa(n));
		if (neg) n = bignum_negate(n);
		return n;
	}
	return NIL;
}

/*----- Primitives -----*/

cell pp_apply(cell x) {
	cell	m, p, q, last;
	char	*err = "apply: improper argument list";

	m = cdr(x);
	p = cdr(m);
	last = p;
	while (p != NIL) {
		if (atom_p(p)) return error(err, x);
		last = p;
		p = cdr(p);
	}
	p = car(last);
	while (p != NIL) {
		if (atom_p(p)) return error(err, car(last));
		p = cdr(p);
	}
	if (cddr(m) == NIL) {
		p = cadr(m);
	}
	else {
		p = flat_copy(cdr(m), &q);
		q = p;
		while (cddr(q) != NIL) q = cdr(q);
		cdr(q) = car(last);
	}
	return alloc(car(m), p);
}

cell pp_boolean_p(cell x) {
	return boolean_p(cadr(x))? TRUE: FALSE;
}

cell pp_car(cell x) {
	return caadr(x);
}

cell pp_cdr(cell x) {
	return cdadr(x);
}

cell pp_char_p(cell x) {
	return char_p(cadr(x))? TRUE: FALSE;
}

cell pp_char_to_integer(cell x) {
	return make_integer(cadadr(x));
}

cell pp_char_alphabetic_p(cell x) {
	return isalpha(char_value(cadr(x)))? TRUE: FALSE;
}

#define L(c) tolower(c)
int char_ci_le(int c1, int c2) { return L(c1) <= L(c2); }
int char_ci_lt(int c1, int c2) { return L(c1) <  L(c2); }
int char_ci_eq(int c1, int c2) { return L(c1) == L(c2); }
int char_ci_ge(int c1, int c2) { return L(c1) >= L(c2); }
int char_ci_gt(int c1, int c2) { return L(c1) >  L(c2); }

int char_le(int c1, int c2) { return c1 <= c2; }
int char_lt(int c1, int c2) { return c1 <  c2; }
int char_eq(int c1, int c2) { return c1 == c2; }
int char_ge(int c1, int c2) { return c1 >= c2; }
int char_gt(int c1, int c2) { return c1 >  c2; }

cell char_predicate(char *name, int (*p)(int c1, int c2), cell x) {
	char	msg[100];

	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!char_p(cadr(x))) {
			sprintf(msg, "%s: expected char, got", name);
			return error(msg, cadr(x));
		}
		if (!p(char_value(car(x)), char_value(cadr(x))))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

#define R return
cell pp_char_ci_le_p(cell x) { R char_predicate("char-ci<=?", char_ci_le, x); }
cell pp_char_ci_lt_p(cell x) { R char_predicate("char-ci<?",  char_ci_lt, x); }
cell pp_char_ci_eq_p(cell x) { R char_predicate("char-ci=?",  char_ci_eq, x); }
cell pp_char_ci_ge_p(cell x) { R char_predicate("char-ci>=?", char_ci_ge, x); }
cell pp_char_ci_gt_p(cell x) { R char_predicate("char-ci>?",  char_ci_gt, x); }

cell pp_char_le_p(cell x) { R char_predicate("char<=?", char_le, x); }
cell pp_char_lt_p(cell x) { R char_predicate("char<?",  char_lt, x); }
cell pp_char_eq_p(cell x) { R char_predicate("char=?",  char_eq, x); }
cell pp_char_ge_p(cell x) { R char_predicate("char>=?", char_ge, x); }
cell pp_char_gt_p(cell x) { R char_predicate("char>?",  char_gt, x); }

cell pp_char_downcase(cell x) {
	return make_char(tolower(char_value(cadr(x))));
}

cell pp_char_lower_case_p(cell x) {
	return islower(char_value(cadr(x)))? TRUE: FALSE;
}

cell pp_char_numeric_p(cell x) {
	return isdigit(char_value(cadr(x)))? TRUE: FALSE;
}

cell pp_char_upcase(cell x) {
	return make_char(toupper(char_value(cadr(x))));
}

cell pp_char_upper_case_p(cell x) {
	return isupper(char_value(cadr(x)))? TRUE: FALSE;
}

cell pp_char_whitespace_p(cell x) {
	int	c = char_value(cadr(x));

	return (c == ' '  || c == '\t' || c == '\n' ||
		c == '\r' || c == '\f')? TRUE: FALSE;
}

void close_port(int port) {
	if (port < 0 || port >= MAX_PORTS) return;
	if (Ports[port] == NULL) {
		Port_flags[port] = 0;
		return;
	}
	if (fclose(Ports[port]))
		fatal("close_port: fclose() failed");
	Ports[port] = NULL;
	Port_flags[port] = 0;
}

cell pp_close_input_port(cell x) {
	if (port_no(cadr(x)) < 2)
		return error("please do not close the standard input port",
				NOEXPR);
	close_port(port_no(cadr(x)));
	return UNSPECIFIC;
}

cell pp_close_output_port(cell x) {
	if (port_no(cadr(x)) < 2)
		return error("please do not close the standard output port",
				NOEXPR);
	close_port(port_no(cadr(x)));
	return UNSPECIFIC;
}

cell pp_cons(cell x) {
	return alloc(cadr(x), caddr(x));
}

cell make_port(int portno, cell type) {
	cell	n;

	n = alloc_atom(portno, NIL);
	return alloc3(type, n, ATOM_TAG|PORT_TAG);
}

cell pp_current_input_port(cell x) {
	return make_port(Input_port, T_INPUT_PORT);
}

cell pp_current_output_port(cell x) {
	return make_port(Output_port, T_OUTPUT_PORT);
}

cell pp_write(cell x);

cell pp_display(cell x) {
	Displaying = 1;
	pp_write(x);
	Displaying = 0;
	return UNSPECIFIC;
}

void dump_image(char *p);

cell pp_dump_image(cell x) {
	char	*path = copy_string(string(cadr(x)));
	FILE	*f;

	f = fopen(string(cadr(x)), "r");
	if (f != NULL) {
		fclose(f);
		return error("dump-image: file exists", cadr(x));
	}
	dump_image(path);
	free(path);
	return UNSPECIFIC;
}

cell pp_divide(cell x) {
	cell	a, expr;

	expr = x;
	x = cdr(x);
	if (cdr(x) == NIL) {
		a = make_integer(1);
		save(a);
		a = real_divide(expr, a, car(x));
		unsave(1);
		return a;
	}
	a = car(x);
	x = cdr(x);
	save(a);
	while (x != NIL) {
		if (!number_p(car(x))) {
			unsave(1);
			return error("/: expected number, got", car(x));
		}
		a = real_divide(expr, a, car(x));
		car(Stack) = a;
		x = cdr(x);
	}
	unsave(1);
	return a;
}

cell pp_delete_file(cell x) {
	if (remove(string(cadr(x))) < 0)
		error("delete-file: file does not exist", cadr(x));
	return UNSPECIFIC;
}

cell pp_eof_object_p(cell x) {
	return cadr(x) == ENDOFFILE? TRUE: FALSE;
}

cell pp_eq_p(cell x) {
	return cadr(x) == caddr(x)? TRUE: FALSE;
}

cell pp_equal(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error("=: expected number, got", cadr(x));
		if (!real_equal_p(car(x), cadr(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_exact_to_inexact(cell x) {
	cell	n;
	int	flags;

	x = cadr(x);
	if (integer_p(x)) {
		flags = (bignum_negative_p(x)? REAL_NEGATIVE: 0) |
			REAL_INEXACT;
		n = make_real(flags, 0, cdr(bignum_abs(x)));
		return real_normalize(n, "exact->inexact");
	}
	return real_to_inexact(x);
}

cell pp_exact_p(cell x) {
	if (integer_p(cadr(x))) return TRUE;
	return real_inexact_flag(cadr(x))? FALSE: TRUE;
}

cell expand_syntax(cell x);

cell pp_expand_macro(cell x) {
	x = cadr(x);
	save(x);
	x = expand_syntax(x);
	unsave(1);
	return x;
}

cell pp_error(cell x) {
	return error(string(cadr(x)), length(x) > 2? caddr(x): NOEXPR);
}

cell pp_exponent(cell x) {
	if (integer_p(cadr(x))) return make_integer(0);
	return make_integer(real_exponent(cadr(x)));
}

cell pp_file_exists_p(cell x) {
	FILE	*f;

	f = fopen(string(cadr(x)), "r");
	if (f == NULL) return FALSE;
	fclose(f);
	return TRUE;
}

cell pp_floor(cell x) {
	cell	n, m, e;

	x = cadr(x);
	e = real_exponent(x);
	if (e >= 0) return x;
	m = alloc_atom(T_INTEGER, real_mantissa(x));
	save(m);
	while (e < 0) {
		m = bignum_shift_right(m);
		m = car(m);
		car(Stack) = m;
		e++;
	}
	if (real_negative_p(x)) {
		/* sign not in mantissa! */
		m = bignum_add(m, make_integer(1));
	}
	unsave(1);
	n = make_real(real_flags(x), e, cdr(m));
	return real_normalize(n, "floor");
}

cell gensym(char *prefix) {
	static long	g = 0;
	char		s[200];

	do {
		sprintf(s, "%s%ld", prefix, g);
		g++;
	} while (find_symbol(s) != NIL);
	return add_symbol(s);
}

cell pp_gensym(cell x) {
	char	*pre;
	int	k;

	if (cdr(x) == NIL) {
		pre = "g";
		k = 1;
	}
	else {
		pre = string(cadr(x));
		k = string_len(cadr(x));
	}
	if (k > 100)
		return error("gensym: prefix too long", cadr(x));
	return gensym(pre);
}

cell pp_greater(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error(">: expected number, got", cadr(x));
		if (!real_less_p(cadr(x), car(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_greater_equal(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error(">=: expected number, got", cadr(x));
		if (real_less_p(car(x), cadr(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_inexact_p(cell x) {
	if (integer_p(cadr(x))) return FALSE;
	return real_inexact_flag(cadr(x))? TRUE: FALSE;
}

cell pp_inexact_to_exact(cell x) {
	cell	n;

	x = cadr(x);
	if (integer_p(x)) return x;
	n = real_to_integer(x);
	if (n != NIL) return n;
	return real_to_exact(x);
}

cell pp_input_port_p(cell x) {
	return input_port_p(cadr(x))? TRUE: FALSE;
}

cell pp_integer_to_char(cell x) {
	cell	n;

	n = integer_value("integer->char", cadr(x));
	if (n < 0 || n > 255)
		return error("integer->char: argument value out of range",
				cadr(x));
	return make_char(n);
}

cell pp_integer_p(cell x) {
	if (integer_p(cadr(x))) return TRUE;
	if (real_p(cadr(x)) && real_to_integer(cadr(x)) != NIL)
		return TRUE;
	return FALSE;
}

cell pp_less(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error("<: expected number, got", cadr(x));
		if (!real_less_p(car(x), cadr(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_less_equal(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error("<=: expected number, got", cadr(x));
		if (real_less_p(cadr(x), car(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_list_to_string(cell x) {
	cell	n, p;
	int	k = length(cadr(x));
	char	*s;

	n = make_string("", k);
	s = string(n);
	p = cadr(x);
	while (p != NIL) {
		if (atom_p(p))
			return error("list->string: improper list", p);
		if (!char_p(car(p)))
			return error("list->string: expected list of char, "
				"got list containing",
				car(p));
		*s++ = cadar(p);
		p = cdr(p);
	}
	*s = 0;
	return n;
}

cell pp_list_to_vector(cell x) {
	return list_to_vector(cadr(x), "improper list in list->vector", 0);
}

int alloc_port(void) {
	int	i, tries;

	for (tries=0; tries<2; tries++) {
		for (i=0; i<MAX_PORTS; i++) {
			if (Ports[i] == NULL) {
				return i;
			}
		}
		if (tries == 0) gc();
	}
	return -1;
}

int open_port(char *path, char *mode) {
	int	i = alloc_port();

	if (i < 0) return -1;
	Ports[i] = fopen(path, mode);
	if (Ports[i] == NULL)
		return -1;
	else
		return i;
}

cell eval(cell x);

int load(char *file) {
	int	n;
	int	outer_lno;
	int	new_port, old_port;
	int	outer_loading;

	new_port = open_port(file, "r");
	if (new_port == -1) return -1;
	Port_flags[new_port] |= LOCK_TAG;
	File_list = alloc(make_string(file, (int) strlen(file)), File_list);
	save(Environment);
	while (cdr(Environment) != NIL)
		Environment = cdr(Environment);
	outer_loading = car(S_loading);
	car(S_loading) = TRUE;
	old_port = Input_port;
	outer_lno = Line_no;
	Line_no = 1;
	while (!Error_flag) {
		Input_port = new_port;
		n = xread();
		Input_port = old_port;
		if (n == ENDOFFILE) break;
		if (!Error_flag) n = eval(n);
	}
	close_port(new_port);
	Line_no = outer_lno;
	car(S_loading) = outer_loading;
	File_list = cdr(File_list);
	rehash(car(Environment));
	Environment = unsave(1);
	return 0;
}

cell pp_load(cell x) {
	char	file[TOKEN_LENGTH+1];

	if (string_len(cadr(x)) > TOKEN_LENGTH)
		return error("load: path too long", cadr(x));
	strcpy(file, string(cadr(x)));
	if (load(file) < 0)
		return error("load: cannot open file", cadr(x));
	return UNSPECIFIC;
}

cell pp_make_string(cell x) {
	cell	n;
	int	c, k;
	char	*s;

	k = integer_value("make-string", cadr(x));
	n = make_string("", k);
	s = string(n);
	c = cddr(x) == NIL? ' ': char_value(caddr(x));
	memset(s, c, k);
	s[k] = 0;
	return n;
}

cell pp_make_vector(cell x) {
	int	i, k;
	cell	n, *v, m;

	k = integer_value("make-vector", cadr(x));
	n = allocv(T_VECTOR, k * sizeof(cell));
	v = vector(n);
	m = cddr(x) == NIL? FALSE: caddr(x);
	for (i=0; i<k; i++) v[i] = m;
	return n;
}

cell pp_mantissa(cell x) {
	cell	m;

	if (integer_p(cadr(x))) return cadr(x);
	m = alloc_atom(T_INTEGER, real_mantissa(cadr(x)));
	if (real_negative_p(cadr(x))) m = bignum_negate(m);
	return m;
}

cell pp_minus(cell x) {
	cell	a;

	x = cdr(x);
	if (cdr(x) == NIL) {
		if (integer_p(car(x)))
			return bignum_negate(car(x));
		return real_negate(car(x));
	}
	a = car(x);
	x = cdr(x);
	save(a);
	while (x != NIL) {
		if (!number_p(car(x))) {
			unsave(1);
			return error("-: expected number, got", car(x));
		}
		a = real_subtract(a, car(x));
		car(Stack) = a;
		x = cdr(x);
	}
	unsave(1);
	return a;
}

cell pp_open_input_file(cell x) {
	cell	n;
	int	p;

	p = open_port(string(cadr(x)), "r");
	if (p < 0) return error("open-input-file: could not open file",
				cadr(x));
	Port_flags[p] |= LOCK_TAG;
	n = make_port(p, T_INPUT_PORT);
	Port_flags[p] &= ~LOCK_TAG;
	return n;
}

cell pp_open_output_file(cell x) {
	cell	n;
	int	p;
	FILE	*f;

	f = fopen(string(cadr(x)), "r");
	if (f != NULL) {
		fclose(f);
		return error("open-output-file: file already exists",
				cadr(x));
	}
	p = open_port(string(cadr(x)), "w");
	if (p < 0) return error("open-output-file: could not open file",
				cadr(x));
	Port_flags[p] |= LOCK_TAG;
	n = make_port(p, T_OUTPUT_PORT);
	Port_flags[p] &= ~LOCK_TAG;
	return n;
}

cell pp_output_port_p(cell x) {
	return output_port_p(cadr(x))? TRUE: FALSE;
}

cell pp_pair_p(cell x) {
	return atom_p(cadr(x))? FALSE: TRUE;
}

cell pp_plus(cell x) {
	cell	a;

	x = cdr(x);
	if (x == NIL) return make_integer(0);
	if (cdr(x) == NIL) return car(x);
	a = make_integer(0);
	save(a);
	while (x != NIL) {
		if (!number_p(car(x))) {
			unsave(1);
			return error("+: expected number, got", car(x));
		}
		a = real_add(a, car(x));
		car(Stack) = a;
		x = cdr(x);
	}
	unsave(1);
	return a;
}

cell pp_procedure_p(cell x) {
	return (procedure_p(cadr(x)) || primitive_p(cadr(x)))?
		TRUE: FALSE;
}

cell pp_quotient(cell x) {
	char	name[] = "quotient";
	cell	a, b;

	a = integer_argument(name, cadr(x));
	save(a);
	b = integer_argument(name, caddr(x));
	unsave(1);
	return car(bignum_divide(x, a, b));
}

cell pp_read(cell x) {
	cell	n;
	int	new_port, old_port;

	new_port = cdr(x) == NIL? Input_port: port_no(cadr(x));
	if (new_port < 0 || new_port >= MAX_PORTS)
		return error("read: invalid input port (oops)", cadr(x));
	old_port = Input_port;
	Input_port = new_port;
	n = xread();
	Input_port = old_port;
	return n;
}

cell read_char(cell x, int unget) {
	int	c, new_port, old_port;

	new_port = cdr(x) == NIL? Input_port: port_no(cadr(x));
	if (new_port < 0 || new_port >= MAX_PORTS)
		return error("read-char: invalid input port (oops)", cadr(x));
	if (Ports[new_port] == NULL)
		return error("read-char: input port is not open", NOEXPR);
	old_port = Input_port;
	Input_port = new_port;
	c = read_c();
	if (unget) reject(c);
	Input_port = old_port;
	return c == EOF? ENDOFFILE: make_char(c);
}

cell pp_peek_char(cell x) {
	return read_char(x, 1);
}

cell pp_read_char(cell x) {
	return read_char(x, 0);
}

cell pp_real_p(cell x) {
	return number_p(cadr(x))? TRUE: FALSE;
}

cell pp_remainder(cell x) {
	char	name[] = "remainder";
	cell	a, b;

	a = integer_argument(name, cadr(x));
	save(a);
	b = integer_argument(name, caddr(x));
	unsave(1);
	return cdr(bignum_divide(x, a, b));
}

cell pp_set_car_b(cell x) {
	if (constant_p(cadr(x)))
		return error("set-car!: immutable object", cadr(x));
	caadr(x) = caddr(x);
	return UNSPECIFIC;
}

cell pp_set_cdr_b(cell x) {
	if (constant_p(cadr(x)))
		return error("set-cdr!: immutable object", cadr(x));
	cdadr(x) = caddr(x);
	return UNSPECIFIC;
}

cell pp_set_input_port_b(cell x) {
	Input_port = port_no(cadr(x));
	return UNSPECIFIC;
}

cell pp_set_output_port_b(cell x) {
	Output_port = port_no(cadr(x));
	return UNSPECIFIC;
}

cell _eval(cell x, int cbn);

cell pp_stats(cell x) {
	cell	n;
	int	o_run_stats;

	reset_counter(&Reductions);
	reset_counter(&Allocations);
	reset_counter(&Collections);
	o_run_stats = Run_stats;
	Run_stats = 1;
	gcv(); /* start from a known state */
	n = _eval(cadr(x), 0);
	Run_stats = o_run_stats;
	if (!Error_flag) {
		pr("; ");
		pr(counter_to_string(&Reductions));
		pr(" reductions"); nl();
		pr("; ");
		pr(counter_to_string(&Allocations));
		pr(" nodes allocated"); nl();
		pr("; ");
		pr(counter_to_string(&Collections));
		pr(" garbage collections"); nl();
	}
	return n;
}

cell pp_string_to_list(cell x) {
	char	*s;
	cell	n, a, new;
	int	k, i;

	k = string_len(cadr(x));
	n = NIL;
	a = NIL;
	for (i=0; i<k-1; i++) {
		s = string(cadr(x));
		if (n == NIL) {
			n = a = alloc(make_char(s[i]), NIL);
			save(n);
		}
		else {
			new = alloc(make_char(s[i]), NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	if (n != NIL) unsave(1);
	return n;
}

cell pp_string_to_symbol(cell x) {
	cell	y, n;

	y = find_symbol(string(cadr(x)));
	if (y != NIL) return y;
	/*
	 * Cannot pass name to make_symbol(), because
	 * string(cadr(x)) may move during GC.
	 */
	n = make_symbol("", string_len(cadr(x))-1);
	strcpy(symbol_name(n), string(cadr(x)));
	Symbols = alloc(n, Symbols);
	return car(Symbols);
}

cell pp_string_append(cell x) {
	cell	p, n;
	int	k;
	char	*s;

	k = 0;
	for (p = cdr(x); p != NIL; p = cdr(p)) {
		if (!string_p(car(p)))
			return error("string-append: expected string, got",
					car(p));
		k += string_len(car(p))-1;
	}
	n = make_string("", k);
	s = string(n);
	k = 0;
	for (p = cdr(x); p != NIL; p = cdr(p)) {
		strcpy(&s[k], string(car(p)));
		k += string_len(car(p))-1;
	}
	return n;
}

cell pp_string_copy(cell x) {
	cell	n;

	/*
	 * Cannot pass name to make_string(), because
	 * string(cadr(x)) may move during GC.
	 */
	n = make_string("", string_len(cadr(x))-1);
	strcpy(string(n), string(cadr(x)));
	return n;
}

cell pp_string_fill_b(cell x) {
	int	c = char_value(caddr(x)),
		i, k = string_len(cadr(x))-1;
	char	*s = string(cadr(x));

	if (constant_p(cadr(x)))
		return error("string-fill!: immutable object", cadr(x));
	for (i=0; i<k; i++) s[i] = c;
	return UNSPECIFIC;
}

cell pp_substring(cell x) {
	int	k = string_len(cadr(x))-1;
	int	p0 = integer_value("substring", caddr(x));
	int	pn = integer_value("substring", cadddr(x));
	char	*src = string(cadr(x));
	char	*dst;
	cell	n;

	if (p0 < 0 || p0 > k || pn < 0 || pn > k || pn < p0) {
		n = alloc(cadddr(x), NIL);
		return error("substring: invalid range",
				alloc(caddr(x), n));
	}
	n = make_string("", pn-p0);
	dst = string(n);
	if (pn-p0 != 0) memcpy(dst, &src[p0], pn-p0);
	dst[pn-p0] = 0;
	return n;
}

cell pp_string_length(cell x) {
	return make_integer(string_len(cadr(x))-1);
}

cell pp_string_ref(cell x) {
	int	p, k = string_len(cadr(x))-1;

	p = integer_value("string-ref", caddr(x));
	if (p < 0 || p >= k)
		return error("string-ref: index out of range",
				caddr(x));
	return make_char(string(cadr(x))[p]);
}

cell pp_string_set_b(cell x) {
	int	p, k = string_len(cadr(x))-1;

	if (constant_p(cadr(x)))
		return error("string-set!: immutable object", cadr(x));
	p = integer_value("string-set!", caddr(x));
	if (p < 0 || p >= k)
		return error("string-set!: index out of range",
				caddr(x));
	string(cadr(x))[p] = char_value(cadddr(x));
	return UNSPECIFIC;
}

int string_ci_le(char *s1, char *s2) { return strcmp_ci(s1, s2) <= 0; }
int string_ci_lt(char *s1, char *s2) { return strcmp_ci(s1, s2) <  0; }
int string_ci_eq(char *s1, char *s2) { return strcmp_ci(s1, s2) == 0; }
int string_ci_ge(char *s1, char *s2) { return strcmp_ci(s1, s2) >= 0; }
int string_ci_gt(char *s1, char *s2) { return strcmp_ci(s1, s2) >  0; }

int string_le(char *s1, char *s2) { return strcmp(s1, s2) <= 0; }
int string_lt(char *s1, char *s2) { return strcmp(s1, s2) <  0; }
int string_eq(char *s1, char *s2) { return strcmp(s1, s2) == 0; }
int string_ge(char *s1, char *s2) { return strcmp(s1, s2) >= 0; }
int string_gt(char *s1, char *s2) { return strcmp(s1, s2) >  0; }

cell string_predicate(char *name, int (*p)(char *s1, char *s2), cell x) {
	char	msg[100];

	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!string_p(cadr(x))) {
			sprintf(msg, "%s: expected string, got", name);
			return error(msg, cadr(x));
		}
		if (	(p == string_eq || p == string_ci_eq) &&
			string_len(car(x)) != string_len(cadr(x))
		)
			return FALSE;
		if (!p(string(car(x)), string(cadr(x))))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

#define SP return string_predicate
cell pp_string_ci_le_p(cell x) { SP("string-ci<=?", string_ci_le, x); }
cell pp_string_ci_lt_p(cell x) { SP("string-ci<?",  string_ci_lt, x); }
cell pp_string_ci_eq_p(cell x) { SP("string-ci=?",  string_ci_eq, x); }
cell pp_string_ci_ge_p(cell x) { SP("string-ci>=?", string_ci_ge, x); }
cell pp_string_ci_gt_p(cell x) { SP("string-ci>?",  string_ci_gt, x); }

cell pp_string_le_p(cell x) { SP("string<=?", string_le, x); }
cell pp_string_lt_p(cell x) { SP("string<?",  string_lt, x); }
cell pp_string_eq_p(cell x) { SP("string=?",  string_eq, x); }
cell pp_string_ge_p(cell x) { SP("string>=?", string_ge, x); }
cell pp_string_gt_p(cell x) { SP("string>?",  string_gt, x); }

cell pp_string_p(cell x) {
	return string_p(cadr(x))? TRUE: FALSE;
}

cell pp_symbol_to_string(cell x) {
	cell	n;

	/*
	 * Cannot pass name to make_string(), because
	 * symbol_name(cadr(x)) may move during GC.
	 */
	n = make_string("", symbol_len(cadr(x))-1);
	Tag[n] |= CONST_TAG;
	strcpy(string(n), symbol_name(cadr(x)));
	return n;
}

cell pp_symbol_p(cell x) {
	return symbol_p(cadr(x))? TRUE: FALSE;
}

cell pp_symbols(cell x) {
	cell	n, a, y, new;

	n = NIL;
	a = NIL;
	for (y=Symbols; y != NIL; y = cdr(y)) {
		if (n == NIL) {
			n = a = alloc(car(y), NIL);
			save(n);
		}
		else {
			new = alloc(car(y), NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	if (n != NIL) unsave(1);
	return n;
}

cell pp_times(cell x) {
	cell	a;

	x = cdr(x);
	if (x == NIL) return make_integer(1);
	if (cdr(x) == NIL) return car(x);
	a = make_integer(1);
	save(a);
	while (x != NIL) {
		if (!number_p(car(x))) {
			unsave(1);
			return error("*: expected number, got", car(x));
		}
		a = real_multiply(a, car(x));
		car(Stack) = a;
		x = cdr(x);
	}
	unsave(1);
	return a;
}

cell pp_trace(cell x) {
	cell	n = Trace_list;

	if (cdr(x) == NIL) {
		n = Trace_list;
		Trace_list = NIL;
	}
	if (cddr(x) == NIL && cadr(x) == TRUE) {
		Trace_list = TRUE;
	}
	else {
		if (Trace_list == TRUE) Trace_list = NIL;
		x = cdr(x);
		while (x != NIL) {
			if (!symbol_p(car(x)))
				return error("trace: expected symbol, got",
					car(x));
			Trace_list = alloc(car(x), Trace_list);
			x = cdr(x);
		}
	}
	return n;
}

cell pp_unquote(cell x) {
	return error("unquote: not in quasiquote context", NOEXPR);
}

cell pp_unquote_splicing(cell x) {
	return error("unquote-splicing: not in quasiquote context", NOEXPR);
}

cell pp_vector_to_list(cell x) {
	cell	*v;
	cell	n, a, new;
	int	k, i;

	k = vector_len(cadr(x));
	n = NIL;
	a = NIL;
	for (i=0; i<k; i++) {
		v = vector(cadr(x));
		if (n == NIL) {
			n = a = alloc(v[i], NIL);
			save(n);
		}
		else {
			new = alloc(v[i], NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	if (n != NIL) unsave(1);
	return n;
}

cell pp_vector_fill_b(cell x) {
	cell	fill = caddr(x);
	int	i, k = vector_len(cadr(x));
	cell	*v = vector(cadr(x));

	if (constant_p(cadr(x)))
		return error("vector-fill!: immutable object", cadr(x));
	for (i=0; i<k; i++) v[i] = fill;
	return UNSPECIFIC;
}

cell pp_vector_length(cell x) {
	return make_integer(vector_len(cadr(x)));
}

cell pp_vector_ref(cell x) {
	int	p, k = vector_len(cadr(x));

	p = integer_value("vector-ref", caddr(x));
	if (p < 0 || p >= k)
		return error("vector-ref: index out of range",
				caddr(x));
	return vector(cadr(x))[p];
}

cell pp_vector_set_b(cell x) {
	int	p, k = vector_len(cadr(x));

	if (constant_p(cadr(x)))
		return error("vector-set!: immutable object", cadr(x));
	p = integer_value("vector-set!", caddr(x));
	if (p < 0 || p >= k)
		return error("vector-set!: index out of range",
				caddr(x));
	vector(cadr(x))[p] = cadddr(x);
	return UNSPECIFIC;
}

cell pp_vector_p(cell x) {
	return vector_p(cadr(x))? TRUE: FALSE;
}

cell pp_write(cell x) {
	int	new_port, old_port;

	new_port = cddr(x) == NIL? Output_port: port_no(caddr(x));
	if (new_port < 0 || new_port >= MAX_PORTS)
		return error("write: invalid output port (oops)", caddr(x));
	old_port = Output_port;
	Output_port = new_port;
	print_form(cadr(x));
	Output_port = old_port;
	return UNSPECIFIC;
}

cell pp_write_char(cell x) {
	return pp_display(x);
}

/*----- Evaluator -----*/

PRIM Primitives[] = {
 { "apply",               pp_apply,               2, -1, { PRC,___,___ } },
 { "boolean?",            pp_boolean_p,           1,  1, { ___,___,___ } },
 { "car",                 pp_car,                 1,  1, { PAI,___,___ } },
 { "cdr",                 pp_cdr,                 1,  1, { PAI,___,___ } },
 { "char?",               pp_char_p,              1,  1, { ___,___,___ } },
 { "char->integer",       pp_char_to_integer,     1,  1, { CHR,___,___ } },
 { "char-alphabetic?",    pp_char_alphabetic_p,   1,  1, { CHR,___,___ } },
 { "char-ci<=?",          pp_char_ci_le_p,        2, -1, { CHR,___,___ } },
 { "char-ci<?",           pp_char_ci_lt_p,        2, -1, { CHR,___,___ } },
 { "char-ci=?",           pp_char_ci_eq_p,        2, -1, { CHR,___,___ } },
 { "char-ci>=?",          pp_char_ci_ge_p,        2, -1, { CHR,___,___ } },
 { "char-ci>?",           pp_char_ci_gt_p,        2, -1, { CHR,___,___ } },
 { "char-downcase",       pp_char_downcase,       1,  1, { CHR,___,___ } },
 { "char-lower-case?",    pp_char_lower_case_p,   1,  1, { CHR,___,___ } },
 { "char-numeric?",       pp_char_numeric_p,      1,  1, { CHR,___,___ } },
 { "char-upcase",         pp_char_upcase,         1,  1, { CHR,___,___ } },
 { "char-upper-case?",    pp_char_upper_case_p,   1,  1, { CHR,___,___ } },
 { "char-whitespace?",    pp_char_whitespace_p,   1,  1, { CHR,___,___ } },
 { "char<=?",             pp_char_le_p,           2, -1, { CHR,___,___ } },
 { "char<?",              pp_char_lt_p,           2, -1, { CHR,___,___ } },
 { "char=?",              pp_char_eq_p,           2, -1, { CHR,___,___ } },
 { "char>=?",             pp_char_ge_p,           2, -1, { CHR,___,___ } },
 { "char>?",              pp_char_gt_p,           2, -1, { CHR,___,___ } },
 { "close-input-port",    pp_close_input_port,    1,  1, { INP,___,___ } },
 { "close-output-port",   pp_close_output_port,   1,  1, { OUP,___,___ } },
 { "cons",                pp_cons,                2,  2, { ___,___,___ } },
 { "current-input-port",  pp_current_input_port,  0,  0, { ___,___,___ } },
 { "current-output-port", pp_current_output_port, 0,  0, { ___,___,___ } },
 { "delete-file",         pp_delete_file,         1,  1, { STR,___,___ } },
 { "display",             pp_display,             1,  2, { ___,OUP,___ } },
 { "dump-image",          pp_dump_image,          1,  1, { STR,___,___ } },
 { "/",                   pp_divide,              1, -1, { REA,___,___ } },
 { "eof-object?",         pp_eof_object_p,        1,  1, { ___,___,___ } },
 { "eq?",                 pp_eq_p,                2,  2, { ___,___,___ } },
 { "=",                   pp_equal,               2, -1, { REA,___,___ } },
 { "error",               pp_error,               1,  2, { STR,___,___ } },
 { "exact->inexact",      pp_exact_to_inexact,    1,  1, { REA,___,___ } },
 { "exact?",              pp_exact_p,             1,  1, { REA,___,___ } },
 { "expand-macro",        pp_expand_macro,        1,  1, { ___,___,___ } },
 { "exponent",            pp_exponent,            1,  1, { REA,___,___ } },
 { "file-exists?",        pp_file_exists_p,       1,  1, { STR,___,___ } },
 { "floor",               pp_floor,               1,  1, { REA,___,___ } },
 { "gensym",              pp_gensym,              0,  1, { STR,___,___ } },
 { ">",                   pp_greater,             2, -1, { REA,___,___ } },
 { ">=",                  pp_greater_equal,       2, -1, { REA,___,___ } },
 { "inexact->exact",      pp_inexact_to_exact,    1,  1, { REA,___,___ } },
 { "inexact?",            pp_inexact_p,           1,  1, { REA,___,___ } },
 { "input-port?",         pp_input_port_p,        1,  1, { ___,___,___ } },
 { "integer?",            pp_integer_p,           1,  1, { ___,___,___ } },
 { "integer->char",       pp_integer_to_char,     1,  1, { INT,___,___ } },
 { "<",                   pp_less,                2, -1, { REA,___,___ } },
 { "<=",                  pp_less_equal,          2, -1, { REA,___,___ } },
 { "list->string",        pp_list_to_string,      1,  1, { LST,___,___ } },
 { "list->vector",        pp_list_to_vector,      1,  1, { LST,___,___ } },
 { "load",                pp_load,                1,  1, { STR,___,___ } },
 { "make-string",         pp_make_string,         1,  2, { INT,___,___ } },
 { "make-vector",         pp_make_vector,         1,  2, { INT,___,___ } },
 { "mantissa",            pp_mantissa,            1,  1, { REA,___,___ } },
 { "-",                   pp_minus,               1, -1, { REA,___,___ } },
 { "open-input-file",     pp_open_input_file,     1,  1, { STR,___,___ } },
 { "open-output-file",    pp_open_output_file,    1,  1, { STR,___,___ } },
 { "output-port?",        pp_output_port_p,       1,  1, { ___,___,___ } },
 { "pair?",               pp_pair_p,              1,  1, { ___,___,___ } },
 { "peek-char",           pp_peek_char,           0,  1, { INP,___,___ } },
 { "+",                   pp_plus,                0, -1, { REA,___,___ } },
 { "procedure?",          pp_procedure_p,         1,  1, { ___,___,___ } },
 { "quotient",            pp_quotient,            2,  2, { INT,INT,___ } },
 { "read",                pp_read,                0,  1, { INP,___,___ } },
 { "read-char",           pp_read_char,           0,  1, { INP,___,___ } },
 { "real?",               pp_real_p,              0,  1, { ___,___,___ } },
 { "remainder",           pp_remainder,           2,  2, { INT,INT,___ } },
 { "set-car!",            pp_set_car_b,           2,  2, { PAI,___,___ } },
 { "set-cdr!",            pp_set_cdr_b,           2,  2, { PAI,___,___ } },
 { "set-input-port!",     pp_set_input_port_b,    1,  1, { INP,___,___ } },
 { "set-output-port!",    pp_set_output_port_b,   1,  1, { OUP,___,___ } },
 { "stats",               pp_stats,               1,  1, { ___,___,___ } },
 { "string->list",        pp_string_to_list,      1,  1, { STR,___,___ } },
 { "string->symbol",      pp_string_to_symbol,    1,  1, { STR,___,___ } },
 { "string-append",       pp_string_append,       0, -1, { STR,___,___ } },
 { "string-copy",         pp_string_copy,         1,  1, { STR,___,___ } },
 { "string-fill!",        pp_string_fill_b,       2,  2, { STR,CHR,___ } },
 { "string-length",       pp_string_length,       1,  1, { STR,___,___ } },
 { "string-ref",          pp_string_ref,          2,  2, { STR,INT,___ } },
 { "string-set!",         pp_string_set_b,        3,  3, { STR,INT,CHR } },
 { "string-ci<=?",        pp_string_ci_le_p,      2, -1, { STR,___,___ } },
 { "string-ci<?",         pp_string_ci_lt_p,      2, -1, { STR,___,___ } },
 { "string-ci=?",         pp_string_ci_eq_p,      2, -1, { STR,___,___ } },
 { "string-ci>=?",        pp_string_ci_ge_p,      2, -1, { STR,___,___ } },
 { "string-ci>?",         pp_string_ci_gt_p,      2, -1, { STR,___,___ } },
 { "string<=?",           pp_string_le_p,         2, -1, { STR,___,___ } },
 { "string<?",            pp_string_lt_p,         2, -1, { STR,___,___ } },
 { "string=?",            pp_string_eq_p,         2, -1, { STR,___,___ } },
 { "string>=?",           pp_string_ge_p,         2, -1, { STR,___,___ } },
 { "string>?",            pp_string_gt_p,         2, -1, { STR,___,___ } },
 { "string?",             pp_string_p,            1,  1, { ___,___,___ } },
 { "substring",           pp_substring,           3,  3, { STR,INT,INT } },
 { "symbol?",             pp_symbol_p,            1,  1, { ___,___,___ } },
 { "symbol->string",      pp_symbol_to_string,    1,  1, { SYM,___,___ } },
 { "symbols",             pp_symbols,             0,  0, { ___,___,___ } },
 { "*",                   pp_times,               0, -1, { REA,___,___ } },
 { "trace",               pp_trace,               0, -1, { ___,___,___ } },
 { "unquote",             pp_unquote,             1,  1, { ___,___,___ } },
 { "unquote-splicing",    pp_unquote_splicing,    1,  1, { ___,___,___ } },
 { "vector-fill!",        pp_vector_fill_b,       2,  2, { VEC,___,___ } },
 { "vector-length",       pp_vector_length,       1,  1, { VEC,___,___ } },
 { "vector-set!",         pp_vector_set_b,        3,  3, { VEC,INT,___ } },
 { "vector-ref",          pp_vector_ref,          2,  2, { VEC,INT,___ } },
 { "vector->list",        pp_vector_to_list,      1,  1, { VEC,___,___ } },
 { "vector?",             pp_vector_p,            1,  1, { ___,___,___ } },
 { "write",               pp_write,               1,  2, { ___,OUP,___ } },
 { "write-char",          pp_write_char,          1,  2, { CHR,OUP,___ } },
 { NULL }
};

cell expected(cell who, char *what, cell got) {
	char	msg[100];
	PRIM	*p;

	p = (PRIM *) cadr(who);
	sprintf(msg, "%s: expected %s, got", p->name, what);
	return error(msg, got);
}

cell primitive(cell x) {
	PRIM	*p;
	cell	a;
	int	k, na, i;

	p = (PRIM *) cadar(x);
	k = length(x);
	if (k-1 < p->min_args)
		return too_few_args(x);
	if (k-1 > p->max_args && p->max_args >= 0)
		return too_many_args(x);
	a = cdr(x);
	na = p->max_args < 0? p->min_args: p->max_args;
	if (na > k-1) na = k-1;
	for (i=1; i<=na; i++) {
		switch (p->arg_types[i-1]) {
		case T_NONE:
			break;
		case T_BOOLEAN:
			if (!boolean_p(car(a)))
				return expected(car(x), "boolean", car(a));
			break;
		case T_CHAR:
			if (!char_p(car(a)))
				return expected(car(x), "char", car(a));
			break;
		case T_INPUT_PORT:
			if (!input_port_p(car(a)))
				return expected(car(x), "input-port", car(a));
			break;
		case T_INTEGER:
		case T_REAL:
			if (!number_p(car(a)))
				return expected(car(x), "number", car(a));
			break;
		case T_OUTPUT_PORT:
			if (!output_port_p(car(a)))
				return expected(car(x), "output-port", car(a));
			break;
		case T_PAIR:
			if (atom_p(car(a)))
				return expected(car(x), "pair", car(a));
			break;
		case T_PAIR_OR_NIL:
			if (car(a) != NIL && atom_p(car(a)))
				return expected(car(x), "pair or ()", car(a));
			break;
		case T_PROCEDURE:
			if (	!procedure_p(car(a)) &&
				!primitive_p(car(a))
			)
				return expected(car(x), "procedure", car(a));
			break;
		case T_STRING:
			if (!string_p(car(a)))
				return expected(car(x), "string", car(a));
			break;
		case T_SYMBOL:
			if (!symbol_p(car(a)))
				return expected(car(x), "symbol", car(a));
			break;
		case T_VECTOR:
			if (!vector_p(car(a)))
				return expected(car(x), "vector", car(a));
			break;
		}
		a = cdr(a);
	}
	return (*p->handler)(x);
}

/* Return (#<procedure> (quote #f)) or () */
cell make_application(char *proc_name) {
	cell	proc_sym;
	cell	app, p;

	proc_sym = find_symbol(proc_name);
	if (proc_sym == NIL) return NIL;
	p = lookup(proc_sym, Environment);
	if (p == NIL) return NIL;
	p = cadr(p);
	if (syntax_p(p)) p = cdr(p);
	app = alloc(FALSE, NIL);
	app = alloc(S_quote, app);
	app = alloc(app, NIL);
	return alloc(p, app);
}

int uses_transformer_p(cell x) {
	cell	y;

	if (atom_p(x)) return 0;
	if (car(x) == S_quote) return 0;
	if (pair_p(x) && symbol_p(car(x))) {
		y = lookup(car(x), Environment);
		if (y != NIL && syntax_p(cadr(y))) return 1;
	}
	while (!atom_p(x)) {
		if (uses_transformer_p(car(x))) return 1;
		x = cdr(x);
	}
	return 0;
}

cell expand_all_syntax(cell x) {
	cell	y, m, n, a, app;

	if (Error_flag) return x;
	if (atom_p(x)) return x;
	if (car(x) == S_quote) return x;
	if (symbol_p(car(x))) {
		y = lookup(car(x), Environment);
		if (y != NIL && syntax_p(cadr(y))) {
			save(x);
			app = alloc(cdadr(y), cdr(x));
			unsave(1);
			return _eval(app, 1);
		}
	}
	/*
	 * If DEFINE-MACRO is followed by (MACRO-NAME ...)
	 * unbind the MACRO-NAME first.
	 */
	if (	car(x) == S_define_macro &&
		cdr(x) != NIL &&
		!atom_p(cadr(x))
	) {
		m = lookup(caadr(x), Environment);
		if (m != NIL) cadr(m) = UNDEFINED;
	}
	n = a = NIL;
	save(n);
	while (!atom_p(x)) {
		m = alloc(expand_all_syntax(car(x)), NIL);
		if (n == NIL) {
			n = m;
			car(Stack) = n;
			a = n;
		}
		else {
			cdr(a) = m;
			a = cdr(a);
		}
		x = cdr(x);
	}
	cdr(a) = x;
	unsave(1);
	return n;
}

cell expand_syntax(cell x) {
	if (Error_flag) return x;
	if (atom_p(x)) return x;
	if (car(x) == S_quote) return x;
	save(x);
	while (!Error_flag) {
		if (!uses_transformer_p(x)) break;
		x = expand_all_syntax(x);
		car(Stack) = x;
	}
	unsave(1);
	return x;
}

cell restore_state(void) {
	cell	v;

	if (State_stack == NIL) fatal("restore_state: stack underflow");
	v = car(State_stack);
	State_stack = cdr(State_stack);
	return v;
}

cell bind_arguments(cell n, cell name) {
	cell	p, v, a, e;
	cell	rib;

	save(Environment);
	p = car(n);
	v = cadr(p);
	e = cdddr(p);
	a = cdr(n);
	if (e != NIL) Environment = e;
	rib = NIL;
	save(rib);
	while (!atom_p(v)) {
		if (atom_p(a)) {
			unsave(1);
			return too_few_args(n);
		}
		Tmp = alloc(car(a), NIL);
		Tmp = alloc(car(v), Tmp);
		rib = alloc(Tmp, rib);
		car(Stack) = rib;
		v = cdr(v);
		a = cdr(a);
	}
	if (symbol_p(v)) {
		Tmp = alloc(a, NIL);
		Tmp = alloc(v, Tmp);
		rib = alloc(Tmp, rib);
		car(Stack) = rib;
	}
	else if (a != NIL) {
		unsave(1);
		return too_many_args(n);
	}
	Tmp = NIL;
	unsave(1);
	Environment = make_env(rib, Environment);
	return UNSPECIFIC;
}

void tail_call(void) {
	if (State_stack == NIL || car(State_stack) != EV_BETA) return;
	Tmp = unsave(1);
	Environment = car(Stack);
	unsave(2);
	restore_state();
	save(Tmp);
	Tmp = NIL;
}

cell apply_special(cell x, int *pc, int *ps) {
	cell	sf;

	sf = car(x);
	if (sf == S_quote) return sf_quote(x);
	else if (sf == S_if) return sf_if(x, pc, ps);
	else if (sf == S_cond) return sf_cond(x, pc, ps);
	else if (sf == S_and) return sf_and(x, pc, ps);
	else if (sf == S_or) return sf_or(x, pc, ps);
	else if (sf == S_lambda) return sf_lambda(x);
	else if (sf == S_begin) return sf_begin(x, pc, ps);
	else if (sf == S_set_b) return sf_set_b(x, pc, ps);
	else if (sf == S_define) return sf_define(x, pc, ps);
	else if (sf == S_define_macro) return sf_define_macro(x, pc, ps);
	else fatal("internal: unknown special form");
	return UNSPECIFIC;
}

void make_dynamic(cell x) {
	if (procedure_p(x))
		cdddr(x) = NIL; /* clear lexical env. */
}

int memqp(cell x, cell a) {
	while (a != NIL) {
		if (car(a) == x) return 1;
		a = cdr(a);
	}
	return 0;
}

void trace(cell name, cell expr) {
	if (Error_flag) return;
	if (Trace_list == TRUE || memqp(name, Trace_list)) {
		pr("+ ");
		print_form(alloc(name, cdr(expr)));
		nl();
	}
}

cell _eval(cell x, int cbn) {
	cell	m2,	/* Root of result list */
		a,	/* Used to append to result */
		rib;	/* Temp storage for args */
	int	s,	/* Current state */
		c;	/* Continuation */
	cell	name;	/* Name of procedure to apply */
	char	cond_err[] = "cond: invalid syntax";

	save(x);
	save(State_stack);
	save(Stack_bottom);
	Stack_bottom = Stack;
	s = EV_ATOM;
	c = 0;
	while (!Error_flag) {
		if (Run_stats) count(&Reductions);
		if (symbol_p(x)) {		/* Symbol -> Value */
			if (cbn) {
				Acc = x;
				cbn = 0;
			}
			else {
				Acc = value_of(x, Environment);
				if (Error_flag) break;
			}
		}
		else if (auto_quoting_p(x) || cbn == 2) {
			Acc = x;
			cbn = 0;
		}
		else {				/* (...) -> Value */
			/*
			 * This block is used to DESCEND into lists.
			 * The following structure is saved on the
			 * Stack: RIB = (args append result source)
			 * The current s is saved on the State_stack.
			 */
			Acc = x;
			x = car(x);
			save_state(s);
			/* Check call-by-name built-ins and flag */
			if (special_p(x) || cbn) {
				cbn = 0;
				rib = alloc(Acc, NIL);	/* source */
				rib = alloc(Acc, rib);	/* result */
				rib = alloc(NIL, rib);	/* append */
				rib = alloc(NIL, rib);	/* args */
				x = NIL;
			}
			else {
				Tmp = alloc(NIL, NIL);
				rib = alloc(Acc, NIL);	/* source */
				rib = alloc(Tmp, rib);	/* result */
				rib = alloc(Tmp, rib);	/* append */
				rib = alloc(cdr(Acc), rib); /* args */
				Tmp = NIL;
				x = car(Acc);
			}
			save(rib);
			s = EV_ARGS;
			continue;
		}
		/*
		 * The following loop is used to ASCEND back to the
		 * root of a list, thereby performing BETA REDUCTION.
		 */
		while (!Error_flag) if (s == EV_BETA) {
			/* Finish BETA reduction */
			Environment = unsave(1);
			unsave(1);		/* source expression */
			s = restore_state();
		}
		else if (s == EV_ARGS) {	/* Append to list, reduce */
			rib = car(Stack);
			x = rib_args(rib);
			a = rib_append(rib);
			m2 = rib_result(rib);
			/* Append new member */
			if (a != NIL) car(a) = Acc;
			if (x == NIL) {	/* End of list */
				Acc = m2;
				/* Remember name of caller */
				name = car(rib_source(rib));
				/* Save result (new source expression) */
				car(Stack) = Acc;
				if (Trace_list != NIL) trace(name, Acc);
				if (primitive_p(car(Acc))) {
					if ((PRIM *) cadar(Acc) == Apply_magic)
						c = cbn = 1;
					Acc = x = primitive(Acc);
				}
				else if (special_p(car(Acc))) {
					Acc = x = apply_special(Acc, &c, &s);
				}
				else if (procedure_p(car(Acc))) {
					name = symbol_p(name)? name: NIL;
					Called_procedures[Proc_ptr] = name;
					Proc_ptr++;
					if (Proc_ptr >= MAX_CALL_TRACE)
						Proc_ptr = 0;
					tail_call();
					bind_arguments(Acc, name);
					x = caddar(Acc);
					c = 2;
					s = EV_BETA;
				}
				else {
					error("application of non-procedure",
						name);
					x = NIL;
				}
				if (c != 2) {
					unsave(1); /* drop source expr */
					s = restore_state();
				}
				/* Leave the ASCENDING loop and descend */
				/* once more into X. */
				if (c) break;
			}
			else if (atom_p(x)) {
				error("improper list in application", x);
				x = NIL;
				break;
			}
			else {		/* X =/= NIL: append to list */
				/* Create space for next argument */
				Acc = alloc(NIL, NIL);
				cdr(a) = Acc;
				rib_append(rib) = cdr(a);
				rib_args(rib) = cdr(x);
				x = car(x);	/* Evaluate next member */
				break;
			}
		}
		else if (s == EV_IF_PRED) {
			x = unsave(1);
			unsave(1);	/* source expression */
			s = restore_state();
			if (Acc != FALSE)
				x = cadr(x);
			else
				x = caddr(x);
			c = 1;
			break;
		}
		else if (s == EV_AND || s == EV_OR) {
			car(Stack) = cdar(Stack);
			if (	(Acc == FALSE && s == EV_AND) ||
				(Acc != FALSE && s == EV_OR) ||
				car(Stack) == NIL
			) {
				unsave(2);	/* state, source expr */
				s = restore_state();
				x = Acc;
				cbn = 2;
			}
			else if (cdar(Stack) == NIL) {
				x = caar(Stack);
				unsave(2);	/* state, source expr */
				s = restore_state();
			}
			else {
				x = caar(Stack);
			}
			c = 1;
			break;
		}
		else if (s == EV_COND) {
			if (Acc != FALSE) {
				x = cdar(car(Stack));
				if (atom_p(x)) {
					x = Acc;
				}
				else if (!atom_p(cdr(x))) {
					if (car(x) == S_arrow) {
						if (cddr(x) != NIL)
							error(cond_err, x);
						Acc = quote(Acc, S_quote);
						Acc = alloc(Acc, NIL);
						Acc = x = alloc(cadr(x), Acc);
					}
					else {
						Acc = x = alloc(S_begin, x);
					}
				}
				else {
					x = car(x);
				}
				unsave(2);	/* state, source expr */
				s = restore_state();
			}
			else if (cdar(Stack) == NIL)  {
				unsave(2);	/* state, source expr */
				s = restore_state();
				x = UNSPECIFIC;
			}
			else {
				car(Stack) = cdar(Stack);
				x = caaar(Stack);
				if (x == S_else && cdar(Stack) == NIL)
					x = TRUE;
			}
			c = 1;
			break;
		}
		else if (s == EV_BEGIN) {
			car(Stack) = cdar(Stack);
			if (cdar(Stack) == NIL) {
				x = caar(Stack);
				unsave(2);	/* state, source expr */
				s = restore_state();
			}
			else {
				x = caar(Stack);
			}
			c = 1;
			break;
		}
		else if (s == EV_SET_VAL || s == EV_DEFINE || s == EV_MACRO) {
			char *err = "define-macro: expected procedure, got";

			if (s == EV_DEFINE) make_dynamic(Acc);
			if (s == EV_MACRO) {
				if (procedure_p(Acc)) {
					Acc = alloc_atom(T_SYNTAX, Acc);
				}
				if (syntax_p(Acc)) {
					/* Acc = Acc; */
				}
				else {
					error(err, Acc);
					break;
				}
			}
			x = unsave(1);
			unsave(1);	/* source expression */
			s = restore_state();
			car(x) = Acc;
			Acc = x = UNSPECIFIC;
			c = 0;
			break;
		}
		else { /* s == EV_ATOM */
			break;
		}
		if (c) {	/* Continue evaluation if requested */
			c = 0;
			continue;
		}
		if (Stack == Stack_bottom) break;
	}
	while (Stack != Stack_bottom) unsave(1);
	Stack_bottom = unsave(1);
	State_stack = unsave(1);
	unsave(1);
	return Acc;		/* Return the evaluated expr */
}

void reset_calltrace(void) {
	int	i;

	for (i=0; i<MAX_CALL_TRACE; i++)
		Called_procedures[i] = NIL;
}

cell eval(cell x) {
	reset_calltrace();
	save(x);
	x = expand_syntax(x);
	unsave(1);
	x = _eval(x, 0);
	return x;
}

/*----- REPL -----*/

void clear_local_envs(void) {
	while (cdr(Environment) != NIL)
		Environment = cdr(Environment);
}

#ifndef NO_SIGNALS
void keyboard_interrupt(int sig) {
	error("interrupted", NOEXPR);
	signal(SIGINT, keyboard_interrupt);
}

void keyboard_quit(int sig) {
	fatal("received quit signal, exiting");
}
#endif

void repl(void) {
	cell	n = NIL; /*LINT*/
	cell	sane_env;

	sane_env = alloc(NIL, NIL);
	save(sane_env);
	if (!Quiet_mode) {
		signal(SIGQUIT, keyboard_quit);
		signal(SIGINT, keyboard_interrupt);
	}
	while (1) {
		Error_flag = 0;
		Input_port = 0;
		Output_port = 1;
		clear_local_envs();
		reset_calltrace();
		car(sane_env) = Environment;
		if (!Quiet_mode) {
			pr("> "); flush();
		}
		Program = xread();
		if (Program == ENDOFFILE) break;
		if (!Error_flag) n = eval(Program);
		if (!Error_flag && !unspecific_p(n)) {
			print_form(n);
			pr("\n");
			car(S_latest) = n;
		}
		if (Error_flag) Environment = car(sane_env);
	}
	unsave(1);
	pr("\n");
}

/*----- Startup and Initialization -----*/

/* Variables to dump to image file */
cell *Image_vars[] = {
	&Free_list, &Free_vecs, &Symbols, &Environment,
	&S_arrow, &S_else, &S_extensions, &S_latest,
	&S_library_path, &S_loading, &S_quasiquote,
	&S_quote, &S_unquote, &S_unquote_splicing,
	&S_and, &S_begin, &S_cond, &S_define, &S_define_macro,
	&S_if, &S_lambda, &S_or, &S_quote, &S_set_b,
NULL };

struct magic {
	char	id[2];
	char	version[10];
	char	cell_size[1];
	char	mantissa_size[1];
	char	_pad[2];
	char	byte_order[8];
	char	binary_id[8];
};

void dump_image(char *p) {
	FILE		*f;
	cell		n, **v;
	int		i, k;
	struct magic	m;
	char		buf[100];

	f = fopen(p, "wb");
	if (f == NULL) {
		error("cannot create image file",
			make_string(p, (int) strlen(p)));
		return;
	}
	memset(&m, '_', sizeof(m));
	strncpy(m.id, "S9", sizeof(m.id));
	strncpy(m.version, VERSION, sizeof(m.version));
	m.cell_size[0] = sizeof(cell)+'0';
	m.mantissa_size[0] = MANTISSA_SEGMENTS+'0';
	n = 0x30313233L;
	memcpy(m.byte_order, &n, sizeof(n)>8? 8: sizeof(n));
	n = (cell) &Primitives;
	memcpy(m.binary_id, &n, sizeof(n)>8? 8: sizeof(n));
	fwrite(&m, sizeof(m), 1, f);
	i = Pool_size;
	fwrite(&i, sizeof(int), 1, f);
	i = Vpool_size;
	fwrite(&i, sizeof(int), 1, f);
	v = Image_vars;
	i = 0;
	while (v[i]) {
		fwrite(v[i], sizeof(cell), 1, f);
		i++;
	}
	if (	fwrite(Car, 1, Pool_size*sizeof(cell), f)
		 != Pool_size*sizeof(cell) ||
		fwrite(Cdr, 1, Pool_size*sizeof(cell), f)
		 != Pool_size*sizeof(cell) ||
		fwrite(Tag, 1, Pool_size, f) != Pool_size ||
		fwrite(Vectors, 1, Vpool_size*sizeof(cell), f)
		 != Vpool_size*sizeof(cell)
	) {
		fclose(f);
		error("image dump failed", NOEXPR);
		return;
	}
	fclose(f);
	k = gc();
	if (!Quiet_mode) {
		sprintf(buf, "image dumped: %d nodes used, %d free",
				Pool_size-k, k);
		pr(buf); nl();
	}
}

int load_image(char *p) {
	FILE		*f;
	cell		n, **v;
	int		i;
	struct magic	m;
	int		bad = 0;
	int		inodes, ivcells;
	cell		name;

	name = make_string(p, (int) strlen(p));
	f = fopen(p, "rb");
	if (f == NULL) return -1;
	fread(&m, sizeof(m), 1, f);
	if (memcmp(m.id, "S9", 2)) {
		error("error in image file (magic match failed)", name);
		bad = 1;
	}
	if (memcmp(m.version, VERSION, 10)) {
		error("error in image file (wrong version)", name);
		bad = 1;
	}
	if (m.cell_size[0]-'0' != sizeof(cell)) {
		error("error in image file (wrong cell size)", name);
		bad = 1;
	}
	if (m.mantissa_size[0]-'0' != MANTISSA_SEGMENTS) {
		error("error in image file (wrong mantissa size)", name);
		bad = 1;
	}
	memcpy(&n, m.byte_order, sizeof(cell));
	if (n != 0x30313233L) {
		error("error in image file (wrong architecture)", name);
		bad = 1;
	}
	memcpy(&n, m.binary_id, sizeof(cell));
	if (n != (cell) &Primitives) {
		error("error in image file (wrong interpreter)", name);
		bad = 1;
	}
	memset(Tag, 0, Pool_size);
	fread(&inodes, sizeof(int), 1, f);
	fread(&ivcells, sizeof(int), 1, f);
	while (inodes > Pool_size || ivcells > Vpool_size) {
		if (	Memory_limit_kn &&
			Pool_size + Segment_size > Memory_limit_kn
		) {
			error("image too big", NOEXPR);
			bad = 1;
			break;
		}
		new_segment();
	}
	v = Image_vars;
	i = 0;
	while (v[i]) {
		fread(v[i], sizeof(cell), 1, f);
		i++;
	}
	if (	!bad &&
		(fread(Car, 1, inodes*sizeof(cell), f)
		  != inodes*sizeof(cell) ||
		 fread(Cdr, 1, inodes*sizeof(cell), f)
		  != inodes*sizeof(cell) ||
		 fread(Tag, 1, inodes, f) != inodes ||
		 fread(Vectors, 1, ivcells*sizeof(cell), f)
		  != ivcells*sizeof(cell) ||
		 fgetc(f) != EOF)
	) {
		error("error in image file (bad size)", NOEXPR);
		bad = 1;
	}
	fclose(f);
	if (Error_flag) fatal("unusable image");
	return 0;
}

cell get_library_path(void) {
	char	*s;

	s = getenv("S9FES_LIBRARY_PATH");
	if (s == NULL) s = DEFAULT_LIBRARY_PATH;
	return make_string(s, (int) strlen(s));
}

void load_library(void) {
	char	*path, buf[256], *p;
	char	libdir[240], libfile[256];
	char	*home;
	cell	new;

	path = copy_string(string(car(S_library_path)));
	home = getenv("HOME");
	if (home == NULL) home = ".";
	p = strtok(path, ":");
	while (p != NULL) {
		if (p[0] == '~') {
			if (strlen(p) + strlen(home) >= sizeof(libdir)-1)
				fatal("load_library: path too long");
			sprintf(libdir, "%s%s", home, &p[1]);
		}
		else {
			if (strlen(p) >= sizeof(libdir)-1)
				fatal("load_library: path too long");
			strcpy(libdir, p);
		}
		if (strlen(IMAGEFILE) + strlen(libdir) >= sizeof(libfile)-1)
			fatal("load_library: path too long");
		sprintf(libfile, "%s/%s", libdir, IMAGEFILE);
		if (load_image(libfile) == 0) {
			free(path);
			/* *library-path* is overwritten by load_image() */
			new = get_library_path();
			car(S_library_path) = new;
			return;
		}
		if (strlen(LIBRARY) + strlen(libdir) >= sizeof(libfile)-1)
			fatal("load_library: path too long");
		sprintf(libfile, "%s/%s", libdir, LIBRARY);
		if (load(libfile) == 0) {
			free(path);
			return;
		}
		p = strtok(NULL, ":");
	}
	sprintf(buf, "Found neither \"%s\" nor \"%s\"", IMAGEFILE, LIBRARY);
	fatal(buf);
}

void load_rc(void) {
	char	rcpath[256];
	char	rcfile[] = "/.s9fes/rc";
	char	*home;

	home = getenv("HOME");
	if (home == NULL) return;
	if (strlen(home) + strlen(rcfile) + 1 >= sizeof(rcpath)-1)
		fatal("path too long in HOME");
	sprintf(rcpath, "%s/%s", home, rcfile);
	load(rcpath);
}

void add_primitives(char *name, PRIM *p) {
	cell	v, n, new;
	int	i;

	if (name) {
		n = add_symbol(name);
		new = alloc(n, car(S_extensions));
		car(S_extensions) = new;
	}
	for (i=0; p[i].name; i++) {
		if (Apply_magic == NULL && !strcmp(p[i].name, "apply"))
			Apply_magic = &p[i];
		v = add_symbol(p[i].name);
		n = alloc_atom((cell) &p[i], NIL);
		n = alloc_atom(T_PRIMITIVE, n);
		Environment = extend(v, n, Environment);
	}
}

/* Extension prototypes */
void sc_init(void);
void unix_init(void);

void make_initial_env(void) {
	cell	new;

	Environment = alloc(NIL, NIL);
	Environment = extend(add_symbol("**"), NIL, Environment);
	S_latest = cdadr(Environment);
	Environment = extend(add_symbol("*extensions*"), NIL, Environment);
	S_extensions = cdadr(Environment);
	Environment = extend(add_symbol("*library-path*"), NIL, Environment);
	S_library_path = cdadr(Environment);
	new = get_library_path();
	car(S_library_path) = new;
	Environment = extend(add_symbol("*loading*"), FALSE, Environment);
	S_loading = cdadr(Environment);
	Apply_magic = NULL;
	add_primitives(NULL, Primitives);
	EXTENSIONS;
	Environment = alloc(Environment, NIL);
}

void init(void) {
	int	i;

	for (i=2; i<MAX_PORTS; i++) Ports[i] = NULL;
	Ports[0] = stdin;
	Ports[1] = stdout;
	Port_flags[0] = LOCK_TAG;
	Port_flags[1] = LOCK_TAG;
	Input_port = 0;
	Output_port = 1;
	Segment_size = INITIAL_SEGMENT_SIZE;
	Pool_size = 0,
	Vpool_size = 0;
	Car = NULL,
	Cdr = NULL;
	Tag = NULL;
	Free_list = NIL;
	Vectors = NULL;
	Free_vecs = 0;
	Memory_limit_kn = DEFAULT_LIMIT_KN * 1024L;
	Stack = NIL,
	Stack_bottom = NIL;
	State_stack = NIL;
	Tmp_car = NIL;
	Tmp_cdr = NIL;
	Tmp = NIL;
	Symbols = NIL;
	Program = NIL;
	Proc_ptr = 0;
	Environment = NIL;
	Acc = NIL;
	Input_port = 0;
	Output_port = 1;
	Trace_list = NIL;
	Level = 0;
	Line_no = 1;
	Error_flag = 0;
	Load_level = 0;
	Displaying = 0;
	Quiet_mode = 0;
	Command_line = NULL;
	Run_stats = 0;
	new_segment();
	gc();
	S_arrow = add_symbol("=>");
	S_and = add_symbol("and");
	S_begin = add_symbol("begin");
	S_cond = add_symbol("cond");
	S_define = add_symbol("define");
	S_define_macro = add_symbol("define-macro");
	S_else = add_symbol("else");
	S_if = add_symbol("if");
	S_lambda = add_symbol("lambda");
	S_or = add_symbol("or");
	S_quasiquote = add_symbol("quasiquote");
	S_quote = add_symbol("quote");
	S_set_b = add_symbol("set!");
	S_unquote = add_symbol("unquote");
	S_unquote_splicing = add_symbol("unquote-splicing");
	make_initial_env();
	Program = TRUE;
	rehash(car(Environment));
	reset_calltrace();
}

void init_extensions(void) {
	cell	e, n;
	char	initproc[TOKEN_LENGTH+2];
	char	*s;
	char	*s9 = "s9";

	e = car(S_extensions);
	while (s9 || e != NIL) {
		s = s9? s9: string(car(e));
		if (strlen(s)*2+1 >= TOKEN_LENGTH)
			fatal("init_extension(): procedure name too long");
		sprintf(initproc, "%s:%s", s, s);
		n = find_symbol(initproc);
		if (n != NIL) {
			n = alloc(n, NIL);
			eval(n);
		}
		e = s9? e: cdr(e);
		s9 = NULL;
	}
}

void usage(char *name, int quit) {
	pr("Usage: ");
	pr(name);
	pr(" [-h?] [-gnqv] [-m size[m]] [-f program] [-d image] [-i]"); nl();
	pr("             [-- argument ...]\n");
	if (quit) exit(1);
}

void long_usage(char *name) {
	nl();
	usage(name, 0);
	nl();
	pr("-?       display this summary (also -h)"); nl();
	pr("-d file  dump heap image to file and exit"); nl();
	pr("-f file  run program and exit (implies -q)"); nl();
	pr("-g       print GC summaries (-gg = more)"); nl();
	pr("-i       enter interactive mode (after -f)"); nl();
	pr("-n       do not load $HOME/.s9fes/rc"); nl();
	pr("-m n[m]  set memory limit to nK (or nM) nodes"); nl();
	pr("-q       be quiet (no banner, no prompt, exit on errors)"); nl();
	pr("-v       print version and exit"); nl();
	pr("-- args  pass following arguments to program"); nl();
	nl();
}

void version_info(void) {
	char	buf[100];

	pr("version:        "); pr(VERSION);
#ifdef unix
	pr(" (unix)");
#else
 #ifdef plan9
	pr(" (plan 9)");
 #else
	pr(" (unknown)");
 #endif
#endif
	nl();
	pr("library image:  "); pr(IMAGEFILE); nl();
	pr("library source: "); pr(LIBRARY); nl();
	pr("library path:   "); pr(string(car(S_library_path))); nl();
	pr("memory limit:   ");
	if (Memory_limit_kn) {
		sprintf(buf, "%ld", Memory_limit_kn / 1024);
		pr(buf); pr("K nodes"); nl();
	}
	else {
		pr("none"); nl();
	}
	pr("extensions:     "); print_form(car(S_extensions)); nl();
}

long get_size_k(char *name, char *s) {
	int	c;
	long	n;

	c = s[strlen(s)-1];
	n = atol(s);
	if (isdigit(c))
		;
	else if (c == 'M' || c == 'm')
		n *= 1024L;
	else
		usage(name, 1);
	return n * 1024;
}

int main(int argc, char **argv) {
	char	*name = *argv;
	int	ignore_rc = 0;
	int	interact = 1;

	init();
	argv++;
	load_library();
	init_extensions();
	while (*argv != NULL) {
		if (**argv != '-') break;
		(*argv)++;
		while (**argv) {
			switch (**argv)  {
			case '-':
				Command_line = ++argv;
				break;
			case 'd':
				if (argv[1] == NULL) usage(name, 1);
				dump_image(argv[1]);
				exit(Error_flag? 1: 0);
				break;
			case 'f':
				if (argv[1] == NULL) usage(name, 1);
				if (!ignore_rc) {
					load_rc();
					ignore_rc = 1;
				}
				if (load(argv[1]))
					error("program file not found",
						make_string(argv[1],
							(int)strlen(argv[1])));
				interact = 0;
				argv++;
				*argv = &(*argv)[strlen(*argv)];
				if (Error_flag) exit(1);
				break;
			case 'g':
				Verbose_GC++;
				(*argv)++;
				break;
			case 'i':
				interact = 1;
				Quiet_mode = 0;
				(*argv)++;
				break;
			case 'n':
				ignore_rc = 1;
				(*argv)++;
				break;
			case 'm':
				if (argv[1] == NULL) usage(name, 1);
				Memory_limit_kn = get_size_k(name, argv[1]);
				argv++;
				*argv += strlen(*argv);
				break;
			case 'q':
				Quiet_mode = 1;
				(*argv)++;
				break;
			case 'v':
				version_info();
				exit(0);
				break;
			case 'h':
			case '?':
				long_usage(name);
				exit(0);
				break;
			default:
				usage(name, 1);
				break;
			}
			if (Command_line) break;
		}
		if (Command_line) break;
		argv++;
	}
	if (!Command_line && argv[0] != NULL) usage(name, 1);
	if (!interact) exit(Error_flag? 1: 0);
	if (!Quiet_mode)
		pr("Scheme 9 from Empty Space by Nils M Holm, 2009\n");
	if (!ignore_rc) load_rc();
	repl();
	return 0;
}
