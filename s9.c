/*
 * Scheme 9 from Empty Space
 * By Nils M Holm <nmh@t3x.org>, 2007,2008,2009
 */

/*
 * Use -DNO_SIGNALS to disable POSIX signal handlers.
 * Use -DBITS_PER_WORD_64 on 64-bit systems.
 */

#define VERSION "2009-05-06"

#define EXTERN
#include "s9.h"
#undef EXTERN

#ifndef EXTENSIONS
 #define EXTENSIONS
#endif

int	Verbose_GC = 0;

PRIM	*Apply_magic;

cell	*GC_root[] = { &Program, &Symbols, &Environment, &Tmp,
			&Tmp_car, &Tmp_cdr, &Stack, &Stack_bottom,
			&State_stack, &Acc, &Trace_list, &File_list,
			NULL };

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
	char	*msg = "statistics counter overflow";

	c->n = c->n+1;
	if (c->n >= 1000) {
		c->n = c->n - 1000;
		c->n1k = c->n1k + 1;
		if (c->n1k >= 1000) {
			c->n1k = 0;
			c->n1m = c->n1m+1;
			if (c->n1m >= 1000) {
				c->n1m = 0;
				c->n1g = c->n1g+1;
				if (c->n1g >= 1000) {
					error(msg, NOEXPR);
				}
			}
		}
	}
}

char *counter_to_string(struct counter *c) {
	static char	buf[16];

	memset(buf, ' ', sizeof(buf)-1);
	buf[sizeof(buf)-1] = 0;
	if (c->n1g) {
		sprintf(buf, "%3d,", c->n1g);
	}
	if (c->n1m || c->n1g) {
		if (c->n1g)
			sprintf(&buf[4], "%03d,", c->n1m);
		else
			sprintf(&buf[4], "%3d,", c->n1m);
	}
	if (c->n1k || c->n1m || c->n1g) {
		if (c->n1g || c->n1m)
			sprintf(&buf[8], "%03d,", c->n1k);
		else
			sprintf(&buf[8], "%3d,", c->n1k);
	}
	if (c->n1g || c->n1m || c->n1k)
		sprintf(&buf[12], "%03d", c->n);
	else
		sprintf(&buf[12], "%3d", c->n);
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
		i += 1;
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
			k = k+1;
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
		 */
		if (k < Pool_size / 10) {
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
		k = k-1;
	}
	return n;
}

/*----- Reader -----*/

cell find_symbol(char *s) {
	cell	y;

	y = Symbols;
	while (y != NIL) {
		if (!strcmp(string(car(y)), s))
			return car(y);
		y = cdr(y);
	}
	return NIL;
}

cell make_symbol(char *s, int k) {
	cell	n;

	n = allocv(T_SYMBOL, k+1);
	strcpy(string(n), s);
	return n;
}

cell add_symbol(char *s) {
	cell	y, new;

	y = find_symbol(s);
	if (y != NIL) return y;
	Symbols = alloc(NIL, Symbols);
	new = make_symbol(s, (int) strlen(s));
	car(Symbols) = new;
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

	Level = Level+1;
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
			Level = Level-1;
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
		c = c+1;
	}
	Level = Level-1;
	if (a != NIL) cdr(a) = NIL;	/* Remove trailing empty node */
	unsave(1);
	return c? m: NIL;
}

cell quote(cell n, cell quotation) {
	cell	q;

	q = alloc(n, NIL);
	return alloc(quotation, q);
}

int string_numeric_p(char *s) {
	int	i;

	i = 0;
	if (s[0] == '+' || s[0] == '-') i = 1;
	if (!s[i]) return 0;
	while (s[i]) {
		if (!isdigit(s[i])) return 0;
		i = i+1;
	}
	return 1;
}

cell string_to_bignum(char *s) {
	cell	n;
	int	k, j, v, sign;

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
		n = alloc3(v, n, ATOM_TAG);
	}
	return alloc3(T_INTEGER, n, ATOM_TAG);
}

/* Create a character literal. */
cell make_char(int x) {
	cell n;

	n = alloc3(x, NIL, ATOM_TAG);
	return alloc3(T_CHAR, n, ATOM_TAG);
}

int strcmp_ci(char *s1, char *s2) {
	int	c1, c2;

	for (;;) {
		c1 = tolower(*s1++);
		c2 = tolower(*s2++);
		if (!c1 || !c2 || c1 != c2) break;
	}
	return c1<c2? -1: c1>c2? 1: 0;
}

/* Read a character literal. */
cell character(void) {
	char	buf[10], msg[50];
	int	i, c;

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
	strcpy(string(n), s);
	return n;
}

/* Clone an existing string.
 * When copying a string to a string, the source string
 * may get relocated during vector pool compaction.
 * Hence this routine must be used instead of make_string().
 */
cell clone_string(cell s, int k) {
	cell	n;

	save(s);
	n = allocv(T_STRING, k+1);
	strcpy(string(n), string(s));
	unsave(1);
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
			i = i-1;
		}
		if (q && c != '"' && c != '\\') {
			s[i++] = '\\';
			inv = 1;
		}
		s[i] = c;
		q = !q && c == '\\';
		if (!q) i = i+1;
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
	 (c) == ';'  || (c) == '#'  || (c) == '\'' || \
	 (c) == '`'  || (c) == ','  || (c) == '"'  || \
	 (c) == EOF)

cell symbol_or_number(int c) {
	char	s[TOKEN_LENGTH];
	int	i;

	i = 0;
	while (!separator(c)) {
		if (i >= TOKEN_LENGTH-2) {
			error("symbol too long", NOEXPR);
			i = i-1;
		}
		s[i] = c;
		i = i+1;
		c = read_c_ci();
	}
	s[i] = 0;
	reject(c);
	if (string_numeric_p(s)) return string_to_bignum(s);
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
		i += 1;
		if (i >= sizeof(buf)-1) fatal("ntoa: number too big");
		p -= 1;
		*p = x % 10 + '0';
		x = x / 10;
	}
	while (i < (w-neg) && i < sizeof(buf)-1) {
		i += 1;
		p -= 1;
		*p = '0';
	}
	if (neg) {
		if (i >= sizeof(buf)-1) fatal("ntoa: number too big");
		p -= 1;
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
	while (1) {
		if (n == NIL) break;
		pr(ntoa(buf, car(n), first? 0: DIGITS_PER_WORD));
		n = cdr(n);
		first = 0;
	}
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
		k = k-1;
	}
	if (!Displaying) pr("\"");
	return 1;
}

int print_symbol(cell n) {
	char	b[2];
	int	k;
	char	*s;

	if (!symbol_p(n)) return 0;
	s = string(n);
	k = string_len(n)-1;
	b[1] = 0;
	while (k) {
		b[0] = *s++;
		pr(b);
		k = k-1;
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
		lastp[0] = NIL;
		return NIL;
	}
	m = alloc(NIL, NIL);
	save(m);
	a = m;
	last = m;
	while (n != NIL) {
		car(a) = car(n);
		last = a;
		n = cdr(n);
		if (n != NIL) {
			new = alloc(NIL, NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	unsave(1);
	lastp[0] = last;
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

int list_of_symbols_p(cell n) {
	return !symbol_p(n) && argument_list_p(n);
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
	v = vector(car(e));
	while (p != NIL) {
		s = string(caar(p));
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

	n = alloc(a, NIL);
	n = alloc(v, n);
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
	s = string(v);
	h = 0;
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

cell make_temporaries(int x) {
	cell	n, v;
	int	k = 0;
	char	buf[20];

	n = NIL;
	save(n);
	while (x != NIL) {
		sprintf(buf, "##%d", k);
		v = add_symbol(buf);
		n = alloc(v, n);
		car(Stack) = n;
		x = cdr(x);
		k++;
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
	return alloc3(T_PROCEDURE, n, ATOM_TAG);
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
	car(Stack) = cdr(v); /* unsave(1); save(cdr(v)); */
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
	car(Stack) = cdr(v); /* unsave(1); save(cdr(v)); */
	*pc = 2;
	*ps = EV_MACRO;
	return a;
}

/*----- Bignums -----*/

cell make_integer(long i) {
	cell	n;

	n = alloc3(i, NIL, ATOM_TAG);
	return alloc3(T_INTEGER, n, ATOM_TAG);
}

int integer_value(char *src, cell x) {
	char	msg[100];

	if (cddr(x) != NIL) {
		sprintf(msg, "%s: integer argument too big", src);
		error(msg, x);
		return 0;
	}
	return cadr(x);
}

cell bignum_abs(cell a) {
	cell	n;

	n = alloc3(abs(cadr(a)), cddr(a), ATOM_TAG);
	return alloc3(T_INTEGER, n, ATOM_TAG);
}

cell bignum_negate(cell a) {
	cell	n;

	n = alloc3(-cadr(a), cddr(a), ATOM_TAG);
	return alloc3(T_INTEGER, n, ATOM_TAG);
}

cell reverse_segments(cell n) {
	cell	m;

	m = NIL;
	while (n != NIL) {
		m = alloc3(car(n), m, ATOM_TAG);
		n = cdr(n);
	}
	return m;
}

cell bignum_add(cell a, cell b);
cell bignum_subtract(cell a, cell b);

cell _bignum_add(cell a, cell b) {
	cell	fa, fb, result;
	long	r;
	int	carry;

	if (bignum_negative_p(a)) {
		if (bignum_negative_p(b)) {
			/* -A+-B --> -(|A|+|B|) */
			a = bignum_abs(a);
			save(a);
			b = bignum_abs(b);
			save(b);
			a = bignum_add(a, b);
			unsave(2);
			return bignum_negate(a);
		}
		else {
			/* -A+B --> B-|A| */
			a = bignum_abs(a);
			save(a);
			a = bignum_subtract(b, a);
			unsave(1);
			return a;
		}
	}
	else if (bignum_negative_p(b)) {
		/* A+-B --> A-|B| */
		b = bignum_abs(b);
		save(b);
		a = bignum_subtract(a, b);
		unsave(1);
		return a;
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
		result = alloc3(r, result, ATOM_TAG);
		car(Stack) = result;
		if (a != NIL) a = cdr(a);
		if (b != NIL) b = cdr(b);
	}
	unsave(3);
	return alloc3(T_INTEGER, result, ATOM_TAG);
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
	cell	fa, fb, result;
	int	borrow;
	long	r;

	if (bignum_negative_p(a)) {
		if (bignum_negative_p(b)) {
			/* -A--B --> -A+|B| --> |B|-|A| */
			a = bignum_abs(a);
			save(a);
			b = bignum_abs(b);
			save(b);
			a = bignum_subtract(b, a);
			unsave(2);
			return a;
		}
		else {
			/* -A-B --> -(|A|+B) */
			a = bignum_abs(a);
			save(a);
			a = bignum_add(a, b);
			unsave(1);
			return bignum_negate(a);
		}
	}
	else if (bignum_negative_p(b)) {
		/* A--B --> A+|B| */
		b = bignum_abs(b);
		save(b);
		a = bignum_add(a, b);
		unsave(1);
		return a;
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
		result = alloc3(r, result, ATOM_TAG);
		car(Stack) = result;
		if (a != NIL) a = cdr(a);
		if (b != NIL) b = cdr(b);
	}
	unsave(3);
	while (car(result) == 0 && cdr(result) != NIL)
		result = cdr(result);
	return alloc3(T_INTEGER, result, ATOM_TAG);
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
	long	r, c;
	int	carry;
	cell	result;

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
		result = alloc3(r, result, ATOM_TAG);
		car(Stack) = result;
		a = cdr(a);
	}
	if (carry) result = alloc3(carry, result, ATOM_TAG);
	unsave(2);
	return alloc3(T_INTEGER, result, ATOM_TAG);
}

/* Result: (a/10 . a%10) */
cell bignum_shift_right(cell a) {
	long	r, c;
	int	carry;
	cell	result;

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
		result = alloc3(r, result, ATOM_TAG);
		car(Stack) = result;
		a = cdr(a);
	}
	result = reverse_segments(result);
	if (car(result) == 0 && cdr(result) != NIL) result = cdr(result);
	result = alloc3(T_INTEGER, result, ATOM_TAG);
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
		c = make_integer(0);
		cadddr(Stack) = c;
		caddr(Stack) = c0 = c;
		i = 0;
		while (!bignum_less_p(a, c)) {
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
	while (1) {
		if (separator(c)) break;
		p = 0;
		while (digits[p] && digits[p] != c) p++;
		if (p >= radix) {
			sprintf(buf, "invalid digit in %s number: %c",
				pre, c);
			return error(buf, NOEXPR);
		}
		num = bignum_multiply(num, base);
		car(Stack) = num;
		num = bignum_add(num, make_integer(p));
		car(Stack) = num;
		nd++;
		c = read_c_ci();
	}
	if (!nd) {
		sprintf(buf, "digits expected after %s", pre);
		return error(buf, NOEXPR);
	}
	unsave(2);
	reject(c);
	return s? bignum_negate(num): num;
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

	n = alloc3(portno, NIL, ATOM_TAG);
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
		if (!integer_p(cadr(x)))
			return error("=: expected integer, got", cadr(x));
		if (!bignum_equal_p(car(x), cadr(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell expand_quasiquote(cell x);
cell expand_syntax(cell x);

cell pp_expand_macro(cell x) {
	x = cadr(x);
	save(x);
	x = expand_quasiquote(x);
	car(Stack) = x;
	x = expand_syntax(x);
	unsave(1);
	return x;
}

cell pp_file_exists_p(cell x) {
	FILE	*f;

	f = fopen(string(cadr(x)), "r");
	if (f == NULL) return FALSE;
	fclose(f);
	return TRUE;
}

cell pp_gensym(cell x) {
	static long	g = 0;
	char		s[200], *pre;

	pre = cdr(x) == NIL? "g": string(cadr(x));
	if (strlen(pre) > 100)
		return error("gensym: prefix too long", cadr(x));
	do {
		sprintf(s, "%s%ld", pre, g);
		g += 1;
	} while (find_symbol(s) != NIL);
	return add_symbol(s);
}

cell pp_greater(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!integer_p(cadr(x)))
			return error(">: expected integer, got", cadr(x));
		if (!bignum_less_p(cadr(x), car(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_greater_equal(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!integer_p(cadr(x)))
			return error(">=: expected integer, got", cadr(x));
		if (bignum_less_p(car(x), cadr(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
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
	return integer_p(cadr(x))? TRUE: FALSE;
}

cell pp_less(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!integer_p(cadr(x)))
			return error("<: expected integer, got", cadr(x));
		if (!bignum_less_p(car(x), cadr(x))) return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_less_equal(cell x) {
	x = cdr(x);
	while (cdr(x) != NIL) {
		if (!integer_p(cadr(x)))
			return error("<=: expected integer, got", cadr(x));
		if (bignum_less_p(cadr(x), car(x))) return FALSE;
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
	if (load(string(cadr(x))) < 0)
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

cell pp_minus(cell x) {
	cell	a;

	x = cdr(x);
	if (cdr(x) == NIL) return bignum_negate(car(x));
	a = car(x);
	x = cdr(x);
	save(a);
	while (x != NIL) {
		if (!integer_p(car(x)))
			return error("-: expected integer, got", car(x));
		a = bignum_subtract(a, car(x));
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
	if (cdr(x) == NIL) return car(x);
	a = make_integer(0);
	save(a);
	while (x != NIL) {
		if (!integer_p(car(x)))
			return error("+: expected integer, got", car(x));
		a = bignum_add(a, car(x));
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
	return car(bignum_divide(x, cadr(x), caddr(x)));
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

cell pp_remainder(cell x) {
	return cdr(bignum_divide(x, cadr(x), caddr(x)));
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
	char	*s = string(cadr(x));

	y = find_symbol(s);
	if (y != NIL) return y;
	/*
	 * Cannot use make_symbol(), because
	 * string(cadr(x)) may move during GC.
	 */
	Symbols = alloc(NIL, Symbols);
	n = allocv(T_SYMBOL, strlen(s)+1);
	car(Symbols) = n;
	strcpy(string(n), string(cadr(x)));
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
	return clone_string(cadr(x), string_len(cadr(x))-1);
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
	return clone_string(cadr(x), (int) strlen(string(cadr(x))));
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
	if (cdr(x) == NIL) return car(x);
	a = make_integer(1);
	save(a);
	while (x != NIL) {
		if (!integer_p(car(x)))
			return error("*: expected integer, got", car(x));
		a = bignum_multiply(a, car(x));
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

cell pp_wrong(cell x) {
	return error(string(cadr(x)), length(x) > 2? caddr(x): NOEXPR);
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
 { "char-ci<=?",          pp_char_ci_le_p,        2, -1, { CHR,CHR,___ } },
 { "char-ci<?",           pp_char_ci_lt_p,        2, -1, { CHR,CHR,___ } },
 { "char-ci=?",           pp_char_ci_eq_p,        2, -1, { CHR,CHR,___ } },
 { "char-ci>=?",          pp_char_ci_ge_p,        2, -1, { CHR,CHR,___ } },
 { "char-ci>?",           pp_char_ci_gt_p,        2, -1, { CHR,CHR,___ } },
 { "char-downcase",       pp_char_downcase,       1,  1, { CHR,___,___ } },
 { "char-lower-case?",    pp_char_lower_case_p,   1,  1, { CHR,___,___ } },
 { "char-numeric?",       pp_char_numeric_p,      1,  1, { CHR,___,___ } },
 { "char-upcase",         pp_char_upcase,         1,  1, { CHR,___,___ } },
 { "char-upper-case?",    pp_char_upper_case_p,   1,  1, { CHR,___,___ } },
 { "char-whitespace?",    pp_char_whitespace_p,   1,  1, { CHR,___,___ } },
 { "char<=?",             pp_char_le_p,           2, -1, { CHR,CHR,___ } },
 { "char<?",              pp_char_lt_p,           2, -1, { CHR,CHR,___ } },
 { "char=?",              pp_char_eq_p,           2, -1, { CHR,CHR,___ } },
 { "char>=?",             pp_char_ge_p,           2, -1, { CHR,CHR,___ } },
 { "char>?",              pp_char_gt_p,           2, -1, { CHR,CHR,___ } },
 { "close-input-port",    pp_close_input_port,    1,  1, { INP,___,___ } },
 { "close-output-port",   pp_close_output_port,   1,  1, { OUP,___,___ } },
 { "cons",                pp_cons,                2,  2, { ___,___,___ } },
 { "current-input-port",  pp_current_input_port,  0,  0, { ___,___,___ } },
 { "current-output-port", pp_current_output_port, 0,  0, { ___,___,___ } },
 { "delete-file",         pp_delete_file,         1,  1, { STR,___,___ } },
 { "display",             pp_display,             1,  2, { ___,OUP,___ } },
 { "eof-object?",         pp_eof_object_p,        1,  1, { ___,___,___ } },
 { "eq?",                 pp_eq_p,                2,  2, { ___,___,___ } },
 { "=",                   pp_equal,               2, -1, { INT,INT,___ } },
 { "expand-macro",        pp_expand_macro,        1,  1, { ___,___,___ } },
 { "file-exists?",        pp_file_exists_p,       1,  1, { STR,___,___ } },
 { "gensym",              pp_gensym,              0,  1, { STR,___,___ } },
 { ">",                   pp_greater,             2, -1, { INT,INT,___ } },
 { ">=",                  pp_greater_equal,       2, -1, { INT,INT,___ } },
 { "input-port?",         pp_input_port_p,        1,  1, { ___,___,___ } },
 { "integer?",            pp_integer_p,           1,  1, { ___,___,___ } },
 { "integer->char",       pp_integer_to_char,     1,  1, { INT,___,___ } },
 { "<",                   pp_less,                2, -1, { INT,INT,___ } },
 { "<=",                  pp_less_equal,          2, -1, { INT,INT,___ } },
 { "list->string",        pp_list_to_string,      1,  1, { LST,___,___ } },
 { "list->vector",        pp_list_to_vector,      1,  1, { LST,___,___ } },
 { "load",                pp_load,                1,  1, { STR,___,___ } },
 { "make-string",         pp_make_string,         1,  2, { INT,___,___ } },
 { "make-vector",         pp_make_vector,         1,  2, { INT,___,___ } },
 { "-",                   pp_minus,               1, -1, { INT,___,___ } },
 { "open-input-file",     pp_open_input_file,     1,  1, { STR,___,___ } },
 { "open-output-file",    pp_open_output_file,    1,  1, { STR,___,___ } },
 { "output-port?",        pp_output_port_p,       1,  1, { ___,___,___ } },
 { "pair?",               pp_pair_p,              1,  1, { ___,___,___ } },
 { "peek-char",           pp_peek_char,           0,  1, { INP,___,___ } },
 { "+",                   pp_plus,                0, -1, { ___,___,___ } },
 { "procedure?",          pp_procedure_p,         1,  1, { ___,___,___ } },
 { "quotient",            pp_quotient,            2,  2, { INT,INT,___ } },
 { "read",                pp_read,                0,  1, { INP,___,___ } },
 { "read-char",           pp_read_char,           0,  1, { INP,___,___ } },
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
 { "string-ci<=?",        pp_string_ci_le_p,      2, -1, { STR,STR,___ } },
 { "string-ci<?",         pp_string_ci_lt_p,      2, -1, { STR,STR,___ } },
 { "string-ci=?",         pp_string_ci_eq_p,      2, -1, { STR,STR,___ } },
 { "string-ci>=?",        pp_string_ci_ge_p,      2, -1, { STR,STR,___ } },
 { "string-ci>?",         pp_string_ci_gt_p,      2, -1, { STR,STR,___ } },
 { "string<=?",           pp_string_le_p,         2, -1, { STR,STR,___ } },
 { "string<?",            pp_string_lt_p,         2, -1, { STR,STR,___ } },
 { "string=?",            pp_string_eq_p,         2, -1, { STR,STR,___ } },
 { "string>=?",           pp_string_ge_p,         2, -1, { STR,STR,___ } },
 { "string>?",            pp_string_gt_p,         2, -1, { STR,STR,___ } },
 { "string?",             pp_string_p,            1,  1, { ___,___,___ } },
 { "substring",           pp_substring,           3,  3, { STR,INT,INT } },
 { "symbol?",             pp_symbol_p,            1,  1, { ___,___,___ } },
 { "symbol->string",      pp_symbol_to_string,    1,  1, { SYM,___,___ } },
 { "symbols",             pp_symbols,             0,  0, { ___,___,___ } },
 { "*",                   pp_times,               0, -1, { ___,___,___ } },
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
 { "wrong",               pp_wrong,               1,  2, { STR,___,___ } },
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
			if (!integer_p(car(a)))
				return expected(car(x), "integer", car(a));
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
cell make_application(cell proc_sym) {
	cell	app, p;

	p = lookup(proc_sym, Environment);
	if (p == NIL) return NIL;
	p = cadr(p);
	if (syntax_p(p)) p = cdr(p);
	app = alloc(FALSE, NIL);
	app = alloc(S_quote, app);
	app = alloc(app, NIL);
	return alloc(p, app);
}

/* Return (#<procedure> (quote #f)) or () */
cell make_application_by_name(char *proc_name) {
	cell	p_sym;

	p_sym = find_symbol(proc_name);
	if (p_sym == NIL) return NIL;
	return make_application(p_sym);
}

int has_property_p(int (*p)(cell x), cell x) {
	if (atom_p(x)) return 0;
	if (car(x) == S_quote) return 0;
	if (p(x)) return 1;
	while (!atom_p(x)) {
		if (has_property_p(p, car(x))) return 1;
		x = cdr(x);
	}
	return 0;
}

int syntax_object_p(cell x) {
	cell	y;

	if (symbol_p(car(x))) {
		y = lookup(car(x), Environment);
		if (y != NIL && syntax_p(cadr(y))) return 1;
	}
	return 0;
}

int quasiquotation_p(cell x) {
	return car(x) == S_quasiquote;
}

int uses_transformer_p(cell x) {
	return has_property_p(syntax_object_p, x);
}

int uses_quasiquote_p(cell x) {
	return has_property_p(quasiquotation_p, x);
}

cell expand_qq(cell x, cell app) {
	cell	n, a, new;

	if (Error_flag) return x;
	if (atom_p(x)) return x;
	if (car(x) == S_quote) return x;
	if (car(x) == S_quasiquote) {
		cadadr(app) = x;
		return _eval(app, 0);
	}
	n = a = NIL;
	save(n);
	while (!atom_p(x)) {
		if (n == NIL) {
			n = alloc(expand_qq(car(x), app), NIL);
			car(Stack) = n;
			a = n;
		}
		else {
			new = alloc(expand_qq(car(x), app), NIL);
			cdr(a) = new;
			a = cdr(a);
		}
		x = cdr(x);
	}
	cdr(a) = x;
	unsave(1);
	return n;
}

cell expand_quasiquote(cell x) {
	cell	app;

	if (Error_flag) return x;
	if (atom_p(x)) return x;
	if (!uses_quasiquote_p(x)) return x;
	app = make_application_by_name("expand-quasiquote");
	if (app == NIL) return x;
	save(app);
	x = expand_qq(x, app);
	unsave(1);
	return x;
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
	n = a = NIL;
	save(n);
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
		if (atom_p(a)) return too_few_args(n);
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
					Proc_ptr += 1;
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
					Acc = alloc3(T_SYNTAX, Acc, ATOM_TAG);
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
	x = expand_quasiquote(x);
	car(Stack) = x;
	x = expand_syntax(x);
	x = _eval(x, 0);
	unsave(1);
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

void dump_image(char *p) {
	FILE	*f;
	cell	**v;
	int	n, i, k;
	char	magic[33];
	char	buf[100];

	f = fopen(p, "wb");
	if (f == NULL) {
		error("cannot create image file",
			make_string(p, (int) strlen(p)));
		return;
	}
	strcpy(magic, "S9__yyyy-mm-dd__s_0123__________");
	strncpy(&magic[4], VERSION, 10);
	magic[16] = sizeof(cell)+'0';
	n = 0x30313233L;
	memcpy(&magic[18], &n, 4);
	fwrite(magic, 32, 1, f);
	i = Pool_size;
	fwrite(&i, sizeof(int), 1, f);
	i = Vpool_size;
	fwrite(&i, sizeof(int), 1, f);
	v = Image_vars;
	i = 0;
	while (v[i]) {
		fwrite(v[i], sizeof(cell), 1, f);
		i = i+1;
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
	FILE	*f;
	cell	**v;
	int	n, i;
	char	buf[33];
	int	bad = 0;
	int	inodes, ivcells;
	cell	name;

	name = make_string(p, (int) strlen(p));
	f = fopen(p, "rb");
	if (f == NULL) return -1;
	fread(buf, sizeof(buf)-1, 1, f);
	if (memcmp(buf, "S9", 2)) {
		error("error in image file (magic match failed)", name);
		bad = 1;
	}
	if (memcmp(&buf[4], VERSION, 10)) {
		error("error in image file (wrong version)", name);
		bad = 1;
	}
	memcpy(&n, &buf[18], sizeof(int));
	if (n != 0x30313233) {
		error("error in image file (wrong architecture)", name);
		bad = 1;
	}
	memset(Tag, 0, Pool_size);
	fread(&inodes, sizeof(int), 1, f);
	if (inodes > Pool_size) {
		error("error in image file (too many nodes)", NOEXPR);
		bad = 1;
	}
	fread(&ivcells, sizeof(int), 1, f);
	if (ivcells > Vpool_size) {
		error("error in image file (too many vector cells)", NOEXPR);
		bad = 1;
	}
	v = Image_vars;
	i = 0;
	while (v[i]) {
		fread(v[i], sizeof(cell), 1, f);
		i = i+1;
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

	path = strdup(string(car(S_library_path)));
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
			car(S_library_path) = get_library_path();
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
		n = alloc3((cell) &p[i], NIL, ATOM_TAG);
		n = alloc3(T_PRIMITIVE, n, ATOM_TAG);
		Environment = extend(v, n, Environment);
	}
}

/* Extension prototypes */
void unix_init(void);

void make_initial_env(void) {
	Environment = alloc(NIL, NIL);
	Environment = extend(add_symbol("**"), NIL, Environment);
	S_latest = cdadr(Environment);
	Environment = extend(add_symbol("*extensions*"), NIL, Environment);
	S_extensions = cdadr(Environment);
	Environment = extend(add_symbol("*library-path*"), NIL, Environment);
	S_library_path = cdadr(Environment);
	car(S_library_path) = get_library_path();
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

void usage(char *name, int quit) {
	pr("Usage: ");
	pr(name);
	pr(" [-h?] [-gnqv] [-d image] [-f program] [-i] [-m size[m]]"); nl();
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
