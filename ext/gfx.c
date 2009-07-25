/*
 *	Graphics routines
 *	By Nils M Holm, 1997,1998,2003,2009
 *
 *	DISCMAILER: I know virtually nothing about X11 programming.
 *	This code works for me, but is probably broken in general.
 */

#define EXTERN
#include "s9.h"
#undef EXTERN

#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <ctype.h>

#define	XLIM		13333.0
#define YLIM		10000.0

#define FSX	100
#define	FSY	200

#define FONT_POOL_SIZE	2048

#define OK	0
#define NOGFX	-1
#define NOMEM	-2
#define NOFILE	-3
#define BADFILE	-4

#define EV_X		0
#define EV_Y		1
#define EV_BUTTONS	2
#define EV_KEY		3
#define EVENT		4

Display		*Dsp = NULL;
Window		Win;
GC		Csr, Csr_set, Csr_clear, Csr_invert;
double		Fx, Fy;
unsigned int	Xn, Yn, PXn, PYn;
int		Initial;

FILE		*DumpFd;

char		**Vfont;
char		Pool[FONT_POOL_SIZE];
int		Pt;
int		Vfx, Vfy;

struct jrnl {
	int		op;
	int		x, y, dx, dy;
	int		r, m, f;
	char		*s;
	struct jrnl	*next;
} *Journal = NULL;

enum {
	LINE,
	BOX,
	ELLIPSE,
	STRING
};

void adjust(int width, int height) {
	double	t;

	t = width;
	Fx = XLIM / t;
	t = height;
	Fy = YLIM / t;
}

void clear(void) {
	struct jrnl	*jp, *tp;

	jp = Journal;
	Journal = NULL;
	while (jp != NULL) {
		tp = jp;
		jp = jp->next;
		if (tp->op == STRING)
			free(tp->s);
		free(tp);
	}
	Initial = 1;
}

void log(int op, int x, int y, int dx, int dy, int r, int m, int f, char *s) {
	struct jrnl	*jp;
	char	*t;

	jp = (struct jrnl *) malloc(sizeof(struct jrnl));
	if (s == NULL) {
		s = "foo";
	}
	else {
		t = s;
		s = (char *) malloc(strlen(s)+1);
		strcpy(s, t);
	}
	if (jp == NULL || s == NULL) {
		fprintf(stderr, "gfx: out of memory\n");
		exit(1);
	}
	jp->op = op;
	jp->x = x;
	jp->y = y;
	jp->dx = dx;
	jp->dy = dy;
	jp->r = r;
	jp->m = m;
	jp->f = f;
	jp->s = s;
	jp->next = Journal;
	Journal = jp;
}

void refresh(int dummy);

cell x11_init(char *title) {
	XEvent		e;
	XGCValues	v;
	int		i = -1;
	char		*dpyname;
	int		scrn;
	XSizeHints	*wm_size;
	XWMHints	*wm_hints;
	XClassHint	*wm_class;
	XTextProperty	wname;

	PXn = Xn = 800;
	PYn = Yn = 600;
	wm_size = XAllocSizeHints();
	wm_hints = XAllocWMHints();
	wm_class = XAllocClassHint();
	if (!(wm_size && wm_hints && wm_class))
		error("gfx: not enough memory", NOEXPR);
	dpyname = getenv("DISPLAY");
	Dsp = XOpenDisplay(dpyname);
	scrn = DefaultScreen(Dsp);
	if (!Dsp) return error("gfx: could not open display", NOEXPR);
	Win = XCreateSimpleWindow(Dsp, RootWindow(Dsp, scrn), 10, 10,
		Xn, Yn, 0, BlackPixel(Dsp, scrn), WhitePixel(Dsp, scrn));
	if (!Dsp) return error("gfx: could not create window", NOEXPR);
	XSelectInput(Dsp, Win, ExposureMask);
	v.plane_mask = AllPlanes;
	v.foreground = 0;
	v.background = -1;
	v.function = GXset;
	Csr_set = XCreateGC(Dsp, Win, GCPlaneMask | GCForeground
		| GCBackground | GCFunction, &v);
	v.function = GXclear;
	Csr_clear = XCreateGC(Dsp, Win, GCPlaneMask | GCForeground
		| GCBackground | GCFunction, &v);
	XStringListToTextProperty(&title, 1, &wname);
	wm_hints->initial_state = NormalState;
	wm_hints->flags = StateHint;
	wm_class->res_name = title;
	wm_class->res_class = title;
	XSetWMProperties(Dsp, Win, &wname, &wname, &title, 1, wm_size,
		wm_hints, wm_class);
	XMapWindow(Dsp, Win);
	do  {
		XNextEvent(Dsp, &e);
	} while (e.type != Expose);
	adjust(Xn, Yn);
	DumpFd = NULL;
	clear();
	signal(SIGALRM, refresh);
	refresh(1);
	return 0;
}

cell pp_gfx_fini(cell x) {
	if (!Dsp) return UNSPECIFIC;
	clear();
	XFreeGC(Dsp, Csr_set);
	XFreeGC(Dsp, Csr_clear);
	XDestroyWindow(Dsp, Win);
	XFlush(Dsp);
	XCloseDisplay(Dsp);
	Dsp = NULL;
	return UNSPECIFIC;
}

void flags(int f) {
	switch(f) {
	case 0:	Csr = Csr_set; break;
	case 1:	Csr = Csr_clear; break;
	}
}

int xscale(int x) {
	double	t = x;

	t /= Fx;
	return (int) t;
}

int yscale(int y) {
	double	t = y;

	t /= Fy;
	return Yn - (int) t;
}

void line(int x, int y, int dx, int dy, int m) {
	if (DumpFd) {
		fprintf(DumpFd, "%d %d %d %d %d line\n",
			x, y, dx, dy, m?0:1);
		return;
	}
	x = xscale(x);
	y = yscale(y);
	dx = xscale(dx);
	dy = yscale(dy);
	flags(m);
	XDrawLine(Dsp, Win, Csr, x, y, dx, dy);
}

cell pp_gfx_line(cell expr) {
	char	self[] = "gfx:line";
	char	msg1[] = "gfx:line: expected integer, got";
	char	msg2[] = "gfx:line: expected boolean, got";
	int	x, y, dx, dy, m;

	x = integer_value(self, cadr(expr));
	y = integer_value(self, caddr(expr));
	dx = integer_value(self, cadddr(expr));
	if (!integer_p(car(cddddr(expr))))
		return error(msg1, car(cddddr(expr)));
	if (!boolean_p(cadr(cddddr(expr))))
		return error(msg2, cadr(cddddr(expr)));
	dy = integer_value(self, car(cddddr(expr)));
	m = cadr(cddddr(expr)) == TRUE;
	log(LINE, x, y, dx, dy, 0, m, 0, NULL);
	line(x, y, dx, dy, m);
	return UNSPECIFIC;
}

void box(int x, int y, int dx, int dy, int m, int f) {
	cell	t;

	if (DumpFd) {
		fprintf(DumpFd, "%d %d %d %d %d box %s\n",
			x, y, dx, dy, m?0:1, f? "fill": "stroke");
		return;
	}
	x = xscale(x);
	y = yscale(y);
	dx = xscale(dx);
	dy = yscale(dy);
	if (x > dx) { t = x; x = dx; dx = t; }
	if (y > dy) { t = y; y = dy; dy = t; }
	flags(m);
	if (f)
		XFillRectangle(Dsp, Win, Csr, x, y, dx-x, dy-y);
	else
		XDrawRectangle(Dsp, Win, Csr, x, y, dx-x, dy-y);
}

cell pp_gfx_box(cell expr) {
	char	self[] = "gfx:box";
	char	msg1[] = "gfx:box: expected integer, got";
	char	msg2[] = "gfx:box: expected boolean, got";
	int	x, y, dx, dy, m, f;

	x = integer_value(self, cadr(expr));
	y = integer_value(self, caddr(expr));
	dx = integer_value(self, cadddr(expr));
	if (!integer_p(car(cddddr(expr))))
		return error(msg1, car(cddddr(expr)));
	if (!boolean_p(cadr(cddddr(expr))))
		return error(msg2, cadr(cddddr(expr)));
	if (!boolean_p(caddr(cddddr(expr))))
		return error(msg2, cadr(cddddr(expr)));
	dy = integer_value(self, car(cddddr(expr)));
	m = cadr(cddddr(expr)) == TRUE;
	f = caddr(cddddr(expr)) == TRUE;
	log(BOX, x, y, dx, dy, 0, m, f, NULL);
	box(x, y, dx, dy, m, f);
	return UNSPECIFIC;
}

cell pp_gfx_clear(cell expr) {
	clear();
	return UNSPECIFIC;
}

void ellipse(int x, int y, int dx, int dy, int m, int f) {
	cell	t;

	if (DumpFd) {
		fprintf(DumpFd, "%d %d %d %d %d ellipse %s\n",
			x, y, dx, dy, m?0:1, f? "fill": "stroke");
		return;
	}
	x = xscale(x);
	y = yscale(y);
	dx = xscale(dx);
	dy = yscale(dy);
	if (x > dx) { t = x; x = dx; dx = t; }
	if (y > dy) { t = y; y = dy; dy = t; }
	flags(m);
	if (f)
		XFillArc(Dsp, Win, Csr, x, y, dx-x, dy-y, 0, 360*64);
	else
		XDrawArc(Dsp, Win, Csr, x, y, dx-x, dy-y, 0, 360*64);
}

cell pp_gfx_ellipse(cell expr) {
	char	self[] = "gfx:ellipse";
	char	msg1[] = "gfx:ellipse: expected integer, got";
	char	msg2[] = "gfx:ellipse: expected boolean, got";
	int	x, y, dx, dy, m, f;

	x = integer_value(self, cadr(expr));
	y = integer_value(self, caddr(expr));
	dx = integer_value(self, cadddr(expr));
	if (!integer_p(car(cddddr(expr))))
		return error(msg1, car(cddddr(expr)));
	if (!boolean_p(cadr(cddddr(expr))))
		return error(msg2, cadr(cddddr(expr)));
	if (!boolean_p(caddr(cddddr(expr))))
		return error(msg2, cadr(cddddr(expr)));
	dy = integer_value(self, car(cddddr(expr)));
	m = cadr(cddddr(expr)) == TRUE;
	f = caddr(cddddr(expr)) == TRUE;
	log(ELLIPSE, x, y, dx, dy, 0, m, f, NULL);
	ellipse(x, y, dx, dy, m, f);
	return UNSPECIFIC;
}

cell ratio(void) {
	cell	r;

	r = (cell) Fx * (cell) 100;
	return r / Fy;
}

char *font_alloc(int size) {
	char	*a;

	if (size + Pt >= FONT_POOL_SIZE) {
		error("gfx: out of font memory", NOEXPR);
		return NULL;
	}
	a = &Pool[Pt];
	Pt = Pt+size;
	return a;
}

char **loadvf(char *file, int *statusp) {
	int	infd, n, i, k, p;
	char	buf[4], ch[1];

	Pt = 0;
	Vfont = (char **) font_alloc(128*sizeof(char *));
	if (Vfont == 0) {
		statusp[0] = NOMEM;
		return NULL;
	}
	infd = open(file, 0);
	if (infd < 0) {
		statusp[0] = NOFILE;
		Pt = 0;
		return NULL;
	}
	if (	read(infd, buf, 4) != 4 ||
		memcmp(buf, "VF11", 4)
	) {
		close(infd);
		Pt = 0;
		statusp[0] = BADFILE;
		return NULL;
	}
	for (i=0; i<32; i++) Vfont[i] = 0;
	Vfont[0] = font_alloc(3);
	if (Vfont[0] == 0) {
		close(infd);
		Pt = 0;
		statusp[0] = NOMEM;
		return NULL;
	}
	Vfont[0][0] = 0;
	read(infd, ch, 1);
	Vfx = ch[0];
	Vfont[0][1] = Vfx;
	k = read(infd, ch, 1);
	Vfy = ch[0];
	Vfont[0][2] = Vfy;
	while (k > 0 && i < 128) {
		read(infd, ch, 1);
		n = ch[0];
		Vfont[i] = font_alloc(n+1);
		if (Vfont[i] == 0) {
			close(infd);
			Pt = 0;
			statusp[0] = NOMEM;
			return NULL;
		}
		Vfont[i][0] = n;
		k = n? read(infd, &Vfont[i][1], n): 1;
		i = i+1;
	}
	close(infd);
	if (i<128) {
		Pt = 0;
		statusp[0] = BADFILE;
		return NULL;
	}
	statusp[0] = OK;
	return Vfont;
}

void putv(int x, int y, int scale, int mode, char *v) {
	int	dx, dy, nx, ny, i, cx, cy, d;

	cx = FSX / Vfx * 10/(10-scale);
	cy = FSY / Vfy * 10/(10-scale);
	for (i=1; i<=v[0]; i++) {
		d = (v[i] & 0x70) >> 4;
		if (d == 4)      { dx =  0; dy = -1; }
		else if (d == 3) { dx =  1; dy = -1; }
		else if (d == 2) { dx =  1; dy =  0; }
		else if (d == 1) { dx =  1; dy =  1; }
		else if (d == 0) { dx =  0; dy =  1; }
		else if (d == 7) { dx = -1; dy =  1; }
		else if (d == 6) { dx = -1; dy =  0; }
		else             { dx = -1; dy = -1; }
		nx = x + (dx * (v[i] & 15) * cx);
		ny = y + (dy * (v[i] & 15) * cy);
		if (v[i] & 0x80) line(x, y, nx, ny, mode);
		x = nx;
		y = ny;
	}
}

cell put_string(int x, int y, int r, int m, char *s) {
	int	i, k, cx;

	cx = FSX * r;
	cx = FSX * 10/(10-r);
	k = strlen(s);
	for (i=0; i<k; i++) {
		if (!(s[i] & ~127) && Vfont[s[i]] != 0)
			putv(x, y, r, m, Vfont[s[i]]);
		x += cx;
	}
}

cell pp_gfx_put_string(cell expr) {
	char	self[] = "gfx:put-string";
	char	msg1[] = "gfx:put-string: expected boolean, got";
	char	msg2[] = "gfx:put-string: expected string, got";
	int	x, y, r, m;
	char	*s;
	int	cx, i, k;

	x = integer_value(self, cadr(expr));
	y = integer_value(self, caddr(expr));
	r = integer_value(self, cadddr(expr));
	if (!boolean_p(car(cddddr(expr))))
		return error(msg1, car(cddddr(expr)));
	if (!string_p(cadr(cddddr(expr))))
		return error(msg2, cadr(cddddr(expr)));
	m = car(cddddr(expr)) == TRUE;
	s = string(cadr(cddddr(expr)));
	log(STRING, x, y, 0, 0, r, m, 0, s);
	put_string(x, y, r, m, s);
	return UNSPECIFIC;
}

cell pp_gfx_string_width(cell expr) {
	char	*s;
	int	r;

	s = string(cadr(expr));
	r = integer_value("gfx:string-width", caddr(expr));
	return make_integer(strlen(s) * FSX * 10/(10-r));
}

cell pp_gfx_string_height(cell expr) {
	char	*s;
	int	r;

	s = string(cadr(expr));
	r = integer_value("gfx:string-width", caddr(expr));
	return make_integer(FSY * 10/(10-r));
}

void load_font(void) {
	char	*path, buf[256], *p;
	char	libdir[240], libfile[256];
	char	*home;
	cell	new;
	char	fontfile[] = "system.vf";
	int	status;

	path = copy_string(string(car(S_library_path)));
	home = getenv("HOME");
	if (home == NULL) home = ".";
	p = strtok(path, ":");
	while (p != NULL) {
		if (p[0] == '~') {
			if (strlen(p) + strlen(home) >= sizeof(libdir)-1)
				fatal("load_font: path too long");
			sprintf(libdir, "%s%s", home, &p[1]);
		}
		else {
			if (strlen(p) >= sizeof(libdir)-1)
				fatal("load_font: path too long");
			strcpy(libdir, p);
		}
		if (strlen(fontfile) + strlen(libdir) >= sizeof(libfile)-1)
			fatal("load_font: path too long");
		sprintf(libfile, "%s/%s", libdir, fontfile);
		loadvf(libfile, &status);
		if (status == OK) {
			free(path);
			return;
		}
		p = strtok(NULL, ":");
	}
	sprintf(buf, "Could not find font file: \"%s\"", fontfile);
	fatal(buf);
}

cell pp_gfx_init(cell x) {
	int	p;
	char	fp[256], base[240];
	char	m1[] = "Scheme 9 Scientific Calculator",
		m2[] = "By Nils M Holm, 2009";

	if (Dsp) return UNSPECIFIC;
	x11_init("S9fES GFX");
	if (Error_flag) return UNSPECIFIC;
	load_font();
	log(STRING, 6666-5000, 5500, 0, 0, 7, 1, 0, m1);
	log(STRING, 6666-2000, 4500, 0, 0, 5, 1, 0, m2);
	Initial = 1;
	return UNSPECIFIC;
}

cell reshaped(void) {
	XEvent		xe;
	int		n, r;
	Drawable	d;

	r = 0;
	while (XPending(Dsp)) {
		XNextEvent(Dsp, &xe);
		if (xe.type == Expose) {
			XGetGeometry(Dsp, Win, &d, &n, &n, &Xn, &Yn,
				(unsigned int *) &n, (unsigned int *) &n);
			adjust(Xn, Yn);
			while (XCheckTypedEvent(Dsp, Expose, &xe))
				;
			r = 1;
		}
	}
	return r;
}

void reverse_jrnl(void) {
	struct jrnl	*jp, *prev, *tp;

	prev = NULL;
	jp = Journal;
	tp = NULL;
	while (jp != NULL) {
		tp = jp;
		jp = jp->next;
		tp->next = prev;
		prev = tp;
	}
	Journal = tp;
}

void redraw(void) {
	struct jrnl	*jp;

	if (PXn != Xn || PYn != Yn) {
		box(0, 0, YLIM, XLIM, 0, 1);
		PXn = Xn;
		PYn = Yn;
	}
	reverse_jrnl();
	jp = Journal;
	while (jp != NULL) {
		switch (jp->op) {
		case LINE:
			line(jp->x, jp->y, jp->dx, jp->dy, jp->m);
			break;
		case BOX:
			box(jp->x, jp->y, jp->dx, jp->dy, jp->m, jp->f);
			break;
		case ELLIPSE:
			ellipse(jp->x, jp->y, jp->dx, jp->dy, jp->m, jp->f);
			break;
		case STRING:
			put_string(jp->x, jp->y, jp->r, jp->m, jp->s);
			break;
		}
		jp = jp->next;
	}
	XFlush(Dsp);
	reverse_jrnl();
}

void refresh(int dummy) {
	if (Dsp == NULL) return;
	if (DumpFd != NULL) return;
	if (Initial) box(0, 0, XLIM, YLIM, 0, 1);
	if (Initial || reshaped()) redraw();
	Initial = 0;
	signal(SIGALRM, refresh);
	ualarm(200000L, 0);
}

cell pp_gfx_write_canvas(cell expr) {
	char	*s;

	s = string(cadr(expr));
	if ((DumpFd = fopen(s, "r")) != NULL) {
		fclose(DumpFd);
		refresh(0);
		return error("gfx:write-canvas: file exists", cadr(expr));
	}
	if ((DumpFd = fopen(s, "w")) == NULL) {
		refresh(0);
		return error("gfx:write-canvas: cannot create file",
			cadr(expr));
	}
	fprintf(DumpFd, "%%!PS-Adobe-3.0\n"
			"%%%%BoundingBox: 0 0 1333 1000\n"
			"%%%%EndComments\n"
			"0 0 translate 0.1 0.1 scale\n"
        		"/line {\n"
                	"\tsetgray\n"
                	"\t/dy exch def\n"
			"\t/dx exch def\n"
                	"\t/y exch def\n"
                	"\t/x exch def\n"
                	"\tnewpath\n"
			"\tx y moveto\n"
                	"\tdx dy lineto\n"
                	"\tstroke\n"
        		"} def\n"
        		"/box {\n"
			"\tsetgray\n"
                	"\t/dy exch def\n"
			"\t/dx exch def\n"
                	"\t/y exch def\n"
                	"\t/x exch def\n"
                	"\tnewpath\n"
			"\tx y moveto\n"
                	"\tdx y lineto\n"
			"\tdx dy lineto\n"
                	"\tx dy lineto\n"
                	"\tx y lineto\n"
                	"\tclosepath\n"
        		"} def\n"
			"/ellipse {\n"
			"\t%% Draw an ellipse using four bezier curves\n"
			"\t%% Based on code by RedGrittyBrick, 2003-10-20\n"
			"\tsetgray\n"
			"\t/dy exch def\n"
			"\t/dx exch def\n"
			"\t/y  exch def\n"
			"\t/x  exch def\n"
			"\t/rx dx x sub 2 div  def\n"
			"\t/ry dy y sub 2 div  def\n"
			"\t/y  ry y add  def\n"
			"\t/x  rx x add  def\n"
			"\t/kappa 0.5522847498 def\n"
			"\tnewpath\n"
			"\tx\n"
			"\ty ry add  moveto\n"
			"\tx kappa rx mul add  y ry add\n"
			"\tx rx add  y kappa ry mul add\n"
			"\tx rx add  y  curveto\n"
			"\tx rx add  y kappa ry mul sub\n"
			"\tx kappa rx mul add  y ry sub\n"
			"\tx y ry sub  curveto\n"
			"\tx kappa rx mul sub  y ry sub\n"
			"\tx rx sub  y kappa ry mul sub\n"
			"\tx rx sub  y  curveto\n"
			"\tx rx sub  y kappa ry mul add\n"
			"\tx kappa rx mul sub  y ry add\n"
			"\tx y ry add  curveto\n"
			"} def\n");
	redraw();
	fclose(DumpFd);
	DumpFd = NULL;
	return UNSPECIFIC;
}

struct Primitive_procedure Gfx_primitives[] = {
 { "gfx:box",           pp_gfx_box,           6,  6, { REA,REA,REA } },
 { "gfx:clear",         pp_gfx_clear,         0,  0, { ___,___,___ } },
 { "gfx:write-canvas",  pp_gfx_write_canvas,  1,  1, { STR,___,___ } },
 { "gfx:ellipse",       pp_gfx_ellipse,       6,  6, { REA,REA,REA } },
 { "gfx:fini",          pp_gfx_fini,          0,  0, { ___,___,___ } },
 { "gfx:init",          pp_gfx_init,          0,  0, { ___,___,___ } },
 { "gfx:line",          pp_gfx_line,          5,  5, { REA,REA,REA } },
 { "gfx:put-string",    pp_gfx_put_string,    5,  5, { REA,REA,REA } },
 { "gfx:string-width",  pp_gfx_string_width,  2,  2, { STR,REA,___ } },
 { "gfx:string-height", pp_gfx_string_height, 2,  2, { STR,REA,___ } },
 { NULL }
};

void gfx_init(void) {
	add_primitives("gfx", Gfx_primitives);
}
