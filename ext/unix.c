#define EXTERN
#include "s9.h"
#undef EXTERN

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <pwd.h>
#include <unistd.h>
#include <errno.h>

/*
 *	Allow us at least to write
 *		assign(assign(Car[x] = alloc(foo, bar)));
 *	in presence of that fact that C's
 *	order of evaluation messes up
 *		Car[x] = alloc(foo, bar);
 */
cell	New_node;
#define assign(n,v)	{ New_node = v; n = New_node; }

cell pp_unix_chdir(cell x) {
	if (chdir(string(cadr(x))) < 0)
		return error("chdir returned", make_integer(errno));
	return UNSPECIFIC;
}

cell pp_unix_chmod(cell x) {
	int	r;

	r = chmod(string(cadr(x)), integer_value("unix:chmod", caddr(x)));
	if (r < 0) return error("chown returned", make_integer(errno));
	return UNSPECIFIC;
}

cell pp_unix_chown(cell x) {
	int	r;

	r = chown(string(cadr(x)),
		integer_value("unix:chown", caddr(x)),
		integer_value("unix:chown", cadddr(x)));
	if (r < 0) return error("chown returned", make_integer(errno));
	return UNSPECIFIC;
}

cell pp_unix_command_line(cell x) {
	cell	n, a;
	char	**cl;

	if (Command_line == NULL || *Command_line == NULL)
		return NIL;
	n = alloc(NIL, NIL);
	a = n;
	save(n);
	cl = Command_line;
	while (*cl != NULL) {
		assign(Car[a], make_string(*cl, strlen(*cl)));
		cl++;
		if (*cl != NULL) {
			assign(Cdr[a], alloc(NIL, NIL));
			a = Cdr[a];
		}
	}
	unsave(1);
	return n;
}

cell pp_unix_exit(cell x) {
	int	r;

	r = integer_value("unix:exit", cadr(x));
	if (r > 255 || r < 0)
		return error("unix:exit: value out of range", cadr(x));
	if (!Error_flag) exit(r);
	return UNSPECIFIC;
}

cell pp_unix_flush(cell x) {
	fflush(Ports[port_no(cadr(x))]);
	return UNSPECIFIC;
}

cell pp_unix_getcwd(cell x) {
	char	*s;
	cell	n;

	s = getcwd(NULL, 1024);
	n = make_string(s, strlen(s));
	free(s);
	return n;
}

cell pp_unix_getenv(cell x) {
	char	*s;

	s = getenv(string(cadr(x)));
	if (s == NULL) return FALSE;
	return make_string(s, strlen(s));
}

cell pp_unix_getgid(cell x) {
	return make_integer(getgid());
}

cell mkpwent(struct passwd *pw) {
	cell	n, a;

	n = alloc(NIL, NIL);
	save(n);
	assign(Car[n], alloc(add_symbol("user"), NIL));
	cdar(n) = make_string(pw->pw_name, strlen(pw->pw_name));
	a = alloc(NIL, NIL);
	Cdr[n] = a;
	assign(Car[a], alloc(add_symbol("uid"), NIL));
	cdar(a) = make_integer(pw->pw_uid);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("gid"), NIL));
	cdar(a) = make_integer(pw->pw_gid);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("gecos"), NIL));
	cdar(a) = make_string(pw->pw_gecos, strlen(pw->pw_gecos));
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("home"), NIL));
	cdar(a) = make_string(pw->pw_dir, strlen(pw->pw_dir));
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("shell"), NIL));
	cdar(a) = make_string(pw->pw_shell, strlen(pw->pw_shell));
	unsave(1);
	return n;
}

cell pp_unix_getpwnam(cell x) {
	struct passwd	*pw;

	pw = getpwnam(string(cadr(x)));
	if (pw == NULL) return FALSE;
	return mkpwent(pw);
}

cell pp_unix_getpwuid(cell x) {
	struct passwd	*pw;

	pw = getpwuid(integer_value("unix:getpwuid", cadr(x)));
	if (pw == NULL) return FALSE;
	return mkpwent(pw);
}

cell pp_unix_getuid(cell x) {
	return make_integer(getuid());
}

cell pp_unix_link(cell x) {
	if (link(string(cadr(x)), string(caddr(x))) < 0)
		return error("link returned", make_integer(errno));
	return UNSPECIFIC;
}

cell pp_unix_lock(cell x) {
	char	p[256], *s;

	s = string(cadr(x));
	if (strlen(s) > 248)
		return error("unix:lock: path too long", cadr(x));
	sprintf(p, "%s.lock", s);
	return (mkdir(p, 0700) < 0)? FALSE: TRUE;
}

cell pp_unix_mkdir(cell x) {
	if (mkdir(string(cadr(x)), 0755) < 0)
		return error("mkdir returned", make_integer(errno));
	return UNSPECIFIC;
}

cell pp_unix_rmdir(cell x) {
	if (rmdir(string(cadr(x))) < 0)
		return error("rmdir returned", make_integer(errno));
	return UNSPECIFIC;
}

cell pp_unix_spawn(cell x) {
	int	r;
	cell	n;
	int	to_child[2], from_child[2];
	int	in_port, out_port;

	in_port = alloc_port();
	if (in_port < 0) return error("spawn: out of ports", NOEXPR);
	Port_flags[in_port] |= LOCK_TAG;
	Ports[in_port] = (FILE*)1;
	out_port = alloc_port();
	if (out_port < 0) {
		Ports[in_port] = NULL;
		Port_flags[in_port] = 0;
		return error("spawn: out of ports", NOEXPR);
	}
	Port_flags[out_port] |= LOCK_TAG;
	Ports[out_port] = (FILE*)1;
	if (pipe(from_child) < 0) {
		Port_flags[in_port] = 0;
		Port_flags[out_port] = 0;
		Ports[in_port] = NULL;
		Ports[out_port] = NULL;
		error("spawn: pipe() returned", make_integer(errno));
	}
	if (pipe(to_child) < 0) {
		r = errno;
		Port_flags[in_port] = 0;
		Port_flags[out_port] = 0;
		Ports[in_port] = NULL;
		Ports[out_port] = NULL;
		close(from_child[0]);
		close(from_child[1]);
		error("spawn: pipe() returned", make_integer(r));
	}
	r = fork();
	if (r < 0) {
		r = errno;
		Port_flags[in_port] = 0;
		Port_flags[out_port] = 0;
		Ports[in_port] = NULL;
		Ports[out_port] = NULL;
		close(from_child[0]);
		close(from_child[1]);
		close(to_child[0]);
		close(to_child[1]);
		error("spawn: fork() returned", make_integer(r));
	}
	if (r == 0) {
		close(from_child[0]);
		close(to_child[1]);
		dup2(from_child[1], 1);
		dup2(to_child[0], 0);
		execl("/bin/sh", "/bin/sh", "-c", string(cadr(x)), NULL);
		exit(1);
	}
	close(from_child[1]);
	close(to_child[0]);
	Ports[in_port] = fdopen(from_child[0], "r");
	Ports[out_port] = fdopen(to_child[1], "w");
	n = alloc(NIL, NIL);
	save(n);
	assign(Car[n], make_port(in_port, S_input_port));
	assign(Cdr[n], alloc(NIL, NIL));
	cadr(n) = make_port(out_port, S_output_port);
	unsave(1);
	Port_flags[in_port] &= ~LOCK_TAG;
	Port_flags[out_port] &= ~LOCK_TAG;
	return n;
}

cell pp_unix_stat(cell x) {
	struct stat	sb;
	cell		n, a;

	if (stat(string(cadr(x)), &sb) < 0) return FALSE;
	n = alloc(NIL, NIL);
	save(n);
	assign(Car[n], alloc(add_symbol("name"), cadr(x)));
	a = alloc(NIL, NIL);
	Cdr[n] = a;
	assign(Car[a], alloc(add_symbol("size"), NIL));
	cdar(a) = make_integer(sb.st_size);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("uid"), NIL));
	cdar(a) = make_integer(sb.st_uid);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("gid"), NIL));
	cdar(a) = make_integer(sb.st_gid);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("mode"), NIL));
	cdar(a) = make_integer(sb.st_mode);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("ctime"), NIL));
	cdar(a) = make_integer(sb.st_ctime);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("atime"), NIL));
	cdar(a) = make_integer(sb.st_atime);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("mtime"), NIL));
	cdar(a) = make_integer(sb.st_mtime);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("dev"), NIL));
	cdar(a) = make_integer(sb.st_dev);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("ino"), NIL));
	cdar(a) = make_integer(sb.st_ino);
	assign(Cdr[a], alloc(NIL, NIL));
	a = Cdr[a];
	assign(Car[a], alloc(add_symbol("nlink"), NIL));
	cdar(a) = make_integer(sb.st_nlink);
	unsave(1);
	return n;
}

cell pp_unix_symlink(cell x) {
	if (symlink(string(cadr(x)), string(caddr(x))) < 0)
		return error("symlink returned", make_integer(errno));
	return UNSPECIFIC;
}

cell pp_unix_system(cell x) {
	return system(string(cadr(x))) == 0? TRUE: FALSE;
}

cell pp_unix_unlock(cell x) {
	char	p[256], *s;

	s = string(cadr(x));
	if (strlen(s) > 248)
		return error("unix:unlock: path too long", cadr(x));
	sprintf(p, "%s.lock", s);
	rmdir(p);
	return UNSPECIFIC;
}

cell pp_unix_utimes(cell x) {
	if (utimes(string(cadr(x)), NULL) < 0)
		return error("utimes returned", make_integer(errno));
	return UNSPECIFIC;
}

struct Primitive_procedure Unix_primitives[] = {
 { "unix:chdir",        pp_unix_chdir,        1,  1, { STR,___,___ } },
 { "unix:chmod",        pp_unix_chmod,        2,  2, { STR,INT,___ } },
 { "unix:chown",        pp_unix_chown,        3,  3, { STR,INT,INT } },
 { "unix:command-line", pp_unix_command_line, 0,  0, { ___,___,___ } },
 { "unix:exit",         pp_unix_exit,         1,  1, { INT,___,___ } },
 { "unix:flush",        pp_unix_flush,        1,  1, { OUP,___,___ } },
 { "unix:getcwd",       pp_unix_getcwd,       0,  0, { ___,___,___ } },
 { "unix:getenv",       pp_unix_getenv,       1,  1, { STR,___,___ } },
 { "unix:getgid",       pp_unix_getgid,       0,  0, { ___,___,___ } },
 { "unix:getpwnam",     pp_unix_getpwnam,     1,  1, { STR,___,___ } },
 { "unix:getpwuid",     pp_unix_getpwuid,     1,  1, { INT,___,___ } },
 { "unix:getuid",       pp_unix_getuid,       0,  0, { ___,___,___ } },
 { "unix:link",         pp_unix_link,         2,  2, { STR,STR,___ } },
 { "unix:lock",         pp_unix_lock,         1,  1, { STR,___,___ } },
 { "unix:mkdir",        pp_unix_mkdir,        1,  1, { STR,___,___ } },
 { "unix:rmdir",        pp_unix_rmdir,        1,  1, { STR,___,___ } },
 { "unix:spawn",        pp_unix_spawn,        1,  1, { STR,___,___ } },
 { "unix:stat",         pp_unix_stat,         1,  1, { STR,___,___ } },
 { "unix:symlink",      pp_unix_symlink,      2,  2, { STR,STR,___ } },
 { "unix:system",       pp_unix_system,       1,  1, { STR,___,___ } },
 { "unix:unlock",       pp_unix_unlock,       1,  1, { STR,___,___ } },
 { "unix:utimes",       pp_unix_utimes,       1,  1, { STR,___,___ } },
 { NULL }
};

void unix_init(void) {
	add_primitives("unix", Unix_primitives);
}

