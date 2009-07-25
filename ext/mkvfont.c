/*
 *	MKVFONT -- create vector font files from VF descriptions.
 *	By Nils M Holm, 1995,2009
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#define BUFLEN	1024

FILE	*In, *Out;

void usage(void) {
	fprintf(stderr, "Usage: mkvfont x y VFD-file VF-file\n");
	exit(1);
}

int getnum(void) {
	int	c, v;

	while (1) {
		c = getc(In);
		if (c != ' ' && c != '\t' && c != '\r' && c != '\n')
			break;
	}
	v = 0;
	while (isdigit(c)) {
		v = v * 10 + c - '0';
		c = getc(In);
	}
	return v;
}

int main(int argc, char **argv) {
	int	i, j, k, buf[256], c, x, y;
	char	buffer[256];
	char	*infile, *outfile;

	if (argc < 4) usage();
	x = atoi(argv[1]);
	y = atoi(argv[2]);
	infile = argv[3];
	outfile = argv[4];
	if ((In = fopen(infile, "r")) == NULL) {
		fprintf(stderr, "mkvfont: %s: cannot open\n", infile);
		exit(1);
	}
	if ((Out = fopen(outfile, "w")) == NULL) {
		fprintf(stderr, "mkvfont: %s: cannot create\n", outfile);
		exit(1);
	}
	fprintf(Out, "%s", "VF11");
	fprintf(Out, "%c", x);
	fprintf(Out, "%c", y);
	for (i=32; i<128; i++) {
		while (1) {
			c = getc(In);
			if (c == ';' || c == '\n')
				fgets(buffer, 256, In);
			else
				break;
		}
		if (feof(In)) break;
		buf[0] = getnum();
		for (j=0; j<buf[0]; j++) {
			k = getnum();
			buf[j+1] = (k >= 100)? 0x80: 0;
			if (k>=100) k = k-100;
			buf[j+1] = buf[j+1] | ((k/10)<<4);
			while (k>=10) k = k-10;
			buf[j+1] = buf[j+1] | k;
		}
		for (k=0; k<j+1; k++) fputc(buf[k], Out);
	}
	fclose(In);
	fclose(Out);
	return 0;
}
