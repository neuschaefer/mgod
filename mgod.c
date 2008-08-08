/* mgod: mini gopher server for inetd
 * Copyright (C) 2006-2008 Máté Nagy <mnagy@port70.net>
 * Available under: GPL (version 3), see file "COPYING"
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <strings.h>
#include <dirent.h>
#include <time.h>
#include <stdarg.h>
#include <ctype.h>
#include <netinet/in.h>
#include <arpa/inet.h>



/* size of maximum request string and config file line */
#define REQBUF 512
/* buffer size for file serving */
#define SERVBUF 2048

/* name of directory listing config file */
char DIRLIST[40] = ".gopher";

/* name of inheriting config file */
char INFCONFIG[40] = ".gopher.rec";

/* dirlist file extension */
#define DIRLISTEXT "g"

/* default server settings */
char * servername = "127.0.0.1";
int serverport = 70;
char * rootdir = "/var/gopher";
char * logfile = NULL;

/* file types */
char *filetypes[] = {
	/* images */
	"I", ".jpg.jpeg.jpe.png.bmp.",
	"g", ".gif.",
	/* binary formats */
	"9", ".gz.tgz.tar.zip.bz2.rar.pdf.mid.xm.mod.",
	";", ".avi.mp4.mpg.",
	"a", ".wav.mp3.au.",
	/* html */
	"h", ".htm.html.swf.",

	/* directory list */
	"1", "." DIRLISTEXT ".",

	NULL
};

/********************************** newhash */

typedef  unsigned long int  u4;   /* unsigned 4-byte type */
typedef  unsigned     char  u1;   /* unsigned 1-byte type */

/* The mixing step */
#define mix(a,b,c) \
{ \
	a=a-b;  a=a-c;  a=a^(c>>13); \
	b=b-c;  b=b-a;  b=b^(a<<8);  \
	c=c-a;  c=c-b;  c=c^(b>>13); \
	a=a-b;  a=a-c;  a=a^(c>>12); \
	b=b-c;  b=b-a;  b=b^(a<<16); \
	c=c-a;  c=c-b;  c=c^(b>>5);  \
	a=a-b;  a=a-c;  a=a^(c>>3);  \
	b=b-c;  b=b-a;  b=b^(a<<10); \
	c=c-a;  c=c-b;  c=c^(b>>15); \
}

/* The whole new hash function */
u4 hash(k, length, initval)
	register u1 *k;        /* the key */
	u4           length;   /* the length of the key in bytes */
	u4           initval;  /* the previous hash, or an arbitrary value */
{
	register u4 a,b,c;  /* the internal state */
	u4          len;    /* how many key bytes still need mixing */

	/* Set up the internal state */
	len = length;
	a = b = 0x9e3779b9;  /* the golden ratio; an arbitrary value */
	c = initval;         /* variable initialization of internal state */

	/*---------------------------------------- handle most of the key */
	while (len >= 12)
	{
		a=a+(k[0]+((u4)k[1]<<8)+((u4)k[2]<<16) +((u4)k[3]<<24));
		b=b+(k[4]+((u4)k[5]<<8)+((u4)k[6]<<16) +((u4)k[7]<<24));
		c=c+(k[8]+((u4)k[9]<<8)+((u4)k[10]<<16)+((u4)k[11]<<24));
		mix(a,b,c);
		k = k+12; len = len-12;
	}

	/*------------------------------------- handle the last 11 bytes */
	c = c+length;
	switch(len)              /* all the case statements fall through */
	{
		case 11: c=c+((u4)k[10]<<24);
		case 10: c=c+((u4)k[9]<<16);
		case 9 : c=c+((u4)k[8]<<8);
				 /* the first byte of c is reserved for the length */
		case 8 : b=b+((u4)k[7]<<24);
		case 7 : b=b+((u4)k[6]<<16);
		case 6 : b=b+((u4)k[5]<<8);
		case 5 : b=b+k[4];
		case 4 : a=a+((u4)k[3]<<24);
		case 3 : a=a+((u4)k[2]<<16);
		case 2 : a=a+((u4)k[1]<<8);
		case 1 : a=a+k[0];
				 /* case 0: nothing left to add */
	}
	mix(a,b,c);
	/*-------------------------------------------- report the result */
	return c;
}

/***********************************************************************/


/* maximum number of directory list entries to print */
int limit = -1;

/* the environment received in main() */
char **genvp;

/* output a single line, then \r\n EOL */
void outln(const char *str)
{
	fputs(str, stdout);
	fputs("\r\n", stdout);
}

/* print error message */
void errormsg(const char *e)
{
	printf("3%s\tfake\t(NULL)\t0\r\n.\r\n", e);
}

/* print info line */
void infoline(char *str)
{
	putchar('i');
	fputs(str, stdout);
	outln("\tfake\t(NULL)\t0");
}

/* append to log */
void logprintf(const char *format, ...)
{
	FILE *fp;
	va_list ap;

	if(!logfile) return;

	fp = fopen(logfile, "a");
	if(!fp) {
		perror("can't open log file");
		return;
	}

	va_start(ap, format);
	vfprintf(fp, format, ap);
	va_end(ap);

	fclose(fp);
}

/********************************** path */


/* structure for keeping current path */
typedef struct node node;
struct node {
	char *text;
	node *next;
};

/* current path */
node *path = NULL;

/* make a path node */
node *mkpn(const char *tx) {
	node *n = (node *) malloc(sizeof(node));
	if(!n) exit(1);
	n->text = strdup(tx);
	n->next = NULL;
	return n;
}


/********************************** directory processors */



/* structure for keeping directory processors */
typedef struct dirproc dirproc;
struct dirproc {
	char *name;
	char *cmd;
	int raw;
	dirproc *next;
};

/* directory processors */
dirproc *dirprocs = NULL;

/* record a directory processor */
void adddirproc(char *name, char *cmd)
{
	dirproc *n = (dirproc *) malloc(sizeof(dirproc));
	n->name = strdup(name);
	n->cmd = strdup(cmd);
	n->next = dirprocs;
	n->raw = 0;
	dirprocs = n;
}

/* look up dirproc command by name */
dirproc *getdirproc(char *name)
{
	dirproc *n;
	for(n = dirprocs; n; n=n->next) {
		if(!strcmp(n->name, name)) return n;
	}
	return NULL;
}

/* empty the list of dirprocs */
void cleardirprocs()
{
	dirproc *n, *m = NULL;
	for(n = dirprocs; n; n = n->next) {
		free(m);
		free(n->name);
		free(n->cmd);
		m = n;
	}
	free(m);
	dirprocs = NULL;
}



/********************************** file name aliases */



/* structure for keeping file name aliases */
typedef struct alias alias;
struct alias {
	u4 nhash;
	char *name;
	char *value;

	struct alias *left;
	struct alias *right;
};

alias *aliases = NULL;

/* create name alias node */
alias *mkalias(u4 nhash, const char *name, const char *value) {
	alias *n = (alias *) malloc(sizeof(alias));
	if(!n) exit(1);

	n->nhash = nhash;
	n->name = strdup(name);
	n->value = strdup(value);

	n->left = NULL;
	n->right = NULL;

	return n;
}

/* register name alias */
void regalias(char *name, const char *value) {
	alias *n;
	u4 nhash;

	if(!aliases) {
		aliases = mkalias(hash(name, strlen(name), 0), name, value);
		return;
	}

	n = aliases;
	nhash = hash(name, strlen(name), 0);

	for(;;) {
		if(n->nhash == nhash) {
			if(strcmp(n->name, name)) {
				if(n->left) {
					n = n->left;
					continue;
				}

				n->left = mkalias(nhash, name, value);
				return;
			} else {
				n->value = strdup(value);
				return;
			}
		} else if(nhash < n->nhash) {
			if(n->left) {
				n = n->left;
				continue;
			}

			n->left = mkalias(nhash, name, value);
			return;
		} else {
			if(n->right) {
				n = n->right;
				continue;
			}

			n->right = mkalias(nhash, name, value);
			return;
		}
	}
}

/* retrieve alias by name */
char * getalias(const char *name) {
	alias *n;
	u4 nhash;

	nhash = hash(name, strlen(name), 0);

	n = aliases;
	if(!n) return NULL;

	for(;;) {
		if(n->nhash == nhash) {
			if(strcmp(n->name, name)) {
				if(n->left) {
					n = n->left;
					continue;
				}

				return NULL;
			} else return n->value;
		} else if(nhash < n->nhash) {
			if(n->left) {
				n = n->left;
				continue;
			}

			return NULL;
		} else {
			if(n->right) {
				n = n->right;
				continue;
			}

			return NULL;
		}
	}
}

/***********************************************************************/

/* helper functions for directory listing */
int dirfilter(const struct dirent *a)
{
	if(a->d_name[0] == '.') return 0;
	return 1;
}


/* file list generation properties */
int dirsortmode = 0;
int genlist = 1;
int listrev = 0;
int blogmode = 0;
int dirfirst = 1;
int textsummary = 0;


/* callback function for directory sorting */
int dirsort(const struct dirent **a, const struct dirent **b)
{
	struct stat abuf, bbuf;

	if(stat((*a)->d_name, &abuf)) return 0;
	if(stat((*b)->d_name, &bbuf)) return 0;

	if(dirfirst) {
		if(S_ISDIR(abuf.st_mode) && !S_ISDIR(bbuf.st_mode)) return -1;
		if(!S_ISDIR(abuf.st_mode) && S_ISDIR(bbuf.st_mode)) return 1;
	}

	switch(dirsortmode) {
		case 1:
			if(abuf.st_mtime < bbuf.st_mtime) return -1;
			if(abuf.st_mtime > bbuf.st_mtime) return 1;

			return strverscmp((*a)->d_name, (*b)->d_name);

		default:
			return strverscmp((*a)->d_name, (*b)->d_name);
	}
}

/* print directory entry */
void printentry(char *e)
{
	struct stat stbuf;
	char *ext = NULL;
	node *no;
	char menuchar;
	char *desc;
	struct tm *mt;

	char *fil = e; // file system name
	int absolute = 0;

	if(e[0] == '/') {
		/* when given an absolute path, prepend server root... */
		fil = (char *) alloca(strlen(rootdir) + strlen(e));
		sprintf(fil, "%s%s", rootdir, e);
		absolute = 1;
		e ++;
	}

	if(stat(fil, &stbuf)) {
		errormsg("can't stat file");
		return;
	}

	if(!(stbuf.st_mode & S_IROTH)) {
		/* not readable by all; skip */
		return;
	}

	/* determine menu char */
	if(S_ISDIR(stbuf.st_mode)) {
		menuchar = '1';
	} else if(S_ISREG(stbuf.st_mode)) {

		ext = strrchr(e, '.');
		if(ext) {
			char **ft = filetypes;
			char extwdot[8];

			if(strlen(ext) > 7) {
				menuchar = '0';
			} else {
				strcpy(extwdot, ext);
				strcat(extwdot, ".");
				for(; *ft; ft ++) {
					char mch = *ft[0]; ft++;
					if(strcasestr(*ft, extwdot)) {
						menuchar = mch;
						break;
					}
				}

				if(!(*ft)) menuchar = '0';
			}

			ext ++; // skip dot
		} else {
			menuchar = '0';
		}

	} else {
		return;
	}

	/* whether we're cutting off the .g ... */
	int dotg = 0;

	/* determine description */
	desc = getalias(e);
	if(desc) {
		if(desc[0] == 0) return; /* desc aliased to empty; skip */
	} else {
		desc = e;

		/* cut off .g from extension */
		if(menuchar == '1' && ext && !strcasecmp(ext, DIRLISTEXT)) {
			dotg = 1;
			ext -= strlen(DIRLISTEXT);
			*ext = 0;
		}
	}

	putchar(menuchar);

	mt = localtime(&stbuf.st_mtime);

	if(blogmode) {
		printf("%04d-%02d-%02d ", mt->tm_year + 1900, mt->tm_mon + 1, mt->tm_mday);
	}

	printf("%s\t", desc);

	if(dotg) {
		/* replace dot in the .g */
		*ext = '.';
	}

	if(!absolute) {
		/* print path */
		for(no = path; no; no=no->next)
			printf("%s/", no->text);
	}

	printf("%s\t%s\t%d\r\n", e, servername, serverport);

	if(textsummary > 0 && menuchar == '0') {
		/* output text summary for plain text file */

		char buf[REQBUF];
		FILE *fp = fopen(fil, "r");
		if(fp) {
			int i = 0;
			int spccnt = 0;
			while(!feof(fp) && i < textsummary) {
				int c = fgetc(fp);
				if(isspace(c)) {
					c = ' ';
					spccnt ++;
					if(spccnt > 2) continue;
				} else spccnt = 0;

				if(isprint(c))
					buf[i++] = c;
			}
			buf[i] = 0;

			fclose(fp);

			if(i > 0) infoline(buf);
		}
	}

}


/* special command token separator */
#define SPECSEP " \t"

void readdirlist(FILE *fp);
void runextern(char *ext);

/* interpret bang command */
void speccmd(char *cmd)
{
	char *spec = strtok(cmd, SPECSEP);
	if(!spec) {
		errormsg("Empty ! command in config");
		return;
	}

	if(!strcmp(spec, "nolist")) {
		/* don't generate file list */
		genlist = 0;
	} else if(!strcmp(spec, "reverse")) {
		/* reverse sort */
		listrev = 1;
	} else if(!strcmp(spec, "mtime")) {
		/* sort by mtime */
		dirsortmode = 1;
	} else if(!strcmp(spec, "blog")) {
		/* include mtime before file name in menu */
		blogmode = 1;
	} else if(!strcmp(spec, "dirmixed")) {
		/* consider dirs as files while sorting */
		dirfirst = 0;
	} else if(!strcmp(spec, "summary")) {
		/* enable text summary */

		textsummary = 72;

		char *sumlen = strtok(NULL, SPECSEP);
		if(sumlen) textsummary = atoi(sumlen);
		if(textsummary > REQBUF-1) textsummary = REQBUF-1;
		if(textsummary < 0) textsummary = 0;
	} else if(!strcmp(spec, "limit")) {
		/* limit number of directory list entries */
		char *slimit = strtok(NULL, SPECSEP);
		if(slimit) limit = atoi(slimit); else limit = 20;
	} else if(!strcmp(spec, "include")) {
		/* include .gopher file */

		char *file = strtok(NULL, "");
		if(!file) {
			errormsg("!include without arg in config");
			return;
		}
		FILE *fp = fopen(file, "r");
		if(!fp) {
			errormsg("can't open !include file");
			return;
		}
		readdirlist(fp);
		fclose(fp);
	} else if(!strcmp(spec, "proc")) {
		/* include output from external processor */

		char *ext = strtok(NULL, "");
		if(!ext) {
			errormsg("!proc without arg in config");
			return;
		}
		runextern(ext);
	} else if(!strcmp(spec, "dirproc") || !strcmp(spec, "dirproc-raw")) {
		/* register directory processor */
		char *name = strtok(NULL, SPECSEP);
		if(!name) {
			errormsg("!dirproc without name arg");
			return;
		}
		char *cmd = strtok(NULL, "");
		if(!cmd) {
			errormsg("!dirproc without cmd arg");
			return;
		}
		adddirproc(name, cmd);
		if(!strcmp(spec, "dirproc-raw"))
			dirprocs->raw = 1;
	} else {
		errormsg("Invalid !command in config");
	}
}

/* process configuration file */
void readdirlist(FILE *fp)
{
	char buf[REQBUF];
	char *p;
	node *no;

	while(!feof(fp)) {
		if(!fgets(buf, REQBUF, fp)) break;
		if(ferror(fp)) break;
		if(ferror(stdout)) break;

		for(p=buf; *p; p++)
			if(*p == '\r' || *p == '\n')
				{ *p = 0; break; }

		switch(buf[0]) {
			/* verbatim */
			case ':':
				outln(buf+1);
				break;

			/* verbatim local link */
			case '.':
				fputs(buf+1, stdout);
				printf("\t%s\t%d\r\n", servername, serverport);
				break;

			/* local link, with path automatically prepended */
			case '`':
				p = strchr(buf+1, '\t');
				if(p) {
					*p = 0;
					printf("%s\t", buf+1);
					for(no = path; no; no=no->next)
						printf("%s/", no->text);
					printf("%s\t%s\t%d\r\n", p+1, servername, serverport);
				}
				break;

			/* special command */
			case '!':
				speccmd(buf + 1);
				break;

			/* comment */
			case '#':
				break;

			/* description alias */
			case '=':
				p = strchr(buf+1, '\t');
				if(*p) {
					*p = 0;
					regalias(p+1, buf+1);
				}
				break;

			case '"':
				/* verbatim info line */
				infoline(buf + 1);
				break;

			/* info line */
			default:
			/* no special beginning characters.
			 * Let's try to find tabs */
			{
				char *info = buf;
				char *sel = strchr(buf, '\t');
				char *serv = NULL;
				char *port = NULL;
				if(!sel) {
					/* just an infoline after all */
					infoline(buf);
				} else {
					/* has tabs; try to interpret somewhat similarly
					 * to bucktooth (fill in missing fields) */

					*sel++ = 0;
					serv = strchr(sel, '\t');
					if(serv) {
						*serv++ = 0;
						port = strchr(serv, '\t');
						if(port) *port++ = 0;
					}

					if(!info[0]) {
						/* missing info field */
						if(!serv) {
							/* if no serv specified, just do a printentry */
							if(!strncmp(sel, "URL:", 4)) {
								/* .. unless it begins with URL: */
								printf("h%s\t%s\t%s\t%d\r\n", sel+4, sel, servername, serverport);
							} else printentry(sel);
						} else {
							errormsg("known selector, but no info field on nonlocal serv");
						}
					} else {
						/* info is specified. */
						printf("%s\t", info);

						if(!serv && sel[0] != '/' && strncmp(sel, "URL:", 4)) {
							/* selector is relative and serv is local, print path */
							for(no = path; no; no=no->next)
								printf("%s/", no->text);
						}
						if(sel[0] == '/') sel ++;
						printf("%s\t%s\t%d\r\n", sel, serv ? serv : servername,
							port ? atoi(port) : (serv ? 70 : serverport));
					}
				}
			}
		}
	}
}

/* directory listing */
void dirlist()
{
	FILE *fp;
	int n, i;
	struct dirent **namelist;

	/* process configuration file */
	fp = fopen(DIRLIST, "r");
	if(fp) {
		readdirlist(fp);
		fclose(fp);
	}

	/* generate directory listing */
	if(genlist) {
		n = scandir(".", &namelist, dirfilter, (int (*) (const void *, const void *) ) dirsort);
		if(n < 0) {
			perror("scandir");
			exit(1);
		}

		if(listrev) {
			while(n--) {
				if(limit > -1) {
					if(limit == 0) break;
					limit --;
				}
				printentry(namelist[n]->d_name);
			}
		} else {
			if(limit > -1) n = limit;
			for(i=0; i<n; i++)
				printentry(namelist[i]->d_name);
		}
	}
}

/* serve a file */
void serve(char *fn)
{
	FILE *fp;
	int r;
	char buf[SERVBUF];
	struct stat stbuf;

	if(stat(fn, &stbuf)) {
		errormsg("can't stat file");
		exit(0);
	}

	if(!(stbuf.st_mode & S_IROTH)) {
		/* not everyone has read access */
		errormsg("access denied");
		exit(0);
	}

	fp = fopen(fn, "rb");
	if(!fp) {
		errormsg("couldn't open file");
		exit(0);
	}

	while((r = fread(buf, 1, SERVBUF, fp)) > 0) {
		fwrite(buf, r, 1, stdout);
	}

	fclose(fp);
}

/* try to run/include an external processor */
void runextern(char *cmd)
{
	FILE *fp = popen(cmd, "r");
	if(!fp) {
		errormsg("popen failed for runextern");
		exit(0);
	}
	readdirlist(fp);
	pclose(fp);
}

/* try to run/include a directory processor */
void rundirproc(dirproc *d, char *path)
{
	setenv("QUERY", path, 1);
	if(d->raw) {
		char *args[4] = { "sh", "-c", d->cmd, NULL };
		execv("/bin/sh", args);
		errormsg("execv failed");
		exit(0);
	} else {
		runextern(d->cmd);
	}
}

/* print a stub for redirecting gopher+ clients to the non-gopher+ menu */
void gopherplus_stub(char *req)
{
	/* this is a direct copy of the gopher.floodgap.com server notice */
	outln("+-1");
	printf("+INFO: 1Main menu (non-gopher+)\t\t%s\t%d\r\n", servername, serverport);
	outln("+ADMIN:");
	outln(" Admin: Server Administrator");
	outln(" Server:");
	outln("+VIEWS:");
	outln(" application/gopher+-menu: <512b>");
	outln("+ABSTRACT:");
	outln(" This gopher supports standard gopher access only. Use this");
	outln(" kludge to disable gopher+ client requests by your client.");
}

/* change to a directory; fail if directory is not readable by all */
void chdir_chk(const char *dir)
{
	struct stat stbuf;
	if(stat(dir, &stbuf)) {
		errormsg("failed to stat directory");
		exit(0);
	}

	if(!(stbuf.st_mode & S_IROTH)) {
		errormsg("access to directory denied");
		exit(0);
	}

	if(chdir(dir)) {
		errormsg("chdir failed");
		exit(0);
	}
}

/* process request */
void procreq(char *request)
{
	char *pathel;
	char *sep;
	struct stat stbuf;
	node *no = NULL;

	char *req = strsep(&request, "\t");
	char *search = strsep(&request, "\t");
	if(search) {
		/* search string is specified */
		if(!strcmp(search, "$")) {
			/* gopher+ menu attempted, print gopher+ redirect */
			gopherplus_stub(req);
			exit(0);
		}

		/* set search string in environment
		 * (for the possibly run external search processor */
		setenv("SEARCH", search, 1);
	}

	if(!strncmp(req, "URL:", 4)) {
		/* generate html redirect page */
		printf("<html><head>\r\n");
		printf("<meta http-equiv=\"refresh\" content=\"5;url=%s\"/>\r\n", req+4);
		printf("</head><body>\r\n");
		printf("You'll be redirected in 5 seconds to:\r\n");
		printf("<a href=\"%s\">%s</a>\r\n", req+4, req+4);
		printf("</body></html>\r\n");
		exit(0);
	}

	/* read .gopher.rec in server root if exists */
	if(!stat(INFCONFIG, &stbuf)) {
		FILE *fp = fopen(INFCONFIG, "r");
		if(fp) {
			readdirlist(fp);
			fclose(fp);
		}
	}

	/* follow request path */
	while((sep = strchr(req, '/'))) {
		pathel = req;
		*sep = 0;
		req = sep + 1;

		/* check for directory processors */
		{
			dirproc *d = getdirproc(pathel);
			if(d) {
				*sep = '/';
				rundirproc(d, pathel);
				exit(0);
			}
		}
		cleardirprocs();

		if(!strcmp(pathel, "..")) {
			errormsg("invalid path");
			exit(0);
		}

		/* skip empty elements */
		if(pathel[0] == 0)
			continue;

		chdir_chk(pathel);

		/* record path */
		if(no) {
			no->next = mkpn(pathel);
			no = no->next;
		} else {
			path = no = mkpn(pathel);
		}

		if(!stat(INFCONFIG, &stbuf)) {
			FILE *fp = fopen(INFCONFIG, "r");
			if(fp) {
				readdirlist(fp);
				fclose(fp);
			}
		}
	}


	/* final path element */
	if(req[0]) {
		{
			/* check for directory processors */
			dirproc *d = getdirproc(req);
			if(d) {
				rundirproc(d, req);
				exit(0);
			}
		}

		if(stat(req, &stbuf)) {
			errormsg("file not found");
			exit(0);
		}

		if(S_ISDIR(stbuf.st_mode)) {
			/* directory */
			chdir_chk(req);

			/* record path */
			if(no) {
				no->next = mkpn(req);
				no = no->next;
			} else {
				path = no = mkpn(req);
			}

			dirlist();
			exit(0);
		}

		if(S_ISREG(stbuf.st_mode)) {
			/* serve file */

			if(!strcmp(req, DIRLIST) || !strcmp(req, INFCONFIG)) {
				errormsg("access denied");
				exit(0);
			}

			char *ext;
			ext = strrchr(req, '.');
			if(ext) {
				ext++;
				if(!strcasecmp(ext, DIRLISTEXT)) {
					/* serve dirlist file */
					FILE *fp = fopen(req, "r");
					if(!fp) {
						errormsg("can't open file");
						exit(0);
					}

					readdirlist(fp);
					fclose(fp);
					exit(0);
				}
			}

			serve(req);
			exit(0);
		}

		errormsg("not regular file");
		exit(0);
	}

	dirlist();
	exit(0);
}

int main(int argc, char *argv[], char *envp[])
{
	char buf[REQBUF];
	char *p;
	int o;
	time_t tm;
	struct tm *mt;
	char *sel = NULL;
	char *peer = NULL;

	/* process arguments */
	while((o = getopt(argc, argv, "n:p:r:l:b:hs:P:")) != -1) {
		switch(o) {
			case 'n':
				servername = strdup(optarg);
				break;
			case 'p':
				serverport = atoi(optarg);
				break;
			case 'r':
				rootdir = strdup(optarg);
				break;
			case 'l':
				logfile = strdup(optarg);
				break;
			case 'b':
				strcpy(DIRLIST, optarg);
				strcpy(INFCONFIG, optarg);
				strcat(INFCONFIG, ".rec");
				break;
			case 's':
				sel = optarg;
				break;
			case 'P':
				peer = optarg;
				break;
			case '?':
			case 'h':
				fprintf(stderr, "mgod mini gopher server\n\n");
				fprintf(stderr, "-n name: set server name (default 127.0.0.1)\n");
				fprintf(stderr, "-p port: set server port (default 70)\n");
				fprintf(stderr, "-r dir: set root dir (default /var/gopher)\n");
				fprintf(stderr, "-l name: enable logging\n");
				fprintf(stderr, "-b name: dirlist basename (default .gopher)\n");
				fprintf(stderr, "-s sel: use this selector instead of reading from stdin\n");
				fprintf(stderr, "-P peer: specify peer name for incoming connection (for logging)\n");
				exit(1);
		}
	}

	if(!sel) {
		/* read request */
		if(fgets(buf, REQBUF, stdin) == NULL) {
			exit(1);
		}

		for(p=buf; *p; p++)
			if(*p == '\r' || *p == '\n')
				{ *p = 0; break; }

		sel = buf;
	}

	if(chdir(rootdir)) {
		perror("couldn't change to root dir");
		exit(1);
	}

	tm = time(NULL);
	mt = localtime(&tm);

	if(!peer) {
		/* if peer not specified, try to retrieve info from stdin socket */
		struct sockaddr_in addr;
		socklen_t i = sizeof(addr);
		int r = getpeername(0, (struct sockaddr *) &addr, &i);
		if(r == 0)
			peer = inet_ntoa(addr.sin_addr);
		else
			peer = "unknown";
	}

	setenv("PEER", peer, 1);

	logprintf("REQ\t%04d-%02d-%02d %02d:%02d:%02d\t%s\t%s\n",
			mt->tm_year + 1900, mt->tm_mon + 1, mt->tm_mday,
			mt->tm_hour, mt->tm_min, mt->tm_sec, peer,
			buf);

	genvp = envp;
	procreq(sel);
	return 0;
}
