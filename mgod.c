/* mgod 0.5
 * mini gopher server for inetd
 * by Mate Nagy <k-zed@hactar.net>
 * GPL VERSION 2
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

/* size of maximum request string and config file line */
#define REQBUF 512
/* buffer size for file serving */
#define SERVBUF 2048

/* maximum number of arguments (+1) for external processor */
#define MAXPROCARG 16

/* name of directory listing config file */
char DIRLIST[40] = ".gopher";

/* name of inheriting config file */
char INFCONFIG[40] = ".gopher.rec";

/* name of list of external processors */
#ifndef EXTPROC
#define EXTPROC ".search"
#endif

/* dirlist file extension */
#define DIRLISTEXT "g"

/* default server settings */
char * servername = "127.0.0.1";
int serverport = 70;
char * rootdir = "/var/gopher";
char * adminstring = "Frodo Gophermeister <fng@bogus.edu>";
char * logfile = NULL;

int limit = -1;

/* structure for keeping current path */
typedef struct node node;
struct node {
	char *text;
	node *next;
};

node *path = NULL;

node *mkpn(const char *tx) {
	node *n = (node *) malloc(sizeof(node));
	if(!n) exit(1);
	n->text = strdup(tx);
	n->next = NULL;
	return n;
}

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

/* environment */
char **genvp;

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

/* gopher+ mode */
int gopherplus = 0;
/* print item info instead of file serve */
int iteminfo = 0;
/* process external processor's output */
int managedextern = 0;

/* print error message */
void errormsg(const char *e)
{
	if(gopherplus)
		printf("--2\r\n%s\r\n", e);
	else
		printf("3%s\t\t\t\r\n.\r\n", e);
}

/* print info line */
void infoline(char *str)
{
	if(gopherplus)
		fputs("+INFO: ", stdout);
	putchar('i');
	fputs(str, stdout);
	fputs("\t-\t-\t-\r\n", stdout);
	if(gopherplus)
		printf("+ADMIN:\r\n Admin: %s\r\n", adminstring);
}

/* print directory entry */
void printentry(char *e)
{
	struct stat stbuf;
	char *ext = NULL;
	char *p, *q;
	node *no;
	char *ctype = NULL;
	char menuchar;
	char *desc;
	struct tm *mt;

	if(stat(e, &stbuf)) {
		errormsg("can't stat file");
		return;
	}

	/* determine menu char */
	if(S_ISDIR(stbuf.st_mode)) {
		menuchar = '1';
	} else if(S_ISREG(stbuf.st_mode)) {

		ext = strrchr(e, '.');
		if(ext) {
			ext++;

			/* images */
			if(!strcasecmp(ext, "gif")) {
				menuchar = 'g'; ctype="image/gif";
			} else if(!strcasecmp(ext, "jpg")) {
				menuchar = 'I'; ctype="image/jpeg";
			} else if(!strcasecmp(ext, "jpeg")) {
				menuchar = 'I'; ctype="image/jpeg";
			} else if(!strcasecmp(ext, "jpe")) {
				menuchar = 'I'; ctype="image/jpeg";
			} else if(!strcasecmp(ext, "png")) {
				menuchar = 'I'; ctype="image/png";
			} else if(!strcasecmp(ext, "bmp")) {
				menuchar = 'I'; ctype="image/x-ms-bmp";

			/* binary formats */
			} else if(!strcasecmp(ext, "gz")) {
				menuchar = '9'; ctype="application/x-gtar";
			} else if(!strcasecmp(ext, "tgz")) {
				menuchar = '9'; ctype="application/x-gtar";
			} else if(!strcasecmp(ext, "tar")) {
				menuchar = '9'; ctype="application/x-tar";
			} else if(!strcasecmp(ext, "zip")) {
				menuchar = '9'; ctype="application/zip";
			} else if(!strcasecmp(ext, "pdf")) {
				menuchar = '9'; ctype="application/pdf";
			} else if(!strcasecmp(ext, "html")) {
				menuchar = 'h'; ctype="text/html";
			} else if(!strcasecmp(ext, "htm")) {
				menuchar = 'h'; ctype="text/html";

			} else if(!strcasecmp(ext, DIRLISTEXT)) {
				menuchar = '1';
			} else {
				menuchar = '0';
				ctype="text/plain";
			}

		} else {
			menuchar = '0';
			ctype="text/plain";
		}

	} else {
		return;
	}

	/* whether we're cutting off the .g ... */
	int dotg = 0;

	/* determine description */
	desc = getalias(e);
	if(desc) {
		if(desc[0] == 0) return;
	} else {
		desc = e;

		/* cut off .g from extension */
		if(menuchar == '1' && ext && !strcasecmp(ext, "g")) {
			dotg = 1;
			ext --;
			*ext = 0;
		}
	}

	if(gopherplus)
		fputs("+INFO: ", stdout);

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

	/* print path */
	for(no = path; no; no=no->next)
		printf("%s/", no->text);

	printf("%s\t%s\t%d\t+\r\n", e, servername, serverport);

	if(gopherplus) {
		/* print admin block */
		fputs("+ADMIN:\r\n", stdout);
		printf(" Admin: %s\r\n", adminstring);
		p = ctime(&stbuf.st_mtime);
		q = strchr(p, '\n');
		if(q) *q = 0;
		
		printf(" Mod-Date: %s <%04d%02d%02d%02d%02d%02d>\r\n",
				p, mt->tm_year + 1900, mt->tm_mon + 1, mt->tm_mday,
				mt->tm_hour, mt->tm_min, mt->tm_sec);

		if(ctype) {
			printf("+VIEWS:\r\n %s: <%ldK>\r\n", ctype, stbuf.st_size / 1024);
		}
	}

	if(textsummary > 0 && menuchar == '0') {
		/* output text summary for plain text file */

		char buf[REQBUF];
		FILE *fp = fopen(e, "r");
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

/* guess a VIEWS block from a link */
void guessviews(const char *l)
{
	char *ctype = NULL;
	switch(l[0]) {
		case 'g': ctype = "image/gif"; break;
		case 'I': ctype = "image/png"; break;
		case '0': ctype = "text/plain"; break;
		case '1': ctype = "application/gopher+-menu"; break;
		case '7':
			return;
		case 'h':
				  ctype = "text/html"; break;
		default:
				  ctype = "application/octet-stream";
	}

	printf("+VIEWS:\r\n %s: <0K>\r\n", ctype);
}

#define SPECSEP " \t"

void readdirlist(FILE *fp);
void runextern(char *ext);

/* interpret bang command */
void speccmd(char *cmd)
{
	char *spec = strtok(cmd, SPECSEP);
	if(!spec) {
		fputs("iEmpty ! command in config\t\t\t\r\n", stdout);
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
			fputs("i!include without arg in config\t\t\t\r\n", stdout);
			return;
		}
		FILE *fp = fopen(file, "r");
		if(!fp) {
			fputs("ican't open !include file\t\t\t\r\n", stdout);
			return;
		}
		readdirlist(fp);
		fclose(fp);
	} else if(!strcmp(spec, "proc")) {
		/* include output from external processor */

		char *ext = strtok(NULL, "");
		if(!ext) {
			fputs("i!proc without arg in config\t\t\t\r\n", stdout);
			return;
		}
		runextern(ext);
	} else {
		fputs("iInvalid !command in config\t\t\t\r\n", stdout);
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
			/* verbatim full link */
			case ':':
				if(gopherplus)
					fputs("+INFO: ", stdout);
				fputs(buf+1, stdout);
				fputs("\t+\r\n", stdout);
				if(gopherplus) {
					printf("+ADMIN:\r\n Admin: %s\r\n", adminstring);
					guessviews(buf+1);
				}
				break;

			/* verbatim local link */
			case '.':
				if(gopherplus)
					fputs("+INFO: ", stdout);
				fputs(buf+1, stdout);
				printf("\t%s\t%d\t+\r\n", servername, serverport);
				if(gopherplus) {
					printf("+ADMIN:\r\n Admin: %s\r\n", adminstring);
					guessviews(buf+1);
				}
				break;

			/* local link, with path automatically prepended */
			case '`':
				if(gopherplus)
					fputs("+INFO: ", stdout);

				p = strchr(buf+1, '\t');
				if(p) {
					*p = 0;
					printf("%s\t", buf+1);
					for(no = path; no; no=no->next)
						printf("%s/", no->text);
					printf("%s\t%s\t%d\t+\r\n", p+1, servername, serverport);
				}

				if(gopherplus) {
					printf("+ADMIN:\r\n Admin: %s\r\n", adminstring);
					guessviews(buf+1);
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
				infoline(buf);
		}
	}
}

/* directory listing */
void dirlist()
{
	FILE *fp;
	int n, i;
	struct dirent **namelist;

	if(gopherplus)
		fputs("+-2\r\n", stdout);

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

	if(iteminfo) {
		fputs("+-2\n", stdout);
		printentry(fn);
		return;
	}

	if(stat(fn, &stbuf)) {
		errormsg("can't stat file");
		exit(0);
	}

	if(gopherplus) {
		printf("+%ld\r\n", stbuf.st_size);
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

/* try to run a search processor */
void runsearch(char *name, char *search)
{
	FILE *fp;
	FILE *fp2;
	char buf[REQBUF];
	char *p, *q;
	char *args[MAXPROCARG];
	char *exe;
	int argc=0;
	int status;
	int pfd[2];
	pid_t cpid;

	fp = fopen(EXTPROC, "r");
	if(!fp) {
		errormsg("extern definitions not found");
		exit(0);
	}

	/* read .search */
	while(!feof(fp)) {
		if(!fgets(buf, REQBUF, fp)) break;

		for(p=buf; *p; p++)
			if(*p == '\r' || *p == '\n')
				{ *p = 0; break; }

		p = buf;
		if(!(q = strchr(p, '\t'))) continue;
		*q = 0;
		if(strcmp(p, name)) continue;

		/* found entry, built argument list */

		p=q+1;

		exe = p;
		args[0] = exe; argc++;
		while((q = strchr(p, '\t'))) {
			p = q+1;
			*q=0;

			args[argc] = p; argc++;
			if(argc == MAXPROCARG - 2) {
				errormsg("too many arguments for extern");
				exit(0);
			}
		}

		args[argc] = search; argc++;

		if(gopherplus && !managedextern) {
			args[argc] = "$"; argc++;
		}

		args[argc] = NULL;


		/* invoke processor */

		if(managedextern) {
			/* execute managed processor */

			if(pipe(pfd) == -1) {
				perror("pipe");
				exit(1);
			}

			cpid = fork();
			if(cpid == -1) {
				perror("fork");
				exit(1);
			}

			if(cpid == 0) {
				/* child */
				close(pfd[0]); /* close unused read end */
				close(1); /* close stdout */
				dup2(pfd[1], 1); /* assign writing end to stdout */
				execve(exe, args, genvp);
				errormsg("error running extern");
				exit(1);
			} else {
				/* parent */
				close(pfd[1]); /* close unused write end */

				if(gopherplus)
					fputs("+-2\r\n", stdout);

				fp2 = fdopen(pfd[0], "r");
				readdirlist(fp2);
				fclose(fp2);

				waitpid(cpid, &status, WNOHANG);
				if(!WIFEXITED(status)) {
					kill(cpid, 9); /* kthxbye */
				}
			}

			exit(0);

		} else {
			/* unmanaged */
			execve(exe, args, genvp);
			errormsg("error running extern");
			exit(1);
		}
	}

	fclose(fp);
	errormsg("extern key not found");
	exit(0);
}

/* process request */
void procreq(char *req)
{
	char *pathel;
	char *search = NULL;
	char *sep, *p;
	struct stat stbuf;
	node *no = NULL;

	if((sep = strchr(req, '\t'))) {
		/* check for search string / gopher+ flags */
		search = sep+1;
		*sep = 0;

		p = strchr(search, '\t');
		if(p) {
			*p = 0;
			p++;
		} else p = search;

		if(!strcmp(p, "!")) {
			iteminfo = 1;
			gopherplus = 1;
		} else if(!strcmp(p, "$"))
			gopherplus = 1;
		else if(p[0] == '+')
			gopherplus = 1;
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

	if(!stat(INFCONFIG, &stbuf)) {
		FILE *fp = fopen(INFCONFIG, "r");
		if(fp) {
			readdirlist(fp);
			fclose(fp);
		}
	}

	/* follow path */
	while((sep = strchr(req, '/'))) {
		pathel = req;
		*sep = 0;
		req = sep + 1;

		if(!strcmp(pathel, "..")) {
			errormsg("invalid path");
			exit(0);
		}

		/* skip empty elements */
		if(pathel[0] == 0)
			continue;

		if(chdir(pathel)) {
			errormsg("file not found or chdir failed");
			exit(0);
		}

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
		if(req[0] == '!' || req[0] == '@') {
			/* external processor */
			if(req[0] == '@') managedextern = 1;
			char *p = strchr(req + 1, '?');
			if(p) {
				*p = 0;
				search = p + 1;
			}
			runsearch(req+1, search);
		}

		if(stat(req, &stbuf)) {
			errormsg("file not found");
			exit(0);
		}

		if(iteminfo) {
			/* instead of serving, print info */
			fputs("+-2\n", stdout);
			printentry(req);
			exit(0);
		}

		if(S_ISDIR(stbuf.st_mode)) {
			/* directory */
			if(chdir(req)) {
				errormsg("chdir failed");
				exit(0);
			}

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

					if(gopherplus)
						fputs("+-2\r\n", stdout);

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

	/* process arguments */
	while((o = getopt(argc, argv, "n:p:r:a:l:b:hs:")) != -1) {
		switch(o) {
			case 'a':
				adminstring = strdup(optarg);
				break;
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
			case '?':
			case 'h':
				fprintf(stderr, "mgod mini gopher server\n\n");
				fprintf(stderr, "-n name: set server name (default 127.0.0.1)\n");
				fprintf(stderr, "-p port: set server port (default 70)\n");
				fprintf(stderr, "-r dir: set root dir (default /var/gopher)\n");
				fprintf(stderr, "-a str: set admin string\n");
				fprintf(stderr, "-l name: enable logging\n");
				fprintf(stderr, "-b name: dirlist basename (default .gopher)\n");
				fprintf(stderr, "-s sel: use this selector instead of reading from stdin\n");
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

	struct sockaddr_in addr;
	socklen_t i = sizeof(addr);
	int r = getpeername(0, (struct sockaddr *) &addr, &i);
	char *peer = "unknown";
	if(r == 0)
		peer = inet_ntoa(addr.sin_addr);

	logprintf("REQ\t%04d-%02d-%02d %02d:%02d:%02d\t%s\t%s\n",
			mt->tm_year + 1900, mt->tm_mon + 1, mt->tm_mday,
			mt->tm_hour, mt->tm_min, mt->tm_sec, peer,
			buf);

	genvp = envp;
	procreq(sel);
	return 0;
}
