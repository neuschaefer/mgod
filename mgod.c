/* mgod 0.2
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
#define DIRLIST ".gopher"

/* name of list of external processors */
#define EXTPROC ".search"

/* default server settings */
char * servername = "127.0.0.1";
int serverport = 70;
char * rootdir = "/var/gopher";
char * adminstring = "Frodo Gophermeister <fng@bogus.edu>";
char * logfile = NULL;

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

int dirsortmode = 0;

int dirsort(const struct dirent **a, const struct dirent **b)
{
	struct stat abuf, bbuf;

	switch(dirsortmode) {
		case 1:
			if(stat((*a)->d_name, &abuf)) return 0;
			if(stat((*b)->d_name, &bbuf)) return 0;

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

/* file list generation properties */
int genlist = 1;
int listrev = 0;
int blogmode = 0;

/* print error message */
void errormsg(const char *e)
{
	if(gopherplus)
		printf("--2\r\n%s\r\n", e);
	else
		printf("3%s\t\t\t\r\n.\r\n", e);
}

/* print directory entry */
void printentry(char *e)
{
	struct stat stbuf;
	char *ext;
	char *p, *q;
	node *no;
	char *ctype = NULL;
	char *desc;
	struct tm *mt;

	if(stat(e, &stbuf)) {
		errormsg("can't stat file");
		return;
	}

	desc = getalias(e);
	if(desc) {
		if(desc[0] == 0) return;
	} else desc = e;

	if(gopherplus)
		fputs("+INFO: ", stdout);

	if(S_ISDIR(stbuf.st_mode)) {
		putchar('1');
	} else if(S_ISREG(stbuf.st_mode)) {

		ext = strrchr(e, '.');
		if(ext) {
			ext++;

			/* images */
			if(!strcasecmp(ext, "gif")) {
				putchar('g'); ctype="image/gif";
			} else if(!strcasecmp(ext, "jpg")) {
				putchar('I'); ctype="image/jpeg";
			} else if(!strcasecmp(ext, "jpeg")) {
				putchar('I'); ctype="image/jpeg";
			} else if(!strcasecmp(ext, "jpe")) {
				putchar('I'); ctype="image/jpeg";
			} else if(!strcasecmp(ext, "png")) {
				putchar('I'); ctype="image/png";
			} else if(!strcasecmp(ext, "bmp")) {
				putchar('I'); ctype="image/x-ms-bmp";

			/* binary formats */
			} else if(!strcasecmp(ext, "gz")) {
				putchar('9'); ctype="application/x-gtar";
			} else if(!strcasecmp(ext, "tgz")) {
				putchar('9'); ctype="application/x-gtar";
			} else if(!strcasecmp(ext, "tar")) {
				putchar('9'); ctype="application/x-tar";
			} else if(!strcasecmp(ext, "zip")) {
				putchar('9'); ctype="application/zip";

			} else if(!strcasecmp(ext, "html")) {
				putchar('h'); ctype="text/html";
			} else if(!strcasecmp(ext, "htm")) {
				putchar('h'); ctype="text/html";

			} else {
				putchar('0');
				ctype="text/plain";
			}

		} else {
			putchar('0');
			ctype="text/plain";
		}

	} else {
		return;
	}

	mt = localtime(&stbuf.st_mtime);

	if(blogmode) {
		printf("%04d-%02d-%02d ", mt->tm_year + 1900, mt->tm_mon + 1, mt->tm_mday);
	}

	printf("%s\t", desc);
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
}

/* guess a VIEWS block from a link */
void guessviews(const char *l)
{
	char *ctype = NULL;
	switch(l[0]) {
		case 'g': ctype = "image/gif"; break;
		case 'I': ctype = "image/png"; break;
		case '0': ctype = "text/plain"; break;
		case '1':
		case '7':
				  break;
		case 'h':
				  ctype = "text/html"; break;
		default:
				  ctype = "application/octet-stream";
	}

	printf("+VIEWS:\r\n %s: <0K>\r\n", ctype);
}

/* process configuration file */
void readdirlist(FILE *fp) {
	char buf[REQBUF];
	char *p, *cmd;

	while(!feof(fp)) {
		if(!fgets(buf, REQBUF, fp)) break;
		if(ferror(fp)) break;
		if(ferror(stdout)) break;

		for(p=buf; *p; p++)
			if(*p == '\r' || *p == '\n')
				{ *p = 0; break; }

		switch(buf[0]) {
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

			case '!':
				cmd = buf+1;
				if(!strcmp(cmd, "nolist")) {
					genlist = 0;
				} else if(!strcmp(cmd, "reverse")) {
					listrev = 1;
				} else if(!strcmp(cmd, "mtime")) {
					dirsortmode = 1;
				} else if(!strcmp(cmd, "blog")) {
					blogmode = 1;
				} else {
					fputs("iInvalid !command in config\t\t\t\r\n", stdout);
				}
				break;

			case '#':
				break;

			case '=':
				p = strchr(buf+1, '\t');
				if(*p) {
					*p = 0;
					regalias(p+1, buf+1);
				}
				break;

			default:
				if(gopherplus)
					fputs("+INFO: ", stdout);
				putchar('i');
				fputs(buf, stdout);
				fputs("\t-\t-\t-\r\n", stdout);
				if(gopherplus)
					printf("+ADMIN:\r\n Admin: %s\r\n", adminstring);
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
			while(n--)
				printentry(namelist[n]->d_name);
		} else {
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

/* try to run an external processor */
void runextern(char *name, char *search)
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
		errormsg("extern not found");
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
	errormsg("extern not found");
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
	}

	/* final path element */
	if(req[0]) {
		if(req[0] == '!' || req[0] == '@') {
			/* external processor */
			if(req[0] == '@') managedextern = 1;
			runextern(req+1, search);
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

	/* process arguments */
	while((o = getopt(argc, argv, "n:p:r:a:l:h")) != -1) {
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
			case '?':
			case 'h':
				fprintf(stderr, "mgod mini gopher server\n\n");
				fprintf(stderr, "-n name: set server name (default 127.0.0.1)\n");
				fprintf(stderr, "-p port: set server port (default 70)\n");
				fprintf(stderr, "-r dir: set root dir (default /var/gopher)\n");
				fprintf(stderr, "-a str: set admin string\n");
				fprintf(stderr, "-l name: enable logging\n");
				exit(1);
		}
	}

	/* read request */
	if(fgets(buf, REQBUF, stdin) == NULL) {
		exit(1);
	}

	for(p=buf; *p; p++)
		if(*p == '\r' || *p == '\n')
			{ *p = 0; break; }

	if(chdir(rootdir)) {
		perror("couldn't change to root dir");
		exit(1);
	}

	tm = time(NULL);
	mt = localtime(&tm);
	logprintf("REQ %04d-%02d-%02d %02d:%02d:%02d '%s'\n",
			mt->tm_year + 1900, mt->tm_mon + 1, mt->tm_mday,
			mt->tm_hour, mt->tm_min, mt->tm_sec,
			buf);

	genvp = envp;
	procreq(buf);
	return 0;
}

