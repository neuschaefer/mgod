/* mgod 0.1
 * mini gopher server for inetd
 * by Mate Nagy <k-zed@hactar.net>
 * GPL VERSION 2
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <strings.h>
#include <dirent.h>


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

/* environment */
char **genvp;

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

/* print directory entry */
void printentry(const char *e)
{
	struct stat stbuf;
	char *ext;
	node *no;

	if(stat(e, &stbuf)) {
		printf("ican't stat '%s'\t\t\r\n", e);
		return;
	}

	if(S_ISDIR(stbuf.st_mode)) {
		putchar('1');
	} else if(S_ISREG(stbuf.st_mode)) {

		ext = strrchr(e, '.');
		if(ext) {
			ext++;

			/* images */
			if(!strcasecmp(ext, "gif"))
				putchar('g');
			else if(!strcasecmp(ext, "jpg"))
				putchar('I');
			else if(!strcasecmp(ext, "jpeg"))
				putchar('I');
			else if(!strcasecmp(ext, "jpe"))
				putchar('I');
			else if(!strcasecmp(ext, "png"))
				putchar('I');
			else if(!strcasecmp(ext, "bmp"))
				putchar('I');

			/* binary formats */
			else if(!strcasecmp(ext, "gz"))
				putchar('9');
			else if(!strcasecmp(ext, "tgz"))
				putchar('9');
			else if(!strcasecmp(ext, "tar"))
				putchar('9');
			else if(!strcasecmp(ext, "zip"))
				putchar('9');

			else if(!strcasecmp(ext, "html"))
				putchar('h');
			else if(!strcasecmp(ext, "htm"))
				putchar('h');

			else putchar('0');

		} else putchar('0');

	} else {
		return;
	}

	printf("%s\t", e);
	/* print path */
	for(no = path; no; no=no->next)
		printf("%s/", no->text);

	printf("%s\t%s\t%d\t+\r\n", e, servername, serverport);
}

/* directory listing */
void dirlist()
{
	FILE *fp;
	char *p;
	char buf[REQBUF];
	char *cmd;
	int genlist = 1;
	int listrev = 0;
	int n, i;
	struct dirent **namelist;

	/* process configuration file */
	fp = fopen(DIRLIST, "r");
	if(fp) {
		while(!feof(fp)) {
			if(!fgets(buf, REQBUF, fp)) break;
			for(p=buf; *p; p++)
				if(*p == '\r' || *p == '\n')
					{ *p = 0; break; }

			switch(buf[0]) {
				case ':':
					fputs(buf+1, stdout);
					fputs("\t+\r\n", stdout);
					break;

				case '.':
					fputs(buf+1, stdout);
					printf("\t%s\t%d\t+\r\n", servername, serverport);
					break;

				case '!':
					cmd = buf+1;
					if(!strcmp(cmd, "nolist")) {
						genlist = 0;
					} else if(!strcmp(cmd, "reverse")) {
						listrev = 1;
					} else if(!strcmp(cmd, "mtime")) {
						dirsortmode = 1;
					} else {
						fputs("iInvalid !command in config\t\t\t\r\n", stdout);
					}
					break;

				case '#':
					break;

				default:
					putchar('i');
					fputs(buf, stdout);
					fputs("\t-\t-\t-\r\n", stdout);
			}
		}

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

	fputs(".\r\n", stdout);
}

/* serve a file */
void serve(const char *fn)
{
	FILE *fp;
	int r;
	char buf[SERVBUF];
	fp = fopen(fn, "rb");
	if(!fp) {
		printf("3couldn't open file\t\t\t\r\n.\r\n");
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
	char buf[REQBUF];
	char *p, *q;
	char *args[MAXPROCARG];
	char *exe;
	int argc=0;

	fp = fopen(EXTPROC, "r");
	if(!fp) {
		printf("3Extern not found\t\t\t\r\n.\r\n");
		exit(0);
	}

	while(!feof(fp)) {
		if(!fgets(buf, REQBUF, fp)) break;

		for(p=buf; *p; p++)
			if(*p == '\r' || *p == '\n')
				{ *p = 0; break; }

		p = buf;
		if(!(q = strchr(p, '\t'))) continue;
		*q = 0;
		if(strcmp(p, name)) continue;

		p=q+1;

		exe = p;
		args[0] = exe; argc++;
		while((q = strchr(p, '\t'))) {
			p = q+1;
			*q=0;

			args[argc] = p; argc++;
			if(argc == MAXPROCARG - 1) {
				printf("3Too many arguments for extern\t\t\t\r\n.\r\n");
				exit(0);
			}
		}

		args[argc] = search; argc++;
		args[argc] = NULL;

		execve(exe, args, genvp);
	}

	fclose(fp);
	printf("3Extern not found\t\t\t\r\n.\r\n");
	exit(0);
}

/* process request */
void procreq(char *req)
{
	char *pathel;
	char *search = NULL;
	char *sep;
	struct stat stbuf;
	node *no = NULL;

	if((sep = strchr(req, '\t'))) {
		search = sep+1;
		*sep=0;
	}

	while((sep = strchr(req, '/'))) {
		pathel = req;
		*sep = 0;
		req = sep + 1;

		if(!strcmp(pathel, "..")) {
			printf("3invalid path\t\t\t\r\n.\r\n");
			exit(0);
		}

		if(chdir(pathel)) {
			printf("3'%s' not found\t\t\t\r\n.\r\n", pathel);
			exit(0);
		}

		if(no) {
			no->next = mkpn(pathel);
			no = no->next;
		} else {
			path = no = mkpn(pathel);
		}
	}

	if(req[0]) {
		if(req[0] == '!') {
			runextern(req+1, search);
		}

		if(stat(req, &stbuf)) {
			printf("3not found\r\n.\r\n");
			exit(0);
		}

		if(S_ISDIR(stbuf.st_mode)) {
			if(chdir(req)) {
				printf("3'%s' not found\t\t\t\r\n.\r\n", req);
				exit(0);
			}

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
			serve(req);
			exit(0);
		}

		printf("3not regular file\t\t\t\r\n.\r\n");
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

	while((o = getopt(argc, argv, "n:p:r:h")) != -1) {
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
			case '?':
			case 'h':
				fprintf(stderr, "mgod mini gopher server\n\n");
				fprintf(stderr, "-n: set server name (default 127.0.0.1)\n");
				fprintf(stderr, "-p: set server port (default 70)\n");
				fprintf(stderr, "-r: set root dir (default /var/gopher)\n");
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

	genvp = envp;
	procreq(buf);
	return 0;
}

