/* httpgate: HTTP gateway for mgod
 * Copyright (C) 2006-2008 Máté Nagy <mnagy@port70.net>
 * Available under: GPL (version 3), see file "COPYING"
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/wait.h>

/* settings: */
#include "httpgate.cfg.h"

char *search = NULL;
int rss = 0;
int rssgopher = 0;
int in_listview = 0;

#ifndef JQUERY_PAGER
#define JQUERY_PAGER 50
#endif

int links_on_page = JQUERY_PAGER;
int skip_links = 0;

#ifdef JQUERY
int jquery = 1;
#else
int jquery = 0;
#endif

/* run mgod using a selector, return file descriptor to read output from */
FILE *run_mgod(char *selector)
{
	int pfd[2];
	if(pipe(pfd) == -1) return NULL;

	pid_t cpid;
	cpid = fork();
	if(cpid == -1) return NULL;

	if(cpid == 0) {
		/* child */
		close(pfd[0]); /* close read end */
		dup2(pfd[1], 1); /* use write end for stdout */
		close(pfd[1]);

		/* modify arglist to have selector */
		char **a = mgod_args;
		while(a[3]) a++;

		if(search) {
			*a = (char *) malloc(strlen(selector) + strlen(search) + 2);
			if(!*a) abort();
			sprintf(*a, "%s\t%s", selector, search);
		} else *a = selector;

		a[2] = getenv("REMOTE_ADDR");
		if(!a[2]) a[2] = "unknown";

		/* run mgod */
		execv(mgod_exec, mgod_args);

		exit(1);
	} else {
		/* parent */
		close(pfd[1]); /* close write end */

		return fdopen(pfd[0], "r");
	}
}

/* output a html error message */
void html_error(char *msg)
{
	printf("<html><body><b>%s</b></body></html>", msg);
}

/* print html-escaped string */
char * htmlprint(char *s)
{
	char *p;
	for(p = s; *p; p++) {
		switch(*p) {
		case '&':
			printf("&amp;"); break;
		case '<':
			printf("&lt;"); break;
		case '>':
			printf("&gt;"); break;
		case '"':
			printf("&quot;"); break;
		default:
			putchar(*p);
		}
	}
	return p;
}

/* print url-escaped string */
void urlprint(char *s)
{
	char *p;
	for(p = s; *p; p++) {
		if(isalpha(*p) || isdigit(*p) || *p == '-' || *p == '_'
				|| *p == '.' || *p == '~' || *p == '/')
			putchar(*p);
		else printf("%%%02x", *p);
	}
}

/* urldecode string (in place) */
void urldecode(char *s)
{
	char *h = s;
	char *p = s;
	char hex[3];
	hex[2] = 0;
	while(*p) {
		if(*p == '%') {
			p++;
			if(!*p) return;
			hex[0] = *p;
			p++;
			if(!*p) return;
			hex[1] = *p;
			*h++ = strtol(hex, NULL, 16);
			p++;
		} else if(*p == '+') {
			*h++ = ' ';
			p++;
		} else *h++ = *p++;
	}
	*h++ = 0;
}

/* jquery mode - begin listview if needed */
void begin_listview()
{
	if(in_listview) return;
	printf("<ul data-role=\"listview\" data-inset=\"true\">\n");
	in_listview = 1;
}

/* jquery mode - end listview if needed */
void end_listview()
{
	if(!in_listview) return;
	printf("</ul>\n");
	in_listview = 0;
}

/* print dirlist header */
void dirlist_head(char type, char *sel, char *srch, char *body)
{
	if(rss) {
		printf("<?xml version=\"1.0\"?>\n");
		printf("<rss version=\"2.0\">\n");
		printf("<channel>\n");
		printf("<title>Gopher: ");
		htmlprint(sel);
		printf("</title>");
		printf("<link>");
		if(rssgopher) {
			if(strcmp(home_port, "70"))
				printf("gopher://%s:%s/%c", home_server, home_port, type);
			else
				printf("gopher://%s/%c", home_server, type);
			urlprint(sel);
			//htmlprint(sel);
		} else {
			htmlprint(urlbase);
			printf("?%c", type);
			urlprint(sel);
			//htmlprint(sel);
		}
		printf("</link>");
		printf("<generator>mgod httpgate</generator>\n");
	} else if(jquery) {
		printf("<!DOCTYPE html>\n");
		printf("<html>\n");
		printf("<head>\n");
		printf("<title>Gopher: ");
		htmlprint(sel);
		printf("</title>");
		printf("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n");
		printf("<link rel=\"stylesheet\" href=\"http://port70.net/~kzed/httpgate/1.0.css\" />\n");
		printf("<script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-1.6.4.min.js\"></script>\n");
		printf("<script type=\"text/javascript\" src=\"http://port70.net/~kzed/httpgate/1.0.js\"></script>\n");
		printf("<script type=\"text/javascript\" src=\"http://code.jquery.com/mobile/1.0/jquery.mobile-1.0.min.js\"></script>\n");
		printf("</head>\n<body %s>\n", body ? body : "");

		printf("<div data-role=\"page\">\n");
		printf("<div data-role=\"header\">\n");

		//urlprint(sel);
		printf("<h1>");
		if(sel[0]) {
			char *lastsep = strrchr(sel, '/');
			if(lastsep == NULL) {
				printf("<a href=\"?1\">");
			} else {
				*lastsep = 0;
				printf("<a href=\"?1");
				urlprint(sel);
				printf("\">");
				*lastsep = '/';
			}
			htmlprint(sel);
			printf("</a>");
		} else {
			printf("%s", home_server);
		}

		if(srch) {
			printf(" (");
			htmlprint(srch);
			printf(")");
		}

		printf("</h1>");

		printf("</h1></div><!-- /header -->\n");

		printf("<div data-role=\"content\">\n");
		printf("<div class=\"pager\">pager</div>\n");

	} else {
		printf("<html>\n");
		printf("<head>\n");
		printf("<title>Gopher: ");
		htmlprint(sel);
		printf("</title>");
		printf("</head>\n<body %s>\n", body ? body : "");
		if(strcmp(home_port, "70"))
			printf("<h1>gopher://%s:%s/%c", home_server, home_port, type);
		else
			printf("<h1>gopher://%s/%c", home_server, type);
		urlprint(sel);
		//htmlprint(sel);

		if(srch) {
			printf(" (");
			htmlprint(srch);
			printf(")");
		}

		printf("</h1><hr>\n<pre>");
	}
}

/* print dirlist footer */
void dirlist_foot(char type, char *sel, char *srch, int disp_links)
{
	if(rss) {
		printf("</channel></rss>");
	} else if(jquery) {
		end_listview();
		printf("<div class=\"pager\">pager</div>\n");
		printf("</div><!-- /content -->\n");

		if(srch == NULL && (skip_links != 0 || disp_links > links_on_page)) {
			int pages = 1 + ((skip_links + disp_links - 1) / links_on_page);
			int curpg = (skip_links / links_on_page);
			printf("<script type=\"text/javascript\">setPager(%d, %d, %d, '", curpg, pages, links_on_page);
			urlprint(sel);
			printf("')</script>\n");
		}

		printf("</div><!-- /page -->\n");
		printf("</body></html>\n");
	} else {
		printf("</pre>\n<hr>");
		printf("<a href=\"gopher://port70.net/1mgod\">mgod httpgate</a>");
		printf("</body></html>\n");
	}
}

/* print info row */
void row_info(char *desc)
{
	if(jquery) {
		end_listview();
		printf("<p>");
		htmlprint(desc);
		printf("</p>");
	} else if(!rss) {
		htmlprint(desc);
		putchar('\n');
	}
}

/* print error row */
void row_error(char *desc)
{
	if(jquery) {
		end_listview();
		printf("<p>");
		htmlprint(desc);
		printf("</p>");
	} else if(!rss) {
		printf("<b>!! ");
		htmlprint(desc);
		printf("</b>\n");
	}
}

/* print row prefix for type */
void typeprefix(char type)
{
	switch(type) {
	case '1':
		printf(" <b>/</b> "); break;
	case '7':
		printf(" <b>?</b> "); break;
	case 'I':
	case 'g':
		printf("[] "); break;
	default:
		printf("   ");
	}
}

/* print home link row */
void row_homelink(char type, char *desc, char *sel)
{
	if(jquery) {
		begin_listview();
		printf("<li");
		if(type == '1')
			printf(" data-theme=\"b\">");
		else
			printf(">");
		printf("<a href=\"?%c", type);
		urlprint(sel);
		//htmlprint(sel);
		if(type == '1' || type == '7') {
			printf("\">");
		} else {
			printf("\" target=\"new\" data-ajax=\"false\">");
		}
		typeprefix(type);
		printf(" ");
		htmlprint(desc);
		printf("</a>\n");
	} else if(rss) {
		printf("<item><title>");
		htmlprint(desc);
		printf("</title>");
		printf("<link>");
		if(rssgopher) {
			if(strcmp(home_port, "70"))
				printf("gopher://%s:%s/%c", home_server, home_port, type);
			else
				printf("gopher://%s/%c", home_server, type);
			urlprint(sel);
			//htmlprint(sel);
		} else {
			htmlprint(urlbase);
			printf("?%c", type);
			urlprint(sel);
			//htmlprint(sel);
		}
		printf("</link></item>");
	} else {
		typeprefix(type);
		printf("<a href=\"?%c", type);
		urlprint(sel);
		//htmlprint(sel);
		printf("\">");
		htmlprint(desc);
		printf("</a>\n");
	}
}

/* print external link row */
void row_extlink(char type, char *desc, char *sel, char *host, char *port)
{
	if(jquery) {
		begin_listview();
		printf("<li>");
		printf("<a href=\"gopher://");
		htmlprint(host);
		if(strcmp(port, "70")) {
			putchar(':');
			htmlprint(port);
		}
		printf("/%c", type);
		urlprint(sel);
//		htmlprint(sel);
		printf("\" rel=\"external\" >");
		htmlprint(desc);
		printf("</a>\n");
	} else if(rss) {
		printf("<item><title>");
		htmlprint(desc);
		printf("</title>");
		printf("<link>gopher://");
		htmlprint(host);
		if(strcmp(port, "70")) {
			putchar(':');
			htmlprint(port);
		}
		printf("/%c", type);
		urlprint(sel);
		//htmlprint(sel);
		printf("</link></item>");
	} else {
		typeprefix(type);
		printf("<a href=\"gopher://");
		htmlprint(host);
		if(strcmp(port, "70")) {
			putchar(':');
			htmlprint(port);
		}
		printf("/%c", type);
		urlprint(sel);
//		htmlprint(sel);
		printf("\">");
		htmlprint(desc);
		printf("</a>\n");
	}
}

/* print url redirect */
void row_url(char type, char *desc, char *url)
{
	if(jquery) {
		begin_listview();
		printf("<li>");
		printf("<a href=\"");
	//	urlprint(url);
		htmlprint(url);
		printf("\" rel=\"external\" >");
		htmlprint(desc);
		printf("</a>\n");
	} else if(rss) {
		printf("<item><title>");
		htmlprint(desc);
		printf("</title><link>");
		//urlprint(url);
		htmlprint(url);
		printf("</link></item>");
	} else {
		printf("<b>-&gt;</b> <a href=\"");
	//	urlprint(url);
		htmlprint(url);
		printf("\">");
		htmlprint(desc);
		printf("</a>\n");
	}
}

/* return last component of filename */
char * final_component(char *sel)
{
	char *sep = strrchr(sel, '/');
	if(sep == NULL) return sel; else return sep + 1;
}

/* output a directory list for a selector */
void do_dirlist(char type, char *sel)
{
	char line[512];
	int disp_links = 0;
	int links_to_skip = skip_links;

	if(rss) {
		printf("Content-Disposition: inline; filename*=");
		urlprint(final_component(sel));
		printf(".xml\n");
		printf("Content-type: application/rss+xml; charset=utf-8\n\n");
	} else {
		printf("Content-Disposition: inline; filename*=");
		urlprint(final_component(sel));
		printf(".html\n");
		printf("Content-type: text/html; charset=utf-8\n\n");
	}

	FILE *fp = run_mgod(sel);
	if(!fp) {
		html_error("Failed to run mgod");
		return;
	}

	dirlist_head(type, sel, search, NULL);

	while(fgets(line, 512, fp)) {
		char type = line[0];
		if(!type || type == '\n') continue;

		char *eol = strchr(line, '\r');
		if(eol) *eol = 0;
		eol = strchr(line, '\n');
		if(eol) *eol = 0;

		char *p = line + 1;

		/* grab next token */
		char *grab() {
			if(!p) return NULL;
			char *n = strchr(p, '\t');
			char *t;
			if(!n) {
				t = p;
				p = NULL;
				return t;
			}

			*n = 0;
			t = p;
			p = n + 1;
			return t;
		}

		char *desc = grab();
		if(!desc) continue;
		char *s = grab();
		if(!s) continue;
		char *host = grab();
		if(!host) continue;
		char *port = grab();
		if(!port) continue;

		if(jquery) {
			if(search == NULL) {
				if(links_to_skip > 0) {
					if(type != 'i' && type != '3') links_to_skip --;
					continue;
				}

				if(type != 'i' && type != '3') disp_links ++;
				if(disp_links > links_on_page) continue;
			}
		}

		if(type == 'i')
			row_info(desc);
		else if(type == '3')
			row_error(desc);
		else if(!strncmp("URL:", s, 4))
			row_url(type, desc, s + 4);
		else if(!strcmp(host, home_server) && !strcmp(port, home_port))
			row_homelink(type, desc, s);
		else
			row_extlink(type, desc, s, host, port);
	}

	dirlist_foot(type, sel, search, disp_links);

	fclose(fp);
}

/* ask for search string */
void do_searchform(char *sel)
{
	printf("Content-type: text/html; charset=utf-8\n\n");
	dirlist_head('7', sel, NULL, "onload=\"document.f.search.focus();\"");

	printf("Please enter search string:\n");
	printf("<form name=f method=GET><input type=\"hidden\" name=\"sel\" value=\"7");
	htmlprint(sel);
	printf("\"><input name=\"search\" size=\"40\">\n");
	printf("<input type=\"submit\" value=\"Search\"></form>");

	dirlist_foot('7', sel, NULL, 0);
}

/* check for string extension */
int hasext(char *str, char *ext)
{
	int sl = strlen(str);
	int el = strlen(ext);
	if(sl < el) return 0;
	return !strcasecmp(str+sl-el, ext);
}

/* output a file for a selector */
void do_file(char type, char *sel)
{
	FILE *fp = run_mgod(sel);
	if(!fp) {
		printf("Content-type: text/html; charset=utf-8\n\n");
		html_error("Failed to run mgod");
		return;
	}

	char *mime = "application/octet-stream";

	switch(type) {
	case 'h': mime = "text/html"; break;
	case 'g': mime = "image/gif"; break;
	case 'I':
		if(hasext(sel, ".jpg"))
			mime = "image/jpeg";
		else if(hasext(sel, ".gif"))
			mime = "image/gif";
		else if(hasext(sel, ".bmp"))
			mime = "image/x-ms-bmp";
		else
			mime = "image/png";
		break;

	case '0':
		if(jquery)
			mime = "text/html; charset=utf-8";
		else 
			mime = "text/plain";

	default:
		if(hasext(sel, ".tar.gz"))
			mime = "application/x-gtar";
		else if(hasext(sel, ".tgz"))
			mime = "application/x-gtar";
		else if(hasext(sel, ".tar"))
			mime = "application/x-tar";
		else if(hasext(sel, ".zip"))
			mime = "application/zip";
		else if(hasext(sel, ".doc"))
			mime = "application/msword";
		else if(hasext(sel, ".xls"))
			mime = "application/vnd.ms-excel";
		else if(hasext(sel, ".xlsx"))
			mime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
		else if(hasext(sel, ".rar"))
			mime = "application/rar";
		else if(hasext(sel, ".docx"))
			mime = "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
		else if(hasext(sel, ".pdf"))
			mime = "application/pdf";
		else if(hasext(sel, ".ppt"))
			mime = "application/vnd.ms-powerpoint";
		else if(hasext(sel, ".tif"))
			mime = "image/tiff";

		break;
	}

	printf("Content-Disposition: inline; filename*=");
	urlprint(final_component(sel));
	printf("\n");
	printf("Content-type: %s\n\n", mime);

	if(jquery && type == '0') {
		printf("<!DOCTYPE html>\n");
		printf("<html>\n");
		printf("<head>\n");
		printf("<title>Gopher: ");
		htmlprint(sel);
		printf("</title>");
		printf("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n");
		printf("<link rel=\"stylesheet\" href=\"http://port70.net/~kzed/httpgate/1.0.css\" />\n");
		printf("<script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-1.6.4.min.js\"></script>\n");
		printf("<script type=\"text/javascript\" src=\"http://port70.net/~kzed/httpgate/1.0.js\"></script>\n");
		printf("<script type=\"text/javascript\" src=\"http://code.jquery.com/mobile/1.0/jquery.mobile-1.0.min.js\"></script>\n");
		printf("</head>\n<body>\n");

		printf("<div data-role=\"page\">\n");
		printf("<div data-role=\"header\">\n");

		//urlprint(sel);
		printf("<h1>");
		htmlprint(sel);
		printf("</h1>");

		printf("</h1></div><!-- /header -->\n");

		printf("<div data-role=\"content\">\n<p>\n");
		char buf[512];
		char *line;
		while((line = fgets(buf, 512, fp))) {
			char *last = htmlprint(line);
			if(last > line) {
				if(*(last-1) == '\n') {
					printf("<br>");
				}
			}
		}
		printf("</p>\n");
		printf("</div><!-- /content -->\n");
		printf("</div><!-- /page -->\n");
		printf("</body></html>\n");
	} else {
		char buf[2048];
		int res;
		while((res = fread(buf, 1, 2048, fp)) > 0) {
			if(fwrite(buf, 1, res, stdout) != res) break;
		}
	}

	fclose(fp);
}

int main(int argc, char *argv[])
{
	mgod_args[0] = mgod_exec;

	char *q = getenv("QUERY_STRING");
	if(!q) return 0;

	char *sel;
	char type;

	if(!q[0]) {
		/* empty query */
		sel = q;
		type = '1';
	} else if(strstr(q, "sel=")) {
		/* interpret standard scheme.. */

		/* interpret an a=b style pair */
		void dopair(char *str)
		{
			char *eq = strchr(str, '=');
			if(!eq) return;

			char *n = str;
			*eq = 0;
			char *v = eq + 1;

			if(!strcmp(n, "sel")) {
				urldecode(v);
				if(v[0]) {
					type = v[0];
					sel = v+1;
				} else {
					type = '1';
					sel = v;
				}
			} else if(!strcmp(n, "search")) {
				urldecode(v);
				search = v;
				if(!search[0]) search = NULL;
			}
		}

		while(q && *q) {
			char *p = strchr(q, '&');
			if(p) {
				*p = 0;
				dopair(q);
				q = p + 1;
			} else {
				dopair(q);
				q = p;
			}
		}
	} else {
		/* interpret query string as selector */
		sel = q+1;
		type = q[0];

		char *colon = strrchr(sel, ':');
		if(colon != NULL) {
			*colon = 0;
			skip_links = atoi(colon + 1);
			if(skip_links < 0) skip_links = 0;
		}

		urldecode(sel);
	}

	if(type == 'r') {
		type = '1';
		rss = 1;
		rssgopher = 1;
	} else if(type == 'q') {
		type = '7';
		rss = 1;
		rssgopher = 1;
	} else if(type == 'R') {
		type = '1';
		rss = 1;
		rssgopher = 0;
	} else if(type == 'Q') {
		type = '7';
		rss = 1;
		rssgopher = 0;
	}

	if(type == '7' && !search)
		do_searchform(sel);
	else if(type == '1' || type == '7')
		do_dirlist(type, sel);
	else
		do_file(type, sel);

	return 0;
}
