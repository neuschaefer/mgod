CFLAGS+=-g -Wall
LDFLAGS+=-g
VERSION=0.4
DISTDIR=mgod-$(VERSION)
DISTFILES=mgod.c Makefile rss.awk README
DISTOUT=mgod-$(VERSION).tar.gz

.PHONY:	all
all:	mgod tags

mgod:	mgod.c

.PHONY:	clean
clean:
		rm -f mgod.o
		rm -f mgod
		rm -rf $(DISTDIR)
		rm -f $(DISTOUT)

tags:
		ctags mgod.c

.PHONY: dist
dist:
		rm -rf $(DISTDIR)
		mkdir $(DISTDIR)
		cp $(DISTFILES) $(DISTDIR)
		tar czvf $(DISTOUT) $(DISTDIR)
