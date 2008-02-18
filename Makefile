CFLAGS+=-g -Wall
LDFLAGS+=-g
VERSION=0.4
DISTDIR=mgod-$(VERSION)
DISTFILES=mgod.c Makefile README httpgate.c httpgate.cfg.h.sample
DISTOUT=mgod-$(VERSION).tar.gz

.PHONY:	all
all: mgod tags

mgod: mgod.c

.PHONY:	clean
clean:
		rm -f mgod.o
		rm -f mgod
		rm -rf $(DISTDIR)
		rm -f $(DISTOUT)
		rm -f tags

tags:
		ctags mgod.c

httpgate: httpgate.c httpgate.cfg.h
		$(CC) -o httpgate httpgate.c $(CFLAGS)

.PHONY: dist
dist:
		rm -rf $(DISTDIR)
		mkdir $(DISTDIR)
		cp $(DISTFILES) $(DISTDIR)
		tar czvf $(DISTOUT) $(DISTDIR)
