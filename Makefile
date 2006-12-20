CFLAGS+=-g -Wall
LDFLAGS+=-g

.PHONY:	all
all:	mgod tags

mgod:	mgod.c

.PHONY:	clean
clean:
		rm -f mgod.o
		rm -f mgod

tags:
		ctags mgod.c
