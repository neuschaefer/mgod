CFLAGS+=-g -Wall
LDFLAGS+=-g

.PHONY:	all
all:	mgod

mgod:	mgod.c

.PHONY:	clean
clean:
		rm -f mgod.o
		rm -f mgod

