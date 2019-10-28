GCC=gcc
OPTIONS=-std=c99 -Werror -Wall
all: test_bst.c bst.o
	$(GCC) $(OPTIONS) bst.o test_bst.c -o test_bst

lib: bst.c bst.h
	$(GCC) $(OPTIONS) -c bst.c -o bst.o

test: all
	./test.sh

clean:
	rm bst.o test_bst out.txt
