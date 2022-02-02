.PHONY: all
all:
	csc -o todo -O5 -d0 -strict-types todo.scm

.PHONY: debug
debug:
	csc -o todo -O0 -d3 -strict-types todo.scm
