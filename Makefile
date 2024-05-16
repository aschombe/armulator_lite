all: 
	dune build
	@cp bin/main.exe arml

test:
	./arml -f test.s
