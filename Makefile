all:
	@echo "compiling ./arml..."
	@printf "\t"
	dune build bin/main.exe
	@cp bin/main.exe arml
	@echo "done!"

	@make plugins

plugins:
	@echo "compiling plugins..."
	@printf "\t"
	dune build bin/plagiarism.cmxs
	@cp _build/default/bin/plagiarism.cmxs plagiarism.cmxs
	@chmod +w plagiarism.cmxs
	
	@printf "\t"
	dune build bin/test_plugin.cmxs
	@cp _build/default/bin/test_plugin.cmxs test_plugin.cmxs
	@chmod +w test_plugin.cmxs
	@echo "done!"

test:
	./arml -f test.s

clean:
	rm arml plagiarism.cmxs test_plugin.cmxs