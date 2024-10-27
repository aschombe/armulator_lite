all:
	@echo "compiling ./arml..."
	@printf "\t"
	dune build bin/main.exe
	@cp bin/main.exe arml
	@echo "done!"

	@make pl

pl:
	@echo "compiling plugins..."
	@printf "\t"
	dune build bin/plagiarism.cmxs
	@cp _build/default/bin/plagiarism.cmxs plugins/plagiarism.cmxs
	@chmod +w plugins/plagiarism.cmxs
	
	@printf "\t"
	dune build bin/test_plugin.cmxs
	@cp _build/default/bin/test_plugin.cmxs plugins/test_plugin.cmxs
	@chmod +w plugins/test_plugin.cmxs
	@echo "done!"

test:
	./arml -f test.s

clean:
	rm arml plugins/plagiarism.cmxs plugins/test_plugin.cmxs