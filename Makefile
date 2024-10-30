all:
	@echo "compiling ./arml..."
	@printf "\t"
	dune build src/main.exe
	@cp src/main.exe arml
	@echo "done!"

	@make pl

pl:
	@echo "compiling plugins..."
	$(shell mkdir -p plugins)
	@printf "\t"
	dune build src/plagiarism.cmxs
	@cp _build/default/src/plagiarism.cmxs plugins/plagiarism.cmxs
	@chmod +w plugins/plagiarism.cmxs
	
	@printf "\t"
	dune build src/test_plugin.cmxs
	@cp _build/default/src/test_plugin.cmxs plugins/test_plugin.cmxs
	@chmod +w plugins/test_plugin.cmxs

	@printf "\t"
	dune build src/cc_validator.cmxs
	@cp _build/default/src/cc_validator.cmxs plugins/cc_validator.cmxs
	@chmod +w plugins/cc_validator.cmxs
	@echo "done!"

test:
	./arml -f test.s

clean:
	rm arml plugins/plagiarism.cmxs plugins/test_plugin.cmxs
