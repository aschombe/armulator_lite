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
	dune build bin/test_plugin.cmxs
	@cp _build/default/bin/test_plugin.cmxs test_plugin.cmxs
	@chmod +w test_plugin.cmxs
	
	@printf "\t"
	dune build bin/my_plugin.cmxs
	@cp _build/default/bin/my_plugin.cmxs my_plugin.cmxs
	@chmod +w my_plugin.cmxs
	@echo "done!"

test:
	./arml -f test.s

clean:
	rm arml my_plugin.cmxs test_plugin.cmxs