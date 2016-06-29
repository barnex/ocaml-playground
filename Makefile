all:
	ocamlfind ocamlc -thread -o a.out -linkpkg \
		-package core \
		-package graphics \
		-package unix \
		util.ml \
		phys.ml \
		main.ml \

.PHONY: test
test:
	ocamlfind ocamlopt -thread -o test.out -linkpkg \
		-package core \
		-package graphics \
		-package unix \
		util.ml \
		phys.ml \
		phys_test.ml
		./test.out

