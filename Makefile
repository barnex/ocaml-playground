all:
	ocamlfind ocamlc -w @A-44 -thread -o a.out -linkpkg \
		-package core \
		-package graphics \
		-package unix \
		util.ml \
		phys.ml \
		main.ml \

.PHONY: test
test:
	ocamlfind ocamlc -g -w @A-44 -thread -o test.out -linkpkg \
		-package core \
		-package graphics \
		-package unix \
		util.ml \
		phys.ml \
		phys_test.ml
		./test.out

