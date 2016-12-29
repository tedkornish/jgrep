COMPILER=ocamlbuild -I src -use-ocamlfind -use-menhir

bin:
	$(COMPILER) src/jgrep.native
	cp ./_build/src/jgrep.native ./jgrep

.PHONY: test coverage
test:
	$(COMPILER) -I test -I src -cflags -g test/jgrep_test.native
	OCAMLRUNPARAM=b ./_build/test/jgrep_test.native

coverage:
	$(COMPILER) -pkg bisect_ppx -I test -I src test/jgrep_test.native
	./_build/test/jgrep_test.native
	bisect-ppx-report -I _build/ -html coverage/ bisect*.out
	rm bisect*.out
