FLAGS=-I src -use-menhir -use-ocamlfind
COMPILER=rebuild $(FLAGS)

bin:
	$(COMPILER) src/jf.native
	cp ./_build/src/jf.native ./jf

.PHONY: test coverage
test:
	$(COMPILER) -I test -I src -cflags -g test/jf_test.native
	OCAMLRUNPARAM=b ./_build/test/jf_test.native

coverage:
	$(COMPILER) -pkg bisect_ppx -I test -I src test/jf_test.native
	./_build/test/jf_test.native
	bisect-ppx-report -I _build/ -html coverage/ bisect*.out
	rm bisect*.out
