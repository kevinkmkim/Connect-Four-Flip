.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f c4f.zip
	zip -r c4f.zip . -x@exclude.lst

clean:
	dune clean
	rm -f c4f.zip

doc:
	dune build @doc

check_lines:
	dune clean
	cloc --by-file --include-lang=OCamlcloc --by-file --include-lang=OCaml .
