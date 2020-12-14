SOURCES = flatestubs.c pdfflate.mli pdfflate.ml main.ml

RESULT = main

TARGETS = byte-code

OCAMLFLAGS = -bin-annot
OCAMLNCFLAGS = -g -safe-string -w -3
OCAMLBCFLAGS = -g -safe-string -w -3
OCAMLLDFLAGS = -g

all : $(TARGETS)

clean ::
	rm -rf doc foo foo2 out.txt *.cmt *.cmti

-include OCamlMakefile
