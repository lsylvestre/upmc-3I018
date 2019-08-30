SOURCES= utils.ml parseutils.ml ast.ml parser.mly lexer.mll prim.ml constants.ml  kast.ml expander.ml  bytecode.ml compiler.ml ccmain.ml
RESULT= compiler
LIBS = Unix # pour lancer vm depuis le compilateur

all: byte-code

-include OCamlMakefile


TAR=tar
ARCHIVE_DST=MLCompiler-${shell date +%F}

archive: Makefile OCamlMakefile *.ml *.mli *.mll *.mly
	mkdir $(ARCHIVE_DST)
	mkdir $(ARCHIVE_DST)/tests
	cp -f $? $(ARCHIVE_DST)
	cp -f tests/*.js $(ARCHIVE_DST)/tests/
	$(TAR) cvzf $(ARCHIVE_DST).tar.gz $(DST)
	rm -rf $(ARCHIVE_DST)

cleanall: clean
	rm -f *~
	rm -f testparser
	rm -f compiler
	rm -f ml_microjs*
	rm constants.c constants.h
