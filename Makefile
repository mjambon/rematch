VERSION = 0.0.0

FLAGS = -annot -g
PACKS = pcre

.PHONY: default all opt install doc test
default: all opt test_rematch META
all: rematch.cma
opt: rematch.cmxa rematch

test: test_rematch
	./test_rematch

ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

META: META.in Makefile
	sed -e 's:@@VERSION@@:$(VERSION):' META.in > META

SOURCES = \
  loc.ml messages.ml constants.ml charset.ml regexp.ml types.ml \
  parser.mli parser.ml lexer.ml \
  indent.mli indent.ml # transform.ml main.ml

RUNTIME_SOURCES = rematch.mli rematch.ml

MLI = $(filter %.mli, $(RUNTIME_SOURCES))
ML = $(filter %.ml, $(RUNTIME_SOURCES))
CMI = $(ML:.ml=.cmi)
CMO = $(ML:.ml=.cmo)
CMX = $(ML:.ml=.cmx)
O = $(ML:.ml=.o)

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

rematch: $(SOURCES) Makefile
	ocamlfind ocamlopt -o rematch $(FLAGS) \
		-package $(PACKS) -linkpkg \
		$(SOURCES)

rematch.cma: $(RUNTIME_SOURCES) Makefile
	ocamlfind ocamlc -a $(FLAGS) -o rematch.cma \
		-package "$(PACKS)" $(RUNTIME_SOURCES)

rematch.cmxa: $(RUNTIME_SOURCES) Makefile
	ocamlfind ocamlopt -a $(FLAGS) \
		-o rematch.cmxa -package "$(PACKS)" $(RUNTIME_SOURCES)

test_rematch.ml: test_rematch.mlx rematch
	./rematch -o test_rematch.ml test_rematch.mlx

test_rematch: rematch.cmxa test_rematch.ml
	ocamlfind ocamlopt -o test_rematch $(FLAGS) \
		-package "$(PACKS)" -linkpkg \
		rematch.cmxa test_rematch.ml

doc: doc/index.html
doc/index.html: $(MLI)
	mkdir -p doc
	ocamlfind ocamldoc -d doc -html -package $(PACKS) $(MLI)

install: META
	test ! -f rematch || cp rematch $(BINDIR)/
	test ! -f rematch.exe || cp rematch.exe $(BINDIR)/
	ocamlfind install rematch META \
          $$(ls $(MLI) $(CMI) $(CMO) $(CMX) $(O) \
             rematch.cma rematch.cmxa rematch.a)

uninstall:
	test ! -f $(BINDIR)/rematch || rm $(BINDIR)/rematch
	test ! -f $(BINDIR)/rematch.exe || rm $(BINDIR)/rematch.exe
	ocamlfind remove rematch

.PHONY: clean

clean:
	rm -f *.o *.a *.cm[ioxa] *.cmxa *~ *.annot
	rm -f rematch rematch.exe test_rematch test_rematch.exe META
	rm -f lexer.ml parser.mli parser.ml
	rm -rf doc
