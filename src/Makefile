
# Beathoven: compiler Makefile
#  - builds and manages all compiler components

OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
# OBJS = utils.cmo parser.cmo scanner.cmo analyzer.cmo printer.cmo pythonizer.cmo generator.cmo beathoven.cmo
OBJS = parser.cmo scanner.cmo beathoven.cmo

default: beathoven

beathoven: $(OBJS)
	$(OCAMLC) -g -o beathoven $(OBJS)

all:
	cd ..; make all

scanner.ml: scanner.mll
	$(OCAMLLEX) scanner.mll

parser.ml parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

.PHONY: clean
clean:
	rm -f beathoven scanner.ml parser.ml parser.mli *.cmo *.cmi

# beathoven.cmo: utils.cmo scanner.cmo pythonizer.cmo printer.cmo parser.cmi \
#     generator.cmo analyzer.cmo
# beathoven.cmx: utils.cmx scanner.cmx pythonizer.cmx printer.cmx parser.cmx \
#     generator.cmx analyzer.cmx

parser.cmo: ast.cmi parser.cmi
parser.cmx: ast.cmi parser.cmi

scanner.cmo: parser.cmi ast.cmi
scanner.cmx: parser.cmx ast.cmi

parser.cmi: ast.cmi
