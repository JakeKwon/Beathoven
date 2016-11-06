
# Beathoven: compiler Makefile
#  - builds and manages all compiler components

# The Caml compilers
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
# OBJS = utils.cmo parser.cmo scanner.cmo analyzer.cmo printer.cmo pythonizer.cmo generator.cmo beathoven.cmo
OBJS = parser.cmo scanner.cmo beathoven.cmo
EXEC = beathoven


$(EXEC): $(OBJS)
	$(OCAMLC) -g -o $(EXEC) $(OBJS)

all:
	cd ..; make all


%.ml %.mli : %.mly # parser.ml parser.mli: parser.mly
	$(OCAMLYACC) -v $<

%.ml : %.mll
	$(OCAMLLEX) $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

parser.cmo : ast.cmi parser.cmi
scanner.cmo : parser.cmi 
parser.cmi : ast.cmi


.PHONY: clean
clean:
	rm -f $(EXEC) *.cm[iox] parser.ml parser.mli scanner.ml \
*.out *.diff *.orig *.output


# beathoven.cmo: utils.cmo scanner.cmo pythonizer.cmo printer.cmo parser.cmi \
#     generator.cmo analyzer.cmo
# beathoven.cmx: utils.cmx scanner.cmx pythonizer.cmx printer.cmx parser.cmx \
#     generator.cmx analyzer.cmx
# parser.cmx: ast.cmi parser.cmi
# scanner.cmx: parser.cmx ast.cmi
