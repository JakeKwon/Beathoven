# Beathoven: compiler Makefile
#  - builds and manages all compiler components

# Make sure ocamlbuild can find opam-managed packages: first run
# eval `opam config env`

# The Caml compilers
OCAMLBUILD = ocamlbuild
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLFLAGS = -use-ocamlfind -pkgs llvm
FLAGS = $(OCAMLFLAGS) -cflags -w,+a-4

# OBJS = utils.cmo parser.cmo scanner.cmo analyzer.cmo printer.cmo pythonizer.cmo generator.cmo beathoven.cmo
TARGET = beathoven


.PHONY : $(TARGET).native

$(TARGET).native :
	$(OCAMLBUILD) $(FLAGS) $(TARGET).native
	@mv $(TARGET).native $(TARGET)


.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
	rm -f $(TARGET) *.cm[iox] parser.ml parser.mli scanner.ml \
*.out *.diff *.orig *.output

all:
	cd ..; make all


# $(TARGET) : $(OBJS)
# 	$(OCAMLC) -g -o $(EXEC) $(OBJS)
#
# %.ml %.mli : %.mly # parser.ml parser.mli: parser.mly
# 	$(OCAMLYACC) -v $<
#
# %.ml : %.mll
# 	$(OCAMLLEX) $<
#
# %.cmo: %.ml
# 	$(OCAMLC) -c $<
#
# %.cmi: %.mli
# 	$(OCAMLC) -c $<
#
# %.cmx : %.ml
# 	ocamlfind ocamlopt -c -package llvm $<
#
# parser.cmi : ast.cmi
# parser.cmo : ast.cmi parser.cmi
# scanner.cmo : parser.cmi
# codegen.cmo : ast.cmi