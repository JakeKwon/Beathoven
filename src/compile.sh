echo "create scanner.ml"
ocamllex scanner.mll
echo "create parser.ml and parser.mli"
ocamlyacc parser.mly
echo "compile AST types"
ocamlc -c ast.mli
echo "compile parser types"
ocamlc -c parser.mli
echo "compile the scanner"
ocamlc -c scanner.ml
echo "compile the parser"
ocamlc -c parser.ml
