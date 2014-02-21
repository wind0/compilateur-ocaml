set -e -u

rm -f *.cmi *.cmo

menhir --infer --explain parser.mly 
ocamllex lexer.mll 
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c main.ml
ocamlc -o main lexer.cmo parser.cmo main.cmo
