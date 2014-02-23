set -e -u

rm -f *.cmi *.cmo

menhir -v --infer --explain --dump parser.mly 
ocamllex lexer.mll 
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c main.ml
ocamlc -o main lexer.cmo parser.cmo main.cmo
