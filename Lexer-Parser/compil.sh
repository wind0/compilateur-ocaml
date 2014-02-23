set -e -u
rm -f module/*.cm*
rm -f *.cmi *.cmo

menhir -v --infer --explain --dump parser.mly 
ocamllex lexer.mll 
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c main.ml
mv *.cm* module
rm lexer.ml parser.mli parser.ml
ocamlc -o main -I module lexer.cmo parser.cmo main.cmo
