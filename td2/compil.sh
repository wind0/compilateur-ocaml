set -e -u

rm -f module/*.cmi module/*.cmo

ocamlc -c error.ml
menhir -v --infer --explain --dump parser.mly 
ocamllex lexer.mll 
ocamlc -c parser.mli 
ocamlc -c lexer.ml 
ocamlc -c parser.ml 
ocamlc -c main.ml 
mv *.cm* ./module/
ocamlc -o main -I module
