set -e -u

rm -f module/*.cmi module/*.cmo
rm -f *.dot
rm -f *.png
ocamlc AST.mli
ocamlc -c error.ml
ocamlc -c astprinter.ml
ocamlc -c typecheck.ml
menhir -v --infer parserHELIX.mly 
ocamllex lexer.mll 
ocamlc -c parserHELIX.mli 
ocamlc -c lexer.ml 
ocamlc -c parserHELIX.ml 
ocamlc -c main.ml 
mv *.cm* module
ocamlc -o main -I module error.cmo astprinter.cmo typecheck.cmo lexer.cmo parserHELIX.cmo main.cmo
./main
dot -Tpng foulemoila.dot > output.png
