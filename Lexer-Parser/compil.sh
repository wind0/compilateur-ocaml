set -e -u

rm -f module/*.cmi module/*.cmo
rm -f *.dot
rm -f *.png
ocamlc AST.mli
ocamlc -c error.ml
ocamlc -c astprinter.ml
ocamlc -c typecheck.ml
menhir -v --infer parserFinal.mly 
ocamllex lexer.mll 
ocamlc -c parserFinal.mli 
ocamlc -c lexer.ml 
ocamlc -c parserFinal.ml 
ocamlc -c main.ml 
mv *.cm* module
ocamlc -o main -I module error.cmo astprinter.cmo typecheck.cmo lexer.cmo parserFinal.cmo main.cmo
./main
dot -Tpng output.dot > output.png
