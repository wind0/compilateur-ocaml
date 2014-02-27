open Printf
open AST

(*----------------utilities-------------*)
(*print un label dot*)
let print_label = fun chan name position ->
fprintf chan "%i[label=%s];\n" position name

(*print une arête dot*)
let print_edge = fun chan a1 a2 ->
fprintf chan "%i -- %i;\n" a1 a2

(*marre de réecrire ces deux lignes*)
let print_init = fun chan name father max ->
let position = max +1
in print_label chan name position; print_edge chan position father; position

(*print une liste d'elements grace à la fonction func*)
let rec print_list = fun elem_list chan father max func ->
match elem_list with
|[] -> max
|[h::t] -> let position = func h chan father max in print_list t chan father position func

let print_lister = fun name elem_list chan father max func ->
let position = print_init chan name father max
in print_list elem_list chan father max func

(*print l'id*)
let print_id = fun id chan father max ->
print_init chan id max father


let print_procedure = fun proc chan father max ->
let position = print_init chan "PROCEDURE" father max
in let position1 = print_id proc.proc_name chan position position
in let position2 = print_parameter proc.proc_parameter chan position position1
in print_block proc.proc_body chan position

and print_procedure_list = print_lister "FUNCTION_LIST"

(*FUCKING FUNCTIONS*)
and print_function = fun func chan father max ->
let position = print_init chan "FUNCTION" father max
in let position1 = print_id func.func_name chan position position
in let position2 = print_parameter func.func_parameter chan position position1
in let position3 = print_typ func.func_return_type chan position position2
in print_block func.func_body chan position 

and print_function_list = print_lister "FUNCTION_LIST"

(*print block*)
and print_block = fun block chan father max ->
let me = print_init chan "BLOCK" father max
in position2 = print_block_const block.constants chan me me
in let position3 = print_block_types block.types chan me position2
in let position4 = print_block_var block.variables chan me position3
in let position5 = print_procedure_list block.procedures chan me position4
in print_function_list block.functions chan me position5
 
(*principal*)
let print = fun ast file ->
let chan = open_out file
in let _ = fprintf chan "graph G {\n"
and me = 1
in let _ = print_label chan "PROGRAM" position
and position2 = print_id ast.prog_name chan me me
in print_block ast.prog_body chan me position2; fprintf chan "}"

