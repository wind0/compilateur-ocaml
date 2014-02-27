open Printf
open AST

(*----------------utilities-------------*)
(*print un label dot*)
let print_label = fun chan name position ->
fprintf chan "%i[label=%s];\n" position name

(*print une arête dot*)
let print_edge = fun chan a1 a2 ->
fprintf chan "%i -- %i;\n" a1 a2



(*print l'id*)
let print_id = fun id chan father max ->
let position = max +1
in let _ = print_label chan id position
and _ = print_edge chan position father
in position

(*print block*)
let print_block block chan father max ->
let me = max+1
in let _ = print_label chan "BLOCK" position; print_edge me father
and position2 = print_block_const block.constants chan me me
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

