open Printf
open AST


let print_id = fun id chan father max ->
let position = max +1
in let _ = fprintf chan "%i[label=%s];\n" position id
and _ = fprintf chan "%i -- %i;" position father
in position

let print = fun ast file ->
let chan = open_out file
in let _ = fprintf chan "graph G {\n"
and position = 1
in let _ = fprintf chan "%i[label=PROGRAM];\n" position
and position2 = print_id ast.prog_name chan position position
in print_block ast.prog_body file position position2; fprintf chan "}"

