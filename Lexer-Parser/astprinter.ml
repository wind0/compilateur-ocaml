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
let print_init = fun name chan father max ->
let position = max +1
in print_label chan name position; print_edge chan position father; position

(*surcharge parce que j'en ai marre*)
let print_init1 = fun a b c -> print_init a b c c

(*print une liste d'elements grace à la fonction func*)
let rec print_list = fun elem_list chan father max func ->
match elem_list with
|[] -> max
|[h::t] -> let position = func h chan father max in print_list t chan father position func

let print_lister = fun name elem_list chan father max func ->
let position = print_init name chan father max
in print_list elem_list chan father max func

(*----------------Let us start-----------------*)

(*print un id*)
let print_id = print_init (*mais chuuuut*)

(*LES STATEMENT*)
let print_statement = fun stat chan father max ->
let position = print_init "STATEMENT" chan father max
in match stat with
|Affect (var, exp) -> 	let position1 = print_init1 ":=" chan position 
			in let position2 = print_var_or_id var chan position1 position1 
			in print_expr exp chan position1 position2
|Wut (id, exp_id_list) -> 	let position1 print_init1 "Procedure init" chan position 
				in let position2 = print_id id chan position1 position1
				in print_expr_id_list exp_id_list chan position1 position2
|Embedded(statement_list) ->	print_statement_list statement_list chan position1 position1
|IfThen(exp, stat) -> 	let position1 = print_init1 "If Then" chan position 
			in let position2 = print_expr exp chan position1 position1
			in print_statement stat chan position1 position2
|IfThenElse(exp, stat, stat2) ->	let position1 = print_init1 "If Then Else" chan position
					in let position2 = print_expr exp chan position1 position1
					in let position3 = print_statement stat chan position1 position2
					in print_statement stat2 chan position1 position3
|Case(exp, case_list) -> 	let position1 = print_init1 "Case" chan position 
				in let position2 = print_expr exp chan position1 position1
				in print_case_list case_list chan position1 position2
|While(exp, stat)  -> 	let position1 = print_init1 "While" chan position 
			in let position2 = print_expr exp chan position1 position1
			in print_statement stat chan position1 position2
|Repeat(stat_list, exp) -> 	let position1 = print_init1 "Repeat Until" chan position 
				in let position2 = print_statement_list stat_list chan position1 position1
				in print_expr exp chan position1 position2
|For (id, exp, inc_or_decr , exp2, stat) -> 	let position1 = print_init1 "For" chan position
						in let position2 = print_id id chan position1 position1
						in let position3 = print_expr exp chan position1 position2
						in let position4 = print_inc_or_decr inc_or_decr chan position1 position3
						in let position5 = print_expr exp2 chan position1 position4
						in print_statement stat chan position1 position5
and print_statement_list = print_lister "Statement list"

and print_var_or_id = fun a chan father max ->
match a with
|Variable var -> print_var var chan father max
|Id2 id -> print_id id chan father max
(*print procedures*)
and print_procedure = fun proc chan father max ->
let position = print_init "PROCEDURE" chan father max
in let position1 = print_id proc.proc_name chan position position
in let position2 = print_parameter proc.proc_parameter chan position position1
in print_block proc.proc_body chan position

and print_procedure_list = print_lister "FUNCTION_LIST"

(*FUCKING FUNCTIONS*)
and print_function = fun func chan father max ->
let position = print_init "FUNCTION" chan father max
in let position1 = print_id func.func_name chan position position
in let position2 = print_parameter func.func_parameter chan position position1
in let position3 = print_typ func.func_return_type chan position position2
in print_block func.func_body chan position 

and print_function_list = print_lister "FUNCTION_LIST"

(*print block*)
and print_block = fun block chan father max ->
let me = print_init "BLOCK" chan father max
in position2 = print_block_const block.constants chan me me
in let position3 = print_block_types block.types chan me position2
in let position4 = print_block_var block.variables chan me position3
in let position5 = print_procedure_list block.procedures chan me position4
in let position6 = print_function_list block.functions chan me position5
in print_statement_list block.statements chan me position6

 
(*principal*)
let print = fun ast file ->
let chan = open_out file
in let _ = fprintf chan "graph G {\n"
and me = 1
in let _ = print_label chan "PROGRAM" position
and position2 = print_id ast.prog_name chan me me
in print_block ast.prog_body chan me position2; fprintf chan "}"

