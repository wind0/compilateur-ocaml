
open Printf
open AST
(*TODO:
créer les groupes.
print_parameter est complètement indépendante! foutez lui la paix!
*)
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
|h::t -> let position = func h chan father max in print_list t chan father position func

let print_lister = fun name func elem_list chan father max ->
let position = print_init name chan father max
in print_list elem_list chan father max func

(*----------------Let us start-----------------*)
(*print un id*)
let print_id = print_init (*mais chuuuut*)

let print_id_list = print_lister "ID LIST" print_id

let print_typ = fun typ chan father max ->
match typ with
|TypInteger -> print_init "TypInteger" chan father max
|TypBoolean -> print_init "TypBoolean" chan father max

let print_uconst = fun uc chan father max ->
match uc with
|UInteger i -> print_init (sprintf "%li" i) chan father max
|UString s -> print_init s chan father max

let print_uconst_nil = fun u chan father max ->
match u with 
|UNormal uc -> print_uconst uc chan father max
|Nil -> print_init "NIL" chan father max


(* indep *)

let print_log_operator = fun op chan father max ->
let my_print = fun a -> print_init a chan father max
in
match op with
|Lt -> my_print "<"
|Le -> my_print "<="
|Gt -> my_print ">"
|Ge -> my_print ">="
|Eq -> my_print "=="
|Neq -> my_print "!="
|In -> my_print "In"
;;

let print_sign = fun s chan father max ->
let my_print = fun a  -> print_init a chan father max
in
match s with
|Plus -> my_print "+"
|Minus -> my_print "-"
;;

let print_sign_term_list = print_lister "SIGN_TERM_LIST" print_sign_term ;;


(* groupe variable expression simple_expre term factor *)
let print_simple_expr = fun expr chan father max ->
match expr with
|Signed (signe, t, sign_term_list) -> 	let position = print_init "SIGNED" chan father max
					in let position1 = print_sign signe chan position position
					in let position2 = print_term t chan position position1
					in print_sign_term_list sign_term_list chan position position2
|USigned (t, sign_term_list) -> 	let position = print_init "UNSIGNED" chan father max
					in let position1 = print_term t chan position position
					in print_sign_term_list sign_term_list chan position position1

and print_expr = fun exp chan father max ->
match exp with
|ESimple s -> print_simple_expr s chan father max
|EOperation (e1 , o, e2) -> 	let position = print_log_operator o chan father max 
				in let position1 = print_simple_expr e1 chan position position 
				in print_simple_expr e2 chan position position1
and print_expr_list = print_lister "EXPR_LIST" print_expr

and print_var = fun (id,biv_list) chan father max->
let position = print_init "VARIABLE" chan father max
in let position1 = print_id id chan father max
in print_biv_list biv_list chan father max

and print_biv_list = print_lister "BOUCLE_INTERNE_VARIABLE_LIST" print_biv

and print_biv = fun biv chan father max -> 
match biv with
|Brackety exp_list -> let position = print_init "BRACKETS" chan father max in print_expr_list exp_list chan position position
|Dotty id -> let position = print_init "DOT" chan father max in print_id id chan position position

and print_factor = let rec truc = fun fact chan father max ->
let position = print_init "FACTOR" chan father max
in
match fact with
|UConst u -> print_uconst_nil u chan position position
|Var v -> print_var v chan position position
|Function (i, exp_list) -> let position1 = print_init1 "FUNCTION" chan position
			in let position2 = print_id i chan position1 position1
			in print_expr_list exp_list chan position1 position2
|Expression exp -> print_expr exp chan position position
|Neg_factor f -> let position1 = print_init1 "NOT" chan position
		in truc f chan position1 position1
|Brackets exp_list -> let position1 = print_init1 "Brackets" chan position
			in print_expr_list exp_list chan position1 position1
in truc

and print_term = fun (fac, op_fact_list) chan father max ->
let position = print_init "TERM" chan father max
in let position2 = print_factor fac chan position position
in print_op_term_fact_list op_fact_list chan father max
and print_op_term_fact_list = print_lister "OP_TERM_FACT_LIST" print_op_term_fact

and print_op_term = fun op chan father max ->
let my_print = fun a -> print_init a chan father max
in
match op with
|Times -> my_print "*"
|Div -> my_print "/"
|Mod -> my_print "%"
|Pow -> my_print "^"

and print_op_term_fact = fun (op, f) chan father max ->
let position = print_init "OP_TERM_FACT" chan father max
in let position1 = print_op_term op chan position position
in print_factor f chan position position1
;;

(*FUCK*)
let rec print_parameter = fun typ chan father max ->
let position = print_init "PARAMETER" chan father max
in
match typ with
|NoneParameter -> print_init "NoneParameter" chan father position
|ClassicParameter(identifier_list, typ, parameter) ->
let position1 = print_init "ClassicParameter" chan father position
in let position2 = print_typ typ chan position1 position1
in let position3 = print_id_list identifier_list chan position1 position2
in print_parameter parameter chan position1 position3

|FunctionParameter(identifier_list, typ, parameter) ->
let position1 = print_init "FunctionParameter" chan father position
in let position2 = print_typ typ chan position1 position1
in let position3 = print_id_list identifier_list chan position1 position2
in print_parameter parameter chan position1 position3

|VariableParameter(identifier_list, typ, parameter) ->
let position1 = print_init "VariableParameter" chan father position
in let position2 = print_typ typ chan position1 position1
in let position3 = print_id_list identifier_list chan position1 position2
in print_parameter parameter chan position1 position3

|ProcedureParameter(identifier_list, parameter) ->
let position1 = print_init "ProcedureParameter" chan father position
in let position2 = print_id_list identifier_list chan position1 position1
in print_parameter parameter chan position1 position2 ;;


(*dependants des precedents*)

let print_sign_term = fun (sign, term) chan father max ->
let position = print_init "SIGN_TERM" chan father max
in let position1 = print_sign sign chan position position
in print_term term chan position position1
;;

let print_burne = fun b chan father max->
match b with
|BIdentified id -> print_id id chan father max
|BInteger i -> print_id (sprintf "%li" i) chan father max

and print_constant = fun c chan father max->
match c with
|SignedBurne (s,b) -> let position = print_init "SIGNED_CONSTANT" chan father max 
			in let position1 = print_sign s chan position position
			in print_burne b chan position position1
|Burne b -> print_burne b chan father max
|CString s -> let position = print_init "STRING" chan father max
		in print_init1 s chan position
;;

let print_simple_type = fun st chan father max ->
match st with
|Type_identifier t-> print_typ t chan father max
|ID_list id_list -> print_id_list id_list chan father max
|Enum (c1,c2) -> let position = print_init "Enum" chan father max
		in let position1 = print_constant c1 chan position position
		in print_constant c2 chan position position1
;;

let print_simple_type_list = print_lister "SIMPLE_TYPE_LIST" print_simple_type ;;


(*field_list et type_automate*)

let print_field_list = let rec truc  = fun fl chan father max ->
match fl with
|Recur (id_list,typ_auto) -> let position =  print_id_list id_list chan father max in print_typ_auto typ_auto chan father position
|RecurPlus (id_list, typ_auto, fl) -> let position = print_id_list id_list chan father max
					in let position1 = print_typ_auto typ_auto chan father position
					in truc fl chan father position1
|FCase (id, t, line_case_field_list) -> let position = print_id id chan father max
					in let position1 = print_typ t chan father position
					in print_line_case_field_list line_case_field_list chan father position
in truc

and print_line_case_field_list = print_lister "LINE_CASE_FIELD_LIST" print_line_case

and print_line_case = fun (c_list, fl) chan father max->
let position = print_init "LINE_CASE" chan father max
in let position2 = print_constant_list c_list chan position position
in print_field_list fl chan father max

and print_constant_list = print_lister "CONST_LIST" print_constant

and print_typ_auto = fun t chan father max ->
match t with
|Simple ty -> print_simple_type ty chan father max
|Array a -> print_array_type_auto a chan father max
|Record r -> print_field_list r chan father max

and print_array_type_auto = fun (st_list, typ_auto) chan father max->
let position = print_init "ARRAY_TYPE_AUTO" chan father max
in let position1 = print_simple_type_list st_list chan position position
in print_typ_auto typ_auto chan position position1
;;


let print_init_var = fun (id_list,typ_auto) chan father max ->
let position = print_init "INIT_VAR" chan father max
in let position1 = print_id_list id_list chan position position
in print_typ_auto typ_auto chan position position1
;;

let print_block_var = print_lister "INIT_VAR LIST" print_init_var;;

let print_expr_id_list = print_lister "EXP_OR_PROCID_LIST" print_expr_or_procid ;;

let print_expr_or_procid = fun eop chan father max ->
match eop with
|Id id -> print_id id chan father max
|Expr exp -> print_expr exp chan father max
;;

let print_case_list = print_lister "SINGLE_CASE_LIST" print_case ;;


let print_case = fun (c_list, stat) chan father max ->
let position = print_init "SINGLE_CASE" chan father max
in let position1 = print_constant_list c_list chan position position
in print_statement stat chan position position1

and print_statement = let rec truc = fun stat chan father max ->
let position = print_init "STATEMENT" chan father max
in match stat with
|Affect (var, exp) -> 	let position1 = print_init1 ":=" chan position 
			in let position2 = print_var_or_id var chan position1 position1 
			in print_expr exp chan position1 position2
|Wut (id, exp_id_list) -> 	let position1 = print_init1 "Procedure init" chan position 
				in let position2 = print_id id chan position1 position1
				in print_expr_id_list exp_id_list chan position1 position2
|Embedded(statement_list) ->	print_statement_list statement_list chan position position
|IfThen(exp, stat) -> 	let position1 = print_init1 "If Then" chan position 
			in let position2 = print_expr exp chan position1 position1
			in truc stat chan position1 position2
|IfThenElse(exp, stat, stat2) ->	let position1 = print_init1 "If Then Else" chan position
					in let position2 = print_expr exp chan position1 position1
					in let position3 = truc stat chan position1 position2
					in truc stat2 chan position1 position3
|Case(exp, case_list) -> 	let position1 = print_init1 "Case" chan position 
				in let position2 = print_expr exp chan position1 position1
				in print_case_list case_list chan position1 position2
|While(exp, stat)  -> 	let position1 = print_init1 "While" chan position 
			in let position2 = print_expr exp chan position1 position1
			in truc stat chan position1 position2
|Repeat(stat_list, exp) -> 	let position1 = print_init1 "Repeat Until" chan position 
				in let position2 = print_statement_list stat_list chan position1 position1
				in print_expr exp chan position1 position2
|For (id, exp, inc_or_decr , exp2, stat) -> 	let position1 = print_init1 "For" chan position
						in let position2 = print_id id chan position1 position1
						in let position3 = print_expr exp chan position1 position2
						in let position4 = print_inc_or_decr inc_or_decr chan position1 position3
						in let position5 = print_expr exp2 chan position1 position4
						in truc stat chan position1 position5
in truc

and print_statement_list = print_lister "Statement list" print_statement

and print_var_or_id = fun a chan father max ->
match a with
|Variable var -> print_var var chan father max
|Id2 id -> print_id id chan father max

and print_inc_or_decr = fun a chan father max ->
match a with
|To -> print_init "TO" chan father max
|Downto -> print_init "DOWNTO" chan father max

;;

(*Const*)


let print_block_type = print_lister "BLOCK_TYPE" print_init_type;;

let print_init_type = fun (id, typ_auto) chan father max ->
let position = print_init "INIT_TYPE" chan father max
in let position1 = print_id id chan position position
in print_typ_auto typ_auto chan position position1		
;;



(*print block*)
let print_block = fun block chan father max ->
let me = print_init "BLOCK" chan father max
in let position2 = print_block_const block.constants chan me me
in let position3 = print_block_type block.types chan me position2
in let position4 = print_block_var block.variables chan me position3
in let position5 = print_procedure_list block.procedures chan me position4
in let position6 = print_function_list block.functions chan me position5
in print_statement_list block.statements chan me position4

and print_block_const = print_lister "init_cont list" print_init_const 

and print_procedure = fun proc chan father max ->
let position = print_init "PROCEDURE" chan father max
in let position1 = print_id proc.proc_name chan position position
in let position2 = print_parameter proc.proc_parameters chan position position1
in print_block proc.proc_body chan position position2

;;

let print_procedure_list = print_lister "PROCEDURE_LIST" print_procedure ;;

let print_function_list = print_lister "FUNCTION_LIST" print_function ;;


let print_function = fun func chan father max ->
let position = print_init "FUNCTION" chan father max
in let position1 = print_id func.func_name chan position position
in let position2 = print_parameter func.func_parameters chan position position1
in let position3 = print_typ func.func_return_type chan position position2
in print_block func.func_body chan position position3
;;


let print_init_const = fun (identifier, constante) chan father max ->
let position = print_init "INIT CONST" chan father max
in let position1 = print_id identifier chan father position
in print_constant constante chan position position1
;;


(*principal*)
let print = fun ast file ->
let chan = open_out file
	in let _ = fprintf chan "graph G {\n"
	and me = 1 
		in let _ = print_label chan "PROGRAM" me
		and position2 = print_id ast.prog_name chan me me
			in let _  = print_block ast.prog_body chan me position2
				and _ =  fprintf chan "}"
				in close_out chan
;;