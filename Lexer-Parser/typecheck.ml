open AST
module Typecheck = 
struct
type symbol = string *  typ

type const = 
|Cinteger of symbol * int32
|Cbool of symbol * bool

type table_symbol = symbol list

type constant_table = const list

type function_table_p = table_symbol

type variable_table = table_symbol

type procedure_table_p = identifier list

type parameter_tuple = table_symbol * function_table_p * variable_table * procedure_table_p

type procedure_table = identifier * parameter_tuple
type function_table = identifier * typ * parameter_tuple

(*associe une liste d'identifiant et un type à une liste de (identifiant,type) *)
let rec make_my_list_rec = fun id_list typ res->
match id_list with
|h::t -> make_my_list_rec t typ ((h,typ)::res)
|[] -> res

let make_my_list = fun id_list typ ->
make_my_list_rec id_list typ []


(*Les variables de base*)
(*return a symbol option*)
(*manque pas mal de trucs, tableaux, strings et records*)
let get_init_var = fun ivar ->
match ivar with
|(id_l, Simple (Type_identifier ty)) -> Some (make_my_list id_l ty)
|_ -> None



let rec (create_table_rec: AST.block_var -> table_symbol -> table_symbol) = fun bl_var res ->
match bl_var with
|h::t -> begin 
	match get_init_var h with 
	|Some truc	-> create_table_rec t (truc@res)
	|None -> create_table_rec t res
	end 
|[] -> res

let create_table = fun bl_var ->
create_table_rec bl_var []

(*le constantes*)
(*identifier * const*)
(*returns const option*)
let (get_const: identifier*constant -> const option) = fun iconst ->
match iconst with
|(id, CString _) -> None
|(id, c) -> let value = begin 
		match c with 
		| SignedIdOrNumber (s, BInteger bi) -> 	if s == Minus then Int32.neg bi else bi
		| IdOrNumber (BInteger bi) -> bi	
		| _ -> failwith("Impossible error2!")
		end
		in	
		Some(Cinteger((id,TypInteger),value))

let rec create_table_const_rec = fun bl_const res ->
match bl_const with
|h::t -> let a = get_const h in
	begin	
	match a with
	|Some x -> create_table_const_rec t (x::res)
	|None -> create_table_const_rec t res
	end
|[] -> res

let create_table_const = fun bl_const ->
create_table_const_rec bl_const []

(*les paramètres*)
(*prend une parameter_list (hum hum) et recrée un tuple qui contient tous les arguments, séparés par catégorie*)
let rec create_parameter_list_rec = fun params (c,f,v,p)-> 
match params with
|NoneParameter -> (c,f,v,p)
|ClassicParameter (id_list, typ, param2) -> let a = make_my_list id_list typ 
						in create_parameter_list_rec param2 (c@a,f,v,p)
|FunctionParameter (id_list, typ, param2) -> let a = make_my_list id_list typ
						in create_parameter_list_rec param2 (c,f@a,v,p)
|VariableParameter (id_list, typ, param2) -> let a = make_my_list id_list typ
						in create_parameter_list_rec param2 (c,f,v@a,p)
|ProcedureParameter (id_list, param2) -> create_parameter_list_rec param2 (c,f,v,p@id_list)

let create_parameter_list = fun params ->
create_parameter_list_rec params ([],[],[],[])

(*procedures*)

let get_proc_type = fun proc ->
(proc.proc_name,create_parameter_list proc.proc_parameters)


(*fonctions*)

let get_function_type = fun func ->
(func.func_name,func.func_return_type,create_parameter_list func.func_parameters)


(**************Let us start!************)

type block_table = 
{
my_consts : constant_table;
my_vars : table_symbol;
my_funcs : function_table list;
my_procs : procedure_table list;
}


let make_block_table = fun my_block ->
let mconsts = create_table_const my_block.constants
and mvars = create_table my_block.variables
and mfuncs = List.map get_function_type my_block.functions
and mprocs = List.map get_proc_type my_block.procedures
in
{
my_consts = mconsts;
my_vars = mvars;
my_funcs = mfuncs;
my_procs = mprocs;
}


let rec typc_exp = fun expected bt exp ->
match expected, exp with
|TypInteger , EOperation  _ -> failwith("trying to fit a boolean operation in an integer")
|_ , EOperation (e1, logop, e2) -> begin 	match typc_sexp TypInteger bt e1, typc_sexp TypInteger bt e2 with
					|TypInteger, TypInteger -> TypBoolean
					|_ -> failwith("cannot numerically compare other types than integers")
				end
|a , ESimple s -> begin match a, typc_sexp a bt s with
		| t1 , t2 when t1 == t2 -> a
		|_ -> failwith ("wrong type in a simple expression")
		end
and typc_sexp = fun expected bt exp ->
match expected , exp with
TypBoolean, Signed _ | TypBoolean, USigned (_, _::_) -> failwith ("do you want to sign a boolean?")
|TypBoolean, USigned (terme, _) -> typc_term expected bt terme 
|_ , USigned (terme, terme_sign_list) | _, Signed(_, terme, terme_sign_list)
	-> match typc_term expected bt terme, typc_term_list expected bt terme_sign_list with
						|TypInteger, TypInteger -> TypInteger
						| _ -> failwith("Doing sums on a WRONG type")

and typc_term = fun expected bt terme ->
match expected, terme with
|TypBoolean, ( _ , _::_) -> failwith("multiplication on a boolean")
| _ , ( f, []) -> typc_factor expected bt f
|_, (f, l)-> if List.for_all (function a -> let test = typc_factor TypInteger bt a in test == TypInteger) (List.map (function (a,b) -> b) l)
		then TypInteger
		else failwith("???????????")

and typc_term_list = fun expected bt terme_list ->
if List.for_all (function t -> let test = typc_term expected bt t in test = expected) (List.map (function (a,b) -> b) terme_list)
then expected
else failwith ("??")

and typc_factor = fun expected bt terme ->
match expected, terme with
|_, UConst _ -> TypInteger
|TypBoolean , Neg_factor n -> typc_factor expected bt n
|TypInteger, Neg_factor _ -> failwith("negation of an integer")
|_ , Brackets _ -> failwith("No brackets for now, sorry")
|_, Var (v,_) -> List.assoc v bt.my_vars
|_, Function (i,_) -> let cut_tuple = fun (a,b,c)-> a,b in
			let ft = List.map cut_tuple bt.my_funcs
			in List.assoc i ft
|_, Expression a -> typc_exp expected bt a 

let rec typc_statement = fun bt stat ->
match stat with
|Affect((Id2 i),e) |Affect((Variable (i, _)) ,e) -> let expected = List.assoc i bt.my_vars in (typc_exp expected bt e) == expected
|Embedded s_list -> List.for_all (fun a -> typc_statement bt a) s_list
|IfThen (e, s)  | While (e, s) -> ((typc_exp TypBoolean bt e) == TypBoolean) && (typc_statement bt s)
|IfThenElse (e,s1,s2) -> ((typc_exp TypBoolean bt e)== TypBoolean) && (typc_statement bt s1) && (typc_statement bt s2)
|Repeat (stat_list , e) -> (List.for_all (fun a ->typc_statement bt a) stat_list) && ((typc_exp TypBoolean bt e)==TypBoolean)
|_ -> true

let verif_bloc = fun bloc ->
let bt = make_block_table bloc
in List.for_all (fun a -> typc_statement bt a) bloc.statements 

let typc_prog = fun prog ->
verif_bloc prog.prog_body
end
