open AST.mli

type symbol = string *  typ

type const = 
|Cinteger of symbol * int32
|Cbool of symbol * bool

type table_symbol = symbol list

type function_table = table_symbol

type variable_table = table_symbol

type procedure_table = identifier list

type parameter_tuple = table_symbol * function_table * variable_table * procedure_table

type procedure_typ = identifier

type function_table = identifier * typ * parameter_tuple

(*Les variables de base*)
(*return a symbol option*)
(*manque pas mal de trucs, tableaux, strings et records*)
let get_init_var = fun ivar ->
match ivar with
|(id, Simple (Type_identifier ty)) -> Some (id, ty)
|_ -> None



let rec create_table_rec = fun bl_var res ->
match bl_var with
|h::t -> begin 
	match get_init_var h with 
	|Some truc	-> create_table_rec t (truc::res)
	|None -> create_table_rec t res
	end 
|[] -> res

let create_table = fun bl_var ->
create_table_rec bl_var []

(*le constantes*)
(*identifier * const*)
(*returns const option*)
let get_const = fun iconst ->
match iconst with
|(id, c) -> 	let value = begin 
		match c with 
		| SignedIdOrNumber (s, BInteger bi) -> 	if s == Minus then - bi else bi
		| IdOrNumber (BInteger bi) -> bi	
		end
		in	
		Some(Cinteger((id,TypInteger),value))
|_ -> None

let rec create_table_const_rec = fun bl_const res ->
match bl_const with
|h::t -> begin
	match get_const hwith
	|Some x -> create_table_const_rec t x::res
	|None -> create_table_const_rec t res

let create_table_const = fun bl_const ->
create_table_const_rec bl_const []

(*les paramètres*)

(*associe une liste d'identifiant et un type à une liste de (identifiant,type) *)
let rec make_my_list_rec = fun id_list typ res->
match id_list with
|h::t -> make_my_list_rec t typ ((h,typ)::res)

let make_my_list = fun id_list typ ->
make_my_list_rec id_list typ []

(*prend une parameter_list (hum hum) et recrée un tuple qui contient tous les arguments, séparés par catégorie*)
let rec create_parameter_list_rec = fun params (c,f,v,p)-> 
match params with
|NoneParameter -> res
|ClassicParameter (id_list, typ, param2) -> let a = make_my_list id_list typ 
						in create_parameter_list_rec param2 (c@a,f,v,p)
|FunctionParameter (id_list, typ, param2) -> let a = make_my_list id_list typ
						in create_parameter_list_rec param2 (c,f@a,v,p)
|VariableParameter (id_list, typ, param2) -> let a = make_my_list id_list typ
						in create_parameter_list_rec param2 (c,f,v@a,p)
|ProcedureParameter (id_list, param2) -> create_parameter_list_rec param2 (c,f,v,p@id_list)

let create_parameter_list = fun params ->
create_parameter_list_rec params []

(*procedures*)

let get_proc_type = fun proc ->
(proc.proc_name,create_parameter_list proc.proc_parameters)

(*what else?*)

(*fonctions*)

let get_function_type = fun func ->
(func.func_name,func.func_return_type,func.func_parameters)
