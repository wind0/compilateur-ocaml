open AST.mli

type symbol = string *  typ

type const = 
|Cinteger of symbol * int32
|Cbool of symbol * bool

let table_symbol = symbol list

(*return a symbol option*)
(*manque pas mal de trucs, tableaux, strings et records*)
let get_init_var = fun ivar ->
match ivar with
|(id, Simple (Type_identifier ty)) -> Some (id, ty)
|_ -> None



(*identifier * const*)
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


let rec create_table_rec = fun bl_var res ->
match bl_var with
|h::t -> begin 
	match get_init_var with 
	|Some truc	-> create_table_rec t (truc::res)
	|None -> create_table_rec t res
	end 
|[] -> res
|_ -> failwith ("Impossible error?")  

let create_table = fun bl_var ->
create_table_rec bl_var []


