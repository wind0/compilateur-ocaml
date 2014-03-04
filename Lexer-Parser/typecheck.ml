open AST.mli

type symbol = string *  typ

type const = 
|Cinteger of symbol * int32
|Cbool of symbol * bool

let table_symbol = symbol list

let rec create_table_rec = fun bl_var res ->
match bl_var with
|(id, Simple (Type_identifier ty))::t -> create_table_rec t ((id, ty)::res)
|[] -> res
|_::t ->  create_table_rec bl_var res
|_ -> failwith ("Impossible error?")  

let create_table = fun bl_var ->
create_table_rec bl_var []


