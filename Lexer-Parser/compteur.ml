module Compteur :
sig
	val compteurinit : string -> int -> unit
	val compteur : string -> int -> int
end
=
struct
let compteurinit = fun name a ->
	let fout = open_out name
	in let ret = output_binary_int fout a
	in close_out fout; ret;;

let compteur = fun name a ->
	let fin = open_in name
	in let get = input_binary_int fin
	in let fout = close_in fin; open_out name
	in let _ = output_binary_int fout (get + a)
	in close_out fout; get;;	
end;;
