open Lexing
open Printf

module Error :
sig
	val error : Lexing.lexbuf -> string -> 'a
end
=
struct
let error lexbuf msg =
let p1 = Lexing.lexeme_start_p lexbuf
and p2 = Lexing.lexeme_end_p lexbuf
in
let line_number_start = p1.pos_lnum
and offset_start = p1.pos_cnum - p1.pos_bol
and line_number_end = p2.pos_lnum
and offset_end = p2.pos_cnum - p2.pos_bol
in let err_msg = Printf.sprintf "error between line %i char %i and line %i char %i. message= %s" line_number_start offset_start line_number_end offset_end msg
in failwith err_msg
end;;


