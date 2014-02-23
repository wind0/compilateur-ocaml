open Error

let chan = open_in "td2.pas"
let lexbuf = Lexing.from_channel chan

let _ = 
	try
	Parser.program Lexer.token lexbuf
	with Parser.Error ->
	Error.error lexbuf "I tried"
