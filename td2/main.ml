let chan = open_in "td2.pas"
let lexbuf = Lexing.from_channel chan

let _ = Parser.program Lexer.token lexbuf