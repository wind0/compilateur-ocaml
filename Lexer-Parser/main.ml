let chan = open_in "test_0.2.0.pas"
let lexbuf = Lexing.from_channel chan

let _ = Parser.program Lexer.token lexbuf
