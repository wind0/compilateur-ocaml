let chan = open_in "Test_file/test_0.2.5.pas"
let lexbuf = Lexing.from_channel chan

let _ = Parser.program Lexer.token lexbuf