let chan = open_in "Test_file/test_0.2.5.pas"
let lexbuf = Lexing.from_channel chan

let _ = ParserHELIX.program Lexer.token lexbuf
