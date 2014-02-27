let chan = open_in "Test_file/test_0.2.5.pas"
let lexbuf = Lexing.from_channel chan

let truc = ParserHELIX.program Lexer.token lexbuf

let _ = Printf.printf "%s" truc.prog_name
