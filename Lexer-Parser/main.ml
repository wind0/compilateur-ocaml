open Astprinter
open Typecheck

let chan = open_in "Test_file/test_0.2.5.pas"
let lexbuf = Lexing.from_channel chan

let magical_pamyu = ParserHELIX.program Lexer.token lexbuf


let i_love_jelly_babies  = Astprinter.print magical_pamyu "foulemoila.dot"

let _ = Printf.printf "%B\n" (Typecheck.typc_prog magical_pamyu)
