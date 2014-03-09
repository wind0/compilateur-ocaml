open Astprinter
open Typecheck

let chan = open_in "Test_file/test_type_03.pas"
let lexbuf = Lexing.from_channel chan

let arbre = ParserFinal.program Lexer.token lexbuf


let _  = Astprinter.print arbre "output.dot"

let _ = Printf.printf "%B\n" (Typecheck.typc_prog arbre)
