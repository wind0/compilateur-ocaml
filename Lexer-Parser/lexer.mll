{
  open Lexing
  open Printf
  open Parser
  open Error
}

let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']
let identifier = letter (letter | digit)*
let integer = digit+
let newline = ('\010'|'\013'|"\013\010")
let ws = [' ' '\t']

rule token = parse

(* Premier TOKEN pour un programme vide PASCAL *)
(* ordre extrement important !!!!! *)

newline {token lexbuf}
|ws+ {token lexbuf}
|integer as i { try INTC (Int32.of_string i) with Failure _ -> Error.error lexbuf "integer cast failed"}
| "program" { PROGRAM }
| "begin" { BEGIN }
| "end" { END }
| ";" { SEMICOLON }
| "." { DOT }
|"integer" { INTEGER }
|"boolean" { BOOLEAN }
|identifier as id { ID id }
|"nil" { NIL }
|"'" {SIMPLECOTE}
|"+" {UNARYPLUS}
|"-" {UNARYMINUS}
|"," {COMA}
|"(" { LPAR }
|")" { RPAR }
|".." {DOUBLEDOT}

(* Token pour ajouter des VAR aux programmes *)

|eof {failwith "I tried"}
|_ {failwith "Oh my"}