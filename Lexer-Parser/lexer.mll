{
  open Lexing
  open Printf
  open Parser
}

let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']
let identifier = letter (letter | digit)*
let integer = digit+
let newline = ('\010'|'\013'|"\013\010")
let ws = [' ' '\t']

rule token = parse

(* Premier TOKEN pour un programme vide PASCAL *)
newline {token lexbuf}
|ws+ {token lexbuf}
| "program" { PROGRAM }
| "begin" { BEGIN }
| "end" { END }
| ";" { SEMICOLON }
| "." { DOT }
|identifier as id { ID id }

(* Token pour ajouter des VAR aux programmes *)

|eof {failwith "I tried"}
|_ {failwith "Oh my"}