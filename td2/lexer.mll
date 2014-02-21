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
|newline {token lexbuf}
|ws+ {token lexbuf}
|integer as i { INTC (Int32.of_string i) }
|identifier as id { ID id }
|"true" { BOOLC true}
|"false" { BOOLC false }
|"var" { VAR }
|"const" { CONST }
|"program" { PROGRAM }
|"begin" { BEGIN }
|"end" { END }
|"+" { PLUS }
|"-" { MINUS }
|"*" { TIMES }
|"div" { DIV }
|"mod" { MOD }
|"not" { NOT }
|"and" { AND }
|"or" { OR }
|"<" { LT }
|">" { GT }
|"<=" { LE }
|">=" { GE }
|"=" { EQ }
|":=" { COLONEQ }
|"::=" { CCOLONEQ }
|"<>" { NEQ }
|"if" { IF }
|"else" { ELSE }
|"then" { THEN }
|"(" { LPAR }
|")" { RPAR }
|"integer" { INTEGER }
|"boolean" { BOOLEAN }
|"array" { ARRAY }
|":" { COLON }
|";" { SEMICOLON }
|"." { DOT }
|"," { COMMA }
|"[" { LBR }
|"]" { RBR }
|"nil" { NIL }
|"case" { CASE }
|"of" { OF }
|"record" { RECORD }
|"repeat" { REPEAT }
|"until" { UNTIL }
|"for" { FOR }
|"do" { DO }
|"to" { TO }
|"downto" { DOWNTO }
|"while" { WHILE }
|"function" { FUNCTION }
|"procedure" { PROCEDURE } 
|"'"([^''']* as s)"'" {STRINGC s}
|eof {failwith "I tried"}
|_ {failwith "Oh my"}

(*
and stringc = parse
|'\'' { () }
|(letter | digit | ws | newline)+ as content {try printf "%s" content with _ -> failwith "I dun goofed"}*)