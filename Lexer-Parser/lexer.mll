(* LEXEX *)
{
  open Lexing
  open Printf
  open ParserHELIX
  open Error
}

let digit = ['0'-'9']
let lowerletter = ['a'-'z']
let letter = ['A'-'Z' 'a'-'z']
let varid = lowerletter (letter | digit)*
let identifier = letter (letter | digit)*
let integer = digit+
let newline = ('\010'|'\013'|"\013\010")
let ws = [' ' '\t']
let string = [^'''] (letter | digit | newline | ws)*

rule token = parse

(* L'ordre des token est important !!!!! *)

newline {token lexbuf}
|ws+ {token lexbuf}
| "program" { PROGRAM }
| "begin" { BEGIN }
| "end" { END }
| ";" { SEMICOLON }
| ".(" { DOTLPAR }
| "." { DOT }
|"record" { RECORD }
|"array" { ARRAY }
|"integer" { INTEGER }
|"boolean" { BOOLEAN }
|"nil" { NIL }
|"+" { PLUS }
|"-" { MINUS }
|"*" { MULT }
|"/" { DIV }
|"mod" { MOD }
|"^" { PUIS }
|"," { COMA }
|":" { COLON }
|"(" { LPAR }
|")" { RPAR }
|".." {DOUBLEDOT}
|"[" { LBR }
|"]" { RBR }
|"=" { EQ }
|"!=" { NOTEQ }
|"<" { LT }
|">" { GT }
|"<=" { LE }
|">=" { GE }
|"in" { IN }
|"!" { NOT }
|":=" { COLONEQ }
|"var" { VAR }
|"const" { CONST }
|"type" { TYPE }
|"procedure" { PROCEDURE }
|"function" { FUNCTION }
|"if" { IF }
|"then" { THEN }
|"else" { ELSE }
|"case" { CASE }
|"of" { OF }
|"while" { WHILE }
|"do" { DO }
|"repeat" { REPEAT }
|"until" { UNTIL }
|"for" { FOR }
|"to" { TO }
|"downto" { DOWNTO }
|integer as i { try INTC (Int32.of_string i) with Failure _ -> Error.error lexbuf "integer cast failed"}
|"'"([^''']* as s)"'" {STRINGC s}
|varid as v { VARID v }
|identifier as id { ID id }

|eof {failwith "I tried"}
|_ {failwith "Oh my"}
