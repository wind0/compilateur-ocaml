{
  open Lexing
  open Printf
  open Parser
  open Error

    let update_loc lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
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
| "program" { PROGRAM }
| "begin" { BEGIN }
| "end" { END }
| ";" { SEMICOLON }
| "." { DOT }
|"record" { RECORD }
|"array" { ARRAY }
|"integer" { INTEGER }
|"boolean" { BOOLEAN }
|"nil" { NIL }
|"of" { OF }
|"'" { SIMPLECOTE }
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
|identifier as id { ID id }
|identifier as id { PROCID id }
|identifier as id { FUNCID id }
|identifier as id { VARID id }
|identifier as id { TYPEID id }
|identifier as id { PARAMLISTID id }
|identifier as id { CONSTID id }
|identifier as id { CONSTBLOCKID id }
|identifier as id { SIMPLETYPEID id }

(* Token pour ajouter des VAR aux programmes *)

|eof {failwith "I tried"}
|_ {failwith "Oh my"}