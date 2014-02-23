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
newline {update_loc lexbuf; token lexbuf}
|ws+ {token lexbuf}
|integer as i { try INTC (Int32.of_string i) with Failure _ -> Error.error lexbuf "integer cast failed"}
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
|eof {Error.error lexbuf "Unexpected EOF"}
|_ {Error.error lexbuf "Unexpected character"}

(*
and stringc = parse
|'\'' { () }
|(letter | digit | ws | newline)+ as content {try printf "%s" content with _ -> failwith "I dun goofed"}*)
