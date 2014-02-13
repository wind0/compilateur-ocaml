{
  open Lexing
  open Printf
}

let digit = ['0'-'9']
let identifier = ['A'-'Z' 'a'-'z']+ digit*
let integer = digit+
let newline = ('\010'|'\013'|"\013\010")
let ws = [' ' '\t']

rule token = parse
|newline {token lexbuf}
|ws+ {token lexbuf}
|integer as i {try printf "%s" i with _ -> failwith "fuck"}
|identifier as id {try printf "%s" (Lexing.lexeme lexbuf) with _ -> failwith "I tried..."}
|"true" {printf "true\n"}
|"false" {printf "false\n"}
|"var" {printf "var\n"}
|"program" {printf "program\n"}
|"begin" {printf "begin\n"}
|"end" {printf "end\n"}
|"+" {printf "+"}
|"-" {printf "-"}
|"*" {printf "*"}
|"/" {printf "/"}
|"and" {printf "and"}
|"or" {printf "or"}
|"<" {printf "<"}
|">" {printf ">"}
|"<=" {printf "<="}
|">=" {printf ">="}
|"=" {printf "="}
|":=" {printf ":="}
|"<>" {printf "<>"}
|"if" {printf "if"}
|"else" {printf "else\n"}
|"then" {printf "then\n"}
|"(" {printf "("}
|")" {printf ")"}
|"writeln" {printf "writeln"}
|"integer" {printf "integer"}
|"boolean" {printf "boolean"}
|":" {printf ":"}
|";" {printf ";\n"}
|"." {printf ". THE END"}
|"," {printf ","}
