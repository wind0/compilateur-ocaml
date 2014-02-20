%{
    open Printf
}%

%token <int32> INTC
%token <string> STRINGC
%token <bool> BOOLC
%token <string> ID
%token INTEGER BOOLEAN
%token VAR PROGRAM BEGIN END
%token PLUS MINUS TIMES DIV MOD
%token NOT AND OR NIL
%token LT GT LE GE EQ NEQ
%token IF THEN ELSE CASE OF
%token COLON SEMICOLON COMMA DOT
%token COLONEQ CCOLONEQ
%token LBR RBR LPAR RPAR
%token WHILE FOR TO DOWNTO DO REPEAT UNTIL
%token RECORD
%token FUNCTION PROCEDURE

%start<unit> PROGRAM


%left OR
%left AND
%nonassoc LT GT LE GE EQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc LBR
%nonassoc THEN
%nonassoc ELSE


%%
typ:
|INTEGER {sprintf "integer"}
|BOOLEAN {sprintf "boolean"}
|ARRAY OF t = typ {sprintf "array of %s" t}

id_list:
| i = ID { i }
| i = ID COMMA i2 = id_list { sprintf "%s , %s" i i2 }

declaration:
	l = id_list 
	COLON 
	t = typ 
	SEMICOLON 
	{ sprintf "%s : %s ;\n" l t}

var:
	VAR
	v = (declaration)+
	{ sprintf "%s\n" v}

parametres:
	| LPAR RPAR { sprintf "()" }
	| LPAR i = id_list COLON t = typ RPAR {sprintf "( %s : %s )" i t}
	| LPAR i = id_list COLON t = typ (SEMICOLON i2 = id_list COLON t2 = typ)+ RPAR  {sprintf "( %s : %s ; %s : %s )" i t i2 t2} 

functions_and_procs:
	f = (function | procedure)+
	{sprintf "%s" f} 

procedure:
	PROCEDURE i = ID
	para = parametres SEMICOLON
	con = constants SEMICOLON
	var = variables SEMICOLON
	func = functions_and_procs
	bloc = block
	{ sprintf "id %s para %s;\n con %s;\n var %s;\n func %s\n bloc %s\n" i para con var func bloc}

function:
	FUNCTION i = ID 
	para = parametres COLON
	t = typ SEMICOLON
	con = constants SEMICOLON
	var = variables SEMICOLON
	func = functions_and_procs
	BEGIN
		bloc = block
		ret = ID CCOLONEQ var
	END
	{ if i == ret then sprintf "id %s para %s: t %s;\n con %s;\n var %s;\n func %s\n bloc %s\n" i para t con var func bloc else failwith "Woop woop"}	

block:
	blo = (init | blockimbr | conditionnal | boucle | appelfunction)*
	{ sprintf "begin %s end" blo}	

blockimbr:
	BEGIN
	blo = block
	END
	SEMICOLON
	{ sprintf "%s ;" blo}


(*Le grand final*)
program:
	PROGRAM i = ID
	con = constants
	var = variables
	func = functions_and_procs
	bloc = block
	DOT {
		printf "My name is %s\n My vars are %s\n My functions and procs are %s\n I do %s\n" i con var func bloc
	}
