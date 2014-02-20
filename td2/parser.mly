%{
    open Printf
%}

%token <int32> INTC
%token <string> STRINGC
%token <bool> BOOLC
%token <string> ID
%token INTEGER BOOLEAN ARRAY
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

%start <unit> program


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
	v = nonempty_list(declaration)
	{ sprintf "%s\n" v}

binop:
	|PLUS {"+"}
	|MINUS {"-"}
	|TIMES {"*"}
	|DIV {"/"}
	|MOD {"%"}

arithmetic_operation:
	|i = ID {i}
	|i = INTC {sprintf "%d" i}
	|a = arithmetic_operation b = binop c = arithmetic_operation {sprintf "%s %s %s" a b c}
	|LPAR a = arithmetic_operation RPAR {sprintf "( %s )" a}

binlogop:
	|AND {"&&"}
	|OR {"||"}
	|LT {"<"}
	|GT {">"}
	|GE {">="}
	|LE {"<="}
	|EQ {"=="}
	|NEQ {"!="}

operation:
	|NOT (LPAR)? a = operation (RPAR)? {sprintf "! %s" a}
	|arithmetic_operation binlogop arithmetic_operation {}
	(*|b = BOOLC {if b then true else false}
	|i = ID {i}*)

paramult:
	SEMICOLON id_list COLON typ {}

parametres:
	| LPAR RPAR { sprintf "()" }
	| LPAR i = id_list COLON t = typ RPAR {sprintf "( %s : %s )" i t}
	| LPAR i = id_list COLON t = typ nonempty_list(paramult) RPAR  {sprintf "( %s : %s)" i t} 

procedure:
	PROCEDURE i = ID
	para = parametres SEMICOLON
	con = constants SEMICOLON
	var = variables SEMICOLON
	func = functions_and_procs
	BEGIN
		bloc = block
	END
	{ sprintf "id %s para %s;\n con %s;\n var %s;\n func %s\n begin \n bloc %s\n end \n" i para con var func bloc}

functions:
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
	{ if i == ret then sprintf "id %s para %s: t %s;\n con %s;\n var %s;\n func %s\n begin\n bloc %s\n end\n" i para t con var func bloc else failwith "Woop woop"}	

functions_and_procs:
	f = nonempty_list(functions , procedure){sprintf "%s" f} 

block:
	blo = list(init , blockimbr , conditionnal , boucle , appelfunction)
	{blo}	

blockimbr:
	BEGIN
	blo = block
	END
	SEMICOLON
	{ sprintf "begin \n %s end;\n" blo}


(*Le grand final*)
program:
	PROGRAM i = ID
	con = constants
	var = variables
	func = functions_and_procs
	BEGIN
		bloc = block
	END
	DOT {
		printf "My name is %s\n My vars are %s\n My functions and procs are %s\n I do %s\n" i con var func bloc
	}
