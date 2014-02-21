%{
    open Printf
%}

%token <int32> INTC
%token <string> STRINGC
%token <bool> BOOLC
%token <string> ID
%token INTEGER BOOLEAN ARRAY
%token VAR PROGRAM BEGIN END CONST
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
	|NOT option(LPAR) a = operation option(RPAR) {sprintf "! %s" a}
	|arithmetic_operation binlogop arithmetic_operation {}
	
	(* Les deux suivantes posent probleme pour menhir *)
	(*|b = BOOLC {if b then true else false}
	|i = ID {i}*)

init:
 	nonempty_list(arithmetic_operation) {}	

paramult:
	SEMICOLON id_list COLON typ {}

parametres:
	| LPAR RPAR { sprintf "()" }
	| LPAR i = id_list COLON t = typ RPAR {sprintf "( %s : %s )" i t}
	| LPAR i = id_list COLON t = typ nonempty_list(paramult) RPAR  {sprintf "( %s : %s)" i t} 

const_decl:
	| i = ID EQ b = BOOLC SEMICOLON {sprintf "%s = %s;" i b}
	| i = ID EQ inte = INTEGER SEMICOLON {sprintf "%s = %s;" i inte}

constants:
	CONST
	cons = nonempty_list(const_decl)
	{sprintf " %s " cons}

procedure:
	PROCEDURE i = ID
	para = parametres SEMICOLON
	con = constants
	vars = var
	func = functions_and_procs
	BEGIN
		bloc = block
	END
	{ sprintf "id %s para %s;\n con %s;\n var %s;\n func %s\n begin \n bloc %s\n end \n" i para con vars func bloc}

functions:
	FUNCTION i = ID 
	para = parametres COLON
	t = typ SEMICOLON
	con = constants
	vars = var
	func = functions_and_procs
	BEGIN
		bloc = block
		ret = ID CCOLONEQ var
	END
	{ if i == ret then sprintf "id %s para %s: t %s;\n con %s;\n var %s;\n func %s\n begin\n bloc %s\n end\n" i para t con vars func bloc else failwith "Woop woop"}	

option_funcproc:
|func = functions {sprintf "%s" func}
|pro = procedure {sprintf "%s" pro}

functions_and_procs:
	f = nonempty_list(option_funcproc){sprintf "%s" f} 

appelfunction:
	i = ID LPAR id = id_list RPAR {sprintf "%s ( %s )" i id}

ifthen:
| IF ope = operation THEN bl = block {sprintf "if %s then %s" ope bl}
| IF ope = operation THEN bl1 = block ELSE bl2 = block {sprintf "if %s then %s else %s" ope bl1 bl2}

multlistcase:
	INTC COLON option_block SEMICOLON {}

listcase:
 	list(multlistcase) INTC COLON option_block {}

switch:
| CASE ID OF listcase END {}

conditionnal:
|ifthen {}
|switch {}

whiledo:
	| WHILE operation DO  option_block {}
	| WHILE operation DO BEGIN nonempty_list(option_block) END {}

fordo:
	| FOR ID COLONEQ ID TO ID DO option_block {}
	| FOR ID COLONEQ ID TO ID DO BEGIN nonempty_list(option_block) END {}

boucle:
|whiledo {}
|fordo {}

option_block:
|ini = init {sprintf "%s" ini}
|bloc = blockimbr {sprintf "%s" bloc}
|cond = conditionnal {sprintf "%s" cond}
|bouc = boucle {sprintf "%s" bouc}
|appel = appelfunction {sprintf "%s" appel}

block:
	blo = list(option_block)
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
	vars = var
	func = functions_and_procs
	BEGIN
		bloc = block
	END
	DOT {
		printf "My name is %s\n My vars are %s\n My functions and procs are %s\n I do %s\n" i con vars func bloc
	}
