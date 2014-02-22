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
%token WRITELN

%start <unit> program


%left OR
%left AND
%nonassoc NOT
%nonassoc LT GT LE GE EQ NEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc LBR
%nonassoc THEN
%nonassoc ELSE


%%
typ:
INTEGER {sprintf "integer"}
|BOOLEAN {sprintf "boolean"}
|ARRAY OF t = typ {sprintf "array of %s" t}

id_list:
 i = ID { i }
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
	{ sprintf "%s\n" (String.concat "" v)}

idOrIntc:
	i = ID {i}
	|i = INTC {sprintf "%li" i}

%inline binop:
	PLUS {"+"}
	|MINUS {"-"}
	|TIMES {"*"}
	|DIV {"/"}
	|MOD {"%"}

arithmetic_operation:
	i = ID {i}
	|i = INTC {sprintf "%li" i}
	|a = arithmetic_operation b = binop c = arithmetic_operation {sprintf "%s %s %s" a b c}
	|LPAR a = arithmetic_operation RPAR {sprintf "( %s )" a}

%inline binlogop:
	AND {"&&"}
	|OR {"||"}
	|LT {"<"}
	|GT {">"}
	|GE {">="}
	|LE {"<="}
	|EQ {"=="}
	|NEQ {"!="}

operation:
	NOT option(LPAR) a = operation option(RPAR) {sprintf "! %s" a}
	|a = arithmetic_operation b = binlogop c = arithmetic_operation {sprintf "%s %s %s" a b c}
	| NIL {""}
	| WRITELN {sprintf "writeln"}
	|b = BOOLC {sprintf "%B" b }
	(* |i = ID {i} *)

init:
 	i = nonempty_list(arithmetic_operation) {String.concat "" i}	

paramult:
	SEMICOLON i = id_list COLON t = typ {sprintf "; %s : %s" i t}

parametres:
	 LPAR RPAR { sprintf "()" }
	| LPAR i = id_list COLON t = typ RPAR {sprintf "( %s : %s )" i t}
	| LPAR i = id_list COLON t = typ p = nonempty_list(paramult) RPAR  {sprintf "( %s : %s) %s" i t (String.concat "" p)} 

const_decl:
	 i = ID EQ b = BOOLC SEMICOLON {sprintf "%s = %B;" i b}
	| i = ID EQ inte = INTC SEMICOLON {sprintf "%s = %li ;" i inte}

constants:
	CONST
	cons = nonempty_list(const_decl)
	{sprintf " %s " (String.concat "" cons)}

procedure:
	PROCEDURE i = ID
	para = parametres SEMICOLON
	con = option(constants)
	vars = option(var)
	func = option(functions_and_procs)
	 BEGIN
		bloc = block
	END
	SEMICOLON
	{ 	let machin = fun truc ->
		match truc with
		| None -> ""
		| Some x -> x
		in
		let con2 = machin con
		and vars2 = machin vars 
		and func2 = machin func
		in   
		sprintf "id %s para %s;\n con %s;\n var %s;\n func %s\n begin \n bloc %s\n end \n" 
		i para con2 vars2 func2 bloc}

functions:
	FUNCTION i = ID 
	para = parametres SEMICOLON
	con = option(constants)
	t = typ SEMICOLON
	vars = option(var)
	func = option(functions_and_procs)
	BEGIN
		bloc = block
		ID CCOLONEQ v = var
	END
	SEMICOLON
	{
		let machin = fun truc ->
		match truc with
		| None -> ""
		| Some x -> x
		in 
		let con2 = machin con
		and vars2 = machin vars 
		and func2 = machin func
		in 
		sprintf "id %s para %s: t %s;\n con %s;\n var %s;\n func %s\n begin\n bloc %s\n returns %s end\n" 
		i para t con2 vars2 func2 bloc v}	

option_funcproc:
	func = functions {sprintf "%s" func}
	| pro = procedure {sprintf "%s" pro}

functions_and_procs:
	f = list(option_funcproc){sprintf "%s" (String.concat "" f)} 

appelfunction:
	i = ID LPAR id = id_list RPAR {sprintf "%s ( %s )" i id}

ifthen:
	IF ope = operation THEN bl = option_block {sprintf "if %s then %s" ope bl}
	| IF ope = operation THEN bl1 = option_block ELSE bl2 = option_block {sprintf "if %s then %s else %s" ope bl1 bl2}
	| IF ope = operation THEN BEGIN bl = block END {sprintf "if %s then %s" ope bl}
	| IF ope = operation THEN BEGIN bl1 = block END ELSE BEGIN bl2 = block END {sprintf "if %s then %s else %s" ope bl1 bl2}

listswitch:
 	 i = INTC COLON  o = option_block {sprintf "%li : %s" i o }
 	| b = BOOLC COLON o = option_block {sprintf "%B : %s" b o}

listswitchlist:
	l = listswitch SEMICOLON l2 = listswitchlist {sprintf "%s ; %s" l l2}
	|l = listswitch {l}


switch:
	 CASE i = ID OF l = listswitchlist END {sprintf "case %s of %s end\n" i l}

conditionnal:
	i = ifthen { i }
	|i = switch { i }

whiledo:
	 WHILE o = operation DO  o2 = option_block {sprintf "while %s do\n %s\n" o o2}
	| WHILE o = operation DO BEGIN b = block END {sprintf "while %s do\n begin\n %s\n end\n" o b}

fordo:
	 FOR i = ID COLONEQ i2 = idOrIntc DOWNTO i3 = idOrIntc DO o = option_block {sprintf "for %s := %s downto %s do\n %s\n" i i2 i3 o}
	| FOR i = ID COLONEQ i2 = idOrIntc DOWNTO i3 = idOrIntc DO BEGIN b = block END {sprintf "for %s := %s downto %s do\n begin\n %s\n end\n" i i2 i3 b}
	| FOR i = ID COLONEQ i2 = idOrIntc TO i3 = idOrIntc DO o = option_block {sprintf "for %s := %s to %s do\n %s\n" i i2 i3 o}
	| FOR i = ID COLONEQ i2 = idOrIntc TO i3 = idOrIntc DO BEGIN b = block END {sprintf "for %s := %s to %s do\n begin\n %s\n end\n" i i2 i3 b}

boucle:
	w = whiledo {w}
	|f = fordo {f}

option_block:
ini = init {sprintf "%s" ini}
|bloc = blockimbr {sprintf "%s" bloc}
|cond = conditionnal {sprintf "%s" cond}
|bouc = boucle {sprintf "%s" bouc}
|appel = appelfunction {sprintf "%s" appel}


block:
 blo = nonempty_list(option_block)
	{sprintf "begin\n %s \n end\n" (String.concat "" blo)}	

blockimbr:
	BEGIN
	blo = block
	END
	SEMICOLON
	{ sprintf "begin \n %s end;\n" blo}


(*Le grand final*)
program:
	PROGRAM i = ID
	con = option(constants)
	vars = option(var)
	func = option(functions_and_procs)
	BEGIN
		bloc = block
	END
	DOT {
		let machin = fun truc ->
		match truc with
		| None -> ""
		| Some x -> x
		in
		let con2 = machin con 
		and vars2 = machin vars
		and func2 = machin func
		in
		printf "My name is %s\n My constants are %s\n My vars are %s\n My functions and procs are %s\n I do %s\n" i con2 vars2 func2 bloc
	}
