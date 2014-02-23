%{
    open Printf
%}

/* Token */
%token <string> ID
%token <int32> INTC
%token PROGRAM BEGIN END SEMICOLON DOT
%token SIMPLECOTE NIL
%token UNARYPLUS UNARYMINUS
%token COMA DOUBLEDOT
%token RPAR LPAR
%token INTEGER BOOLEAN

/* Start du parser */
%start program
%type <unit> program

/*
%nonassoc UNARYPLUS UNARYMINUS
%left COMA
%left LPAR
*/

%%
/* automate : unsigned constant  */
unsigned_constant : 
	i = ID {i}
	|integ = INTC {sprintf " %li" integ}
	|NIL {sprintf " nil"}
	|SIMPLECOTE id = ID SIMPLECOTE {sprintf " ' %s ' " id}

/* automate : constant */
signe : 
	UNARYPLUS {sprintf "+"}
	| UNARYMINUS {sprintf "-"}

constant_id_OR_unsigned_number:
	i = ID {i}
	|integ = INTC {sprintf " %li" integ}

constant:
	s = signe?  csoun = constant_id_OR_unsigned_number 
	{	let extract = fun rechercher ->
		match rechercher with
		| None -> ""
		| Some x -> x
		in
		let sign = extract s
		in
		sprintf " %s %s " sign csoun}
	|SIMPLECOTE id = ID SIMPLECOTE {sprintf " ' %s ' " id}

(* automate : simple type *)
recur_id:
	COMA id = ID r=recur_id* {sprintf ", %s %s" id (String.concat "" r)}

para_recur_id:
	LPAR i = ID r = recur_id*  RPAR {sprintf "(%s %s)" i (String.concat "" r)}

type_identifier:
	INTEGER {sprintf "integer "} | BOOLEAN {sprintf "boolean "}

simple_type:
	t = type_identifier {t}
	| p = para_recur_id	{p}
	| c1 = constant DOUBLEDOT c2 = constant {sprintf " %s .. %s" c1 c2}

(* pseudo main : Structure principale d'un programme PASCAL *)
program:
	PROGRAM i = ID SEMICOLON
	BEGIN

	(*du debug, pour le moment*)
	(*b = unsigned_constant* *)
	(* c = constant* *)
	s = simple_type*
	END
	DOT
	(*{
		let extract = fun rechercher ->
		match rechercher with
		| None -> ""
		| Some x -> x
		in
		let bstr = extract b 
		in
		printf "program %s;\n begin\n %s\n end.\n\n" i bstr}
	*)
	{printf "program %s;\n begin\n %s\n end.\n\n" i (String.concat "" s)}

%%