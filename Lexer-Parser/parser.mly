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
%token LBR RBR
%token ARRAY OF
%token RECORD
%token COLON
%token CASE

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
	COMA id = ID {sprintf ", %s" id}

para_recur_id:
	LPAR i = ID r = recur_id*  RPAR {sprintf "(%s %s)" i (String.concat "" r)}


type_identifier:
	INTEGER {sprintf "integer "} | BOOLEAN {sprintf "boolean "}

simple_type:
	t = type_identifier {t}
	| p = para_recur_id	{p}
	| c1 = constant DOUBLEDOT c2 = constant {sprintf " %s .. %s" c1 c2}

(* automate : type *)
recur_type_array:
	COMA id = simple_type {sprintf ", %s" id }

array_para_recur_simple_type:
	ARRAY LBR i = simple_type r = recur_type_array* RBR OF typ=type_automate {sprintf "array [%s %s] of %s" i (String.concat "" r) typ}

type_automate:
	s = simple_type {sprintf "%s\n " s}
	| a = array_para_recur_simple_type {a}
	| RECORD f =field_list END {sprintf "record %s end" f}

(* automate : field list *)
semicolon_field_list:
	SEMICOLON f=field_list {sprintf "; %s" f}

recur_const:
	COMA c = constant {sprintf ", %s" c}

line_case_field_list:
	c=constant rc=recur_const* COLON LPAR f=field_list RPAR {sprintf "%s %s : ( %s )"c (String.concat "" rc) f}

recur_line_case_field_list:
	SEMICOLON p=line_case_field_list {sprintf "; %s" p}

field_list:
	i=ID r=recur_id* COLON t=type_automate sfl=semicolon_field_list?
	{
		let extract = fun rechercher ->
		match rechercher with
		| None -> ""
		| Some x -> x
		in
		let sstr = extract sfl 
		in
		sprintf "%s %s : %s %s" i (String.concat "" r) t sstr}
	| CASE i=ID COLON t=type_identifier OF p=line_case_field_list b = recur_line_case_field_list* {sprintf "case %s : %s of %s %s"i t p (String.concat "" b)}

(* pseudo main : Structure principale d'un programme PASCAL *)
program:
	PROGRAM i = ID SEMICOLON
	BEGIN

	(*du debug, pour le moment*)
	(* b = unsigned_constant* *)
	(*c = constant* *)
	(*s = simple_type* *)
	(*t = type_automate* *)
	f = field_list*
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
	{printf "program %s;\n begin\n %s\n end.\n\n" i (String.concat "" f)}

%%