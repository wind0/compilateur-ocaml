%{
    open Printf
%}

/* Token */
%token <string> ID
%token <int32> INTC
%token PROGRAM BEGIN END SEMICOLON DOT
%token SIMPLECOTE NIL EQ
%token MINUS PLUS
%token COMA DOUBLEDOT
%token RPAR LPAR
%token INTEGER BOOLEAN
%token LBR RBR
%token ARRAY OF
%token RECORD
%token COLON
%token CASE
%token VAR CONST TYPE PROCEDURE FUNCTION

/* Start du parser */
%start program
%type <unit> program

%nonassoc unary_minus unary_plus
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
	PLUS {sprintf "+"}
	| MINUS {sprintf "-"}

constant_id_OR_unsigned_number:
	i = ID {i}
	|integ = INTC {sprintf " %li" integ}

unary_signe_with_constant_id_OR_unsigned_number :
	PLUS uc = constant_id_OR_unsigned_number %prec unary_plus 
		{ sprintf "+ %s" uc}
	| MINUS uc2 = constant_id_OR_unsigned_number %prec unary_minus 
		{ sprintf "- %s" uc2}
	| uc3 = constant_id_OR_unsigned_number {uc3}

constant:
	u = unary_signe_with_constant_id_OR_unsigned_number
	{ u }
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
(*

(* automate : simple expression *)
simple_expression:
	s=signe? term {s}

(* automate : variable *)
recur_expression:
	COMA e=expression {sprintf ", %s" e}

boucle_intern:
	LBR expression re=recur_expression* RBR {sprintf " [ %s ]" (String.concat "" re)}
	| DOT i = ID {sprintf ". %s" i}

variable : 
	i = ID b=boucle_intern* {sprintf "%s %s" i, (String.concat "" b)}

*)


(* BLOCK *)


(*Constante*)
init_const:
	i = ID EQ c = constant SEMICOLON 
	{sprintf "%s = %s ;" i c}

block_const:
	CONST
	i = init_const+
	{sprintf "const\n %s" (String.concat "" i)}


(*Type*)
init_type:
	i = ID EQ t = type_automate SEMICOLON
	{sprintf "%s = %s ;" i t}

block_type:
    TYPE
    i = init_type+
    {sprintf "type\n %s" (String.concat "" i)}


(*Var*)
mult_var:
	COMA i = ID {sprintf ", %s" i}

init_var:
	i = ID m = mult_var* COLON t = type_automate SEMICOLON {sprintf "%s %s : %s ;" i (String.concat "" m) t}

block_var:
	VAR
	i = init_var+
	{sprintf "var\n %s" (String.concat "" i)}


(*Procedure et Function*)
mult_parameter:
	SEMICOLON p = under_parameter_list {sprintf "; %s" p}

under_parameter_list:
	i = ID m = mult_var* COLON t = type_identifier mp = mult_parameter* {sprintf "%s %s : %s %s" i (String.concat "" m) t (String.concat "" mp)}
	| FUNCTION i = ID m = mult_var* COLON t = type_identifier mp = mult_parameter* {sprintf "function %s %s : %s %s " i (String.concat "" m) t (String.concat "" mp)}
	| VAR i = ID m = mult_var* COLON t = type_identifier mp = mult_parameter* {sprintf "var %s %s : %s %s " i (String.concat "" m) t (String.concat "" mp)}
	| PROCEDURE i = ID m = mult_var* mp = mult_parameter* {sprintf "procedure %s %s %s" i (String.concat "" m) (String.concat "" mp)}

parameter_list:
	LPAR u=under_parameter_list mp=mult_parameter* RPAR {sprintf "( %s %s )" u (String.concat "" mp)}

procedure:
	PROCEDURE i = ID pa = parameter_list? SEMICOLON b = block SEMICOLON
	{let extract = fun rechercher ->
		match rechercher with
		| None -> ""
		| Some x -> x
	in
		let para = extract pa
	in
	sprintf "procedure %s %s ; %s ;" i para b}	

function_bl:
	FUNCTION i = ID pa = parameter_list? COLON t = type_identifier SEMICOLON b = block SEMICOLON
	{let extract = fun rechercher ->
		match rechercher with
		| None -> ""
		| Some x -> x
	in
		let para = extract pa
	in
	sprintf "function %s %s : %s ; %s ;" i para t b}

(* Main block *)

block:
	bc = block_const?
	bt = block_type?
	bv = block_var?
	pro = procedure*
	func = function_bl*
	BEGIN
		(*du debug, pour le moment*)
		(* b = unsigned_constant* *)
		(*c = constant* *)
		(*s = simple_type* *)
		(*t = type_automate* *)
		(*f = field_list* *)
	END
	{ 	let extract = fun rechercher ->
			match rechercher with
			| None -> ""
			| Some x -> x
		in
			let cons = extract bc
		in	
			let types = extract bt
		in
			let vars = extract bv
		in
		sprintf "%s \n %s \n %s \n %s \n %s \n begin\n end" cons types vars (String.concat "" pro) (String.concat "" func) }


(* pseudo main : Structure principale d'un programme PASCAL *)
program:
	PROGRAM i = ID SEMICOLON
		b = block
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
	{printf "program %s;\n %s.\n\n" i b}

%%