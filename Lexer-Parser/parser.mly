%{
    open Printf

    let extract = fun rechercher ->
	match rechercher with
	| None -> ""
	| Some x -> x

%}


/* Token */
%token <string> ID
%token <int32> INTC
%token PROGRAM BEGIN END SEMICOLON DOT
%token SIMPLECOTE NIL
%token EQ NOTEQ LT GT LE GE IN NOT
%token MINUS PLUS MULT DIV MOD PUIS
%token COMA DOUBLEDOT
%token RPAR LPAR
%token INTEGER BOOLEAN
%token LBR RBR
%token ARRAY OF
%token RECORD
%token COLON COLONEQ
%token VAR CONST TYPE PROCEDURE FUNCTION
%token IF THEN ELSE CASE
%token WHILE DO REPEAT UNTIL FOR TO DOWNTO

(* Start du parser *)
%start program
%type <unit> program

%nonassoc COLONEQ
%nonassoc unary_minus unary_plus
%nonassoc THEN
%nonassoc ELSE
(*

%nonassoc UNARYPLUS UNARYMINUS
%left COMA
%left LPAR
*)

%%
(* automate : unsigned constant  *)

unsigned_constant : 
	(*i = ID {i} *)
	integ = INTC {sprintf " %li" integ}
	|NIL {sprintf " nil"}
	|SIMPLECOTE id = ID SIMPLECOTE {sprintf " ' %s ' " id}

(* automate : constant *)
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

para_recur_id:
	LPAR i = separated_nonempty_list(ID,COMA)  RPAR {sprintf "(%s)" (String.concat "" i)}


type_identifier:
	INTEGER {sprintf "integer "} | BOOLEAN {sprintf "boolean "}

simple_type:
	t = type_identifier {t}
	| p = para_recur_id	{p}
	| c1 = constant DOUBLEDOT c2 = constant {sprintf " %s .. %s" c1 c2}

(* automate : type *)

array_para_recur_simple_type:
	ARRAY LBR i = separated_nonempty_list(COMA, simple_type) RBR OF typ=type_automate {sprintf "array [%s] of %s" (String.concat "" i) typ}

type_automate:
	s = simple_type {sprintf "%s\n " s}
	| a = array_para_recur_simple_type {a}
	| RECORD f =field_list END {sprintf "record %s end" f}

(* automate : field list *)
semicolon_field_list:
	SEMICOLON f=field_list {sprintf "; %s" f}

line_case_field_list:
	c=separated_nonempty_list(COMA,constant) COLON LPAR f=field_list RPAR {sprintf "%s : ( %s )" (String.concat "" c) f}

field_list:
	i=separated_nonempty_list(COMA, ID) COLON t=type_automate sfl=semicolon_field_list?
	{
		let sstr = extract sfl 
		in
		sprintf "%s : %s %s" (String.concat "" i) t sstr}
	| CASE i=ID COLON t=type_identifier OF p=separated_nonempty_list(SEMICOLON,line_case_field_list) {sprintf "case %s : %s of %s "i t (String.concat "" p)}



(* automate : simple expression *)

simple_expression:
	s=signe? t= separated_nonempty_list(signe, term)
	{
		let sstr = extract s
		in
		sprintf "%s %s" sstr (String.concat "" t)
	}

(* automate : variable *)

boucle_intern_variable:
	LBR e= separated_nonempty_list(COMA,expression) RBR {sprintf " [ %s ]"(String.concat "" e)}
	| DOT i = ID {sprintf ". %s" i}

variable :
	(* triste *) 
	i=ID b=boucle_intern_variable* {sprintf " %s"(String.concat "" b)}


(* automate factor *)

(*
(** ancienne version **)
mult_expression:
	COMA e = expression {sprintf ", %s" e}

after_function_identifier:
	LPAR m=mult_expression em=mult_expression* RPAR {sprintf " ( %s %s )" m (String.concat "" em)}
*)

after_function_identifier:
	LPAR e=separated_nonempty_list(COMA,expression)  RPAR {sprintf " ( %s )" (String.concat "" e)}

after_LBR:
	e=separated_nonempty_list(COMA, expression) { sprintf "%s" (String.concat "" e)}

factor:
	(* les 3 suivantes *)
	u = unsigned_constant {u}
	| v = variable {v}
	|funct = ID a=after_function_identifier? 
	{
	let afi = extract a
	in
	sprintf "%s %s" funct afi
	}
	|LPAR e = expression RPAR {sprintf " ( %s )" e}
	|NOT f = factor {sprintf "!%s" f}
	|LBR a=after_LBR? RBR 
	{
	let aflbr = extract a
	in
	sprintf "[%s]" aflbr
	}

(* automate term *)

operator_term:
	MULT {sprintf "*"}
	|DIV {sprintf "/"}
	|MOD {sprintf "mod"}
	|PUIS {sprintf "^"}

term:
	f = separated_nonempty_list(operator_term, factor) {sprintf "%s" (String.concat "" f)}



(* automate expression *)

opexp:
	EQ {sprintf "="}
	|NOTEQ {sprintf "!="}
	|LT {sprintf "<"}
	|GT {sprintf ">"}
	|LE {sprintf "<="}
	|GE {sprintf ">="}
	|IN {sprintf "in"}

opexp_with_simple_expression:
	o = opexp s2 = simple_expression {sprintf "%s %s" o s2}

expression:
	s = simple_expression o=opexp_with_simple_expression?
	{
	let owse = extract o
	in
	sprintf "%s %s" s owse
	}



(* automate statement *)

expr_or_procid:
	id = ID {id}
	| e = expression {e}

expr_proc:
	LPAR i=separated_nonempty_list(COMA,expr_or_procid) RPAR {sprintf "( %s )" (String.concat "" i)}

single_case:
	c = separated_nonempty_list(COMA,constant) COLON s = statement {sprintf "%s : %s" (String.concat "" c) s}

incr_decr:
	TO {sprintf "to"}
	|DOWNTO {sprintf "downto"}

(*else_pos:
	ELSE s = statement {sprintf "else %s" s}	*)

statement:
	v = variable COLONEQ e = expression {sprintf "%s := %s" v e}
	|i = ID COLONEQ e = expression {sprintf "%s := %s" i e}
	|i = ID e = expr_proc? {let expr = extract e in sprintf "%s" expr}
	|BEGIN s = separated_nonempty_list(SEMICOLON,statement) END {sprintf "begin %s end" (String.concat "" s)}
	|IF e = expression THEN s = statement {sprintf "if %s then %s " e s}   
	|IF e = expression THEN s = statement ELSE s2 = statement {sprintf "if %s then %s else %s" e s s2}   
	|CASE e = expression OF s = separated_nonempty_list(SEMICOLON,single_case) END {sprintf "case %s of %s end" e (String.concat "" s)}
	|WHILE e= expression DO s = statement {sprintf "while %s do %s" e s}
	|REPEAT s = separated_nonempty_list(SEMICOLON, statement) UNTIL e = expression {sprintf "repeat %s until %s" (String.concat "" s) e}
	|FOR i = ID COLONEQ e = expression inc = incr_decr e2 = expression DO s = statement {sprintf "for %s := %s %s %s do %s" i e inc e2 s} 

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

init_var:
	i = separated_nonempty_list(COMA, ID) COLON t = type_automate SEMICOLON {sprintf "%s : %s ;" (String.concat "" i) t}

block_var:
	VAR
	i = init_var+
	{sprintf "var\n %s" (String.concat "" i)}


(*Procedure et Function*)
mult_parameter:
	SEMICOLON p = under_parameter_list {sprintf "; %s" p}

under_parameter_list:
	i = separated_nonempty_list(COMA,ID) COLON t = type_identifier mp = mult_parameter* {sprintf "%s : %s %s" (String.concat "" i) t (String.concat "" mp)}
	| FUNCTION i = separated_nonempty_list(COMA,ID) COLON t = type_identifier mp = mult_parameter* {sprintf "function %s : %s %s " (String.concat "" i) t (String.concat "" mp)}
	| VAR i = separated_nonempty_list(COMA,ID) COLON t = type_identifier mp = mult_parameter* {sprintf "var %s : %s %s " (String.concat "" i) t (String.concat "" mp)}
	| PROCEDURE i = separated_nonempty_list(COMA,ID) mp = mult_parameter* {sprintf "procedure %s %s" (String.concat "" i) (String.concat "" mp)}

parameter_list:
	LPAR u=under_parameter_list mp=mult_parameter* RPAR {sprintf "( %s %s )" u (String.concat "" mp)}

procedure:
	PROCEDURE i = ID pa = parameter_list? SEMICOLON b = block SEMICOLON
	{
		let para = extract pa
	in
	sprintf "procedure %s %s ; %s ;" i para b}	
	(* fonction *)
function_bl:
	FUNCTION i = ID pa = parameter_list? COLON t = type_identifier SEMICOLON b = block SEMICOLON
	{
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
		s = separated_nonempty_list(SEMICOLON, statement)
	END
	{ 
			let cons = extract bc in
			let types = extract bt in 
			let vars = extract bv
		in
		sprintf "%s \n %s \n %s \n %s \n %s \n begin\n %s end" cons types vars (String.concat "" pro) (String.concat "" func) (String.concat "" s) }

(* pseudo main : Structure principale d'un programme PASCAL *)
program:
	PROGRAM i = ID SEMICOLON
		b = block
		(*e = expression* *)
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
	{sprintf "program %s;\n %s.\n" i b}

%%
