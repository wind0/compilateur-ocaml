(* PARSER *)
%{
    open Printf
    open AST
%}


(* Token *)

%token <string> VARID
%token <string> ID

%token <int32> INTC
%token <string> STRINGC

%token PROGRAM BEGIN END SEMICOLON DOT
%token NIL
%token EQ NOTEQ LT GT LE GE IN NOT
%token MINUS PLUS MULT DIV MOD PUIS
%token COMA DOUBLEDOT
%token RPAR LPAR DOTLPAR
%token INTEGER BOOLEAN
%token LBR RBR
%token ARRAY OF
%token RECORD
%token COLON COLONEQ
%token VAR CONST TYPE PROCEDURE FUNCTION
%token IF THEN ELSE CASE
%token WHILE DO REPEAT UNTIL FOR TO DOWNTO

(* Start *)

%start program
%type <AST.program> program

%nonassoc unary_minus unary_plus
%nonassoc THEN
%nonassoc ELSE

%%

(* Automate : unsigned constant  *)

unsigned_constant : 
	integ = INTC {UNormal(UInteger(integ))}
	|NIL {Nil}
	|id = STRINGC {UNormal(UString(id))}


(* Automate : constant *)

signe : 
	PLUS {Plus}
	| MINUS {Minus}

constant_id_OR_unsigned_number:
	i = VARID {BIdentified(i)}
	|integ = INTC {BInteger(integ)}

unary_signe_with_constant_id_OR_unsigned_number :
	PLUS uc = constant_id_OR_unsigned_number %prec unary_plus 
		{ SignedIdOrNumber(Plus,uc)}
	| MINUS uc2 = constant_id_OR_unsigned_number %prec unary_minus 
		{ SignedIdOrNumber(Minus,uc2)}
| uc3 = constant_id_OR_unsigned_number {IdOrNumber(uc3)}

constant:
	u = unary_signe_with_constant_id_OR_unsigned_number { u }
	|id = STRINGC {CString(id)}


(* Automate : simple type *)

para_recur_id:
	LPAR i = separated_nonempty_list(COMA,ID)  RPAR {i}


type_identifier:
	INTEGER {TypInteger} 
	| BOOLEAN {TypBoolean}

simple_type:
	t = type_identifier {Type_identifier(t)}
	| p = para_recur_id	{ID_list(p)}
	| c1 = constant DOUBLEDOT c2 = constant {Enum(c1,c2)}


(* Automate : type *)

array_para_recur_simple_type:
	ARRAY LBR i = separated_nonempty_list(COMA, simple_type) RBR OF typ=type_automate {(i,typ)}

type_automate:
	s = simple_type {Simple(s)}
	| a = array_para_recur_simple_type {Array(a)}
	| RECORD f =field_list END {Record(f)}


(* Automate : field list *)

semicolon_field_list:
	SEMICOLON f=field_list {f}

line_case_field_list:
	c=separated_nonempty_list(COMA,constant) COLON LPAR f=field_list RPAR {(c, f)}

field_list:
	i=separated_nonempty_list(COMA, ID) COLON t=type_automate sfl=semicolon_field_list?
	{
		let result = function
		(i,t,None) -> Recur(i, t)
		|(i,t,Some x) -> RecurPlus(i,t,x)
		in
		result (i,t,sfl)}
	| CASE i=ID COLON t=type_identifier OF p=separated_nonempty_list(SEMICOLON,line_case_field_list) {FCase(i,t,p)}


(* Automate : simple expression *)

recur_simple_expression:
	si=signe t2=term {(si,t2)}

simple_expression:
	s=signe? t=term r=recur_simple_expression*
	{
		let result  = function
		(None, t, r) -> USigned(t,r)
		|(Some x, t, r) -> Signed(x,t,r)
		in
		result (s,t,r)
	}


(* Automate : variable *)

boucle_intern_variable:
	LBR e= separated_nonempty_list(COMA,expression) RBR {Brackety(e)}
	 | DOT i = VARID {Dotty(i)}

variable :
	i=VARID b=boucle_intern_variable* {(i,b)}


(* Automate : factor *)

after_function_identifier:
	LPAR e=separated_nonempty_list(COMA,expression)  RPAR {e}

after_LBR:
	e=separated_nonempty_list(COMA, expression) {e}

factor:
	u = unsigned_constant {UConst(u)}
	| v = variable {Var(v)}
	|funct = ID a=after_function_identifier? 
	{
	let result = function
	(funct, None) -> Function(funct, [])
	|(funct, Some exp) -> Function(funct, exp)
	in
	result (funct, a)
	}
	|DOTLPAR e = expression RPAR {Expression(e)}
	|NOT f = factor {Neg_factor(f)}
	|LBR a=after_LBR? RBR 
	{
	let result = function
	None -> Brackets([])
	| Some x -> Brackets(x)
	in
	result a
	}


(* Automate : term *)

operator_term:
	MULT {Times}
	|DIV {Div}
	|MOD {Mod}
	|PUIS {Pow} (*One of us, one of us*)

mult_factor:
	o = operator_term f = factor {(o,f)} 	

term:
	f = factor m = mult_factor* {(f,m)}


(* Automate : expression *)

opexp:
	EQ {Eq}
	|NOTEQ {Neq}
	|LT {Lt}
	|GT {Gt}
	|LE {Le}
	|GE {Ge}
	|IN {In}

opexp_with_simple_expression:
	o = opexp s2 = simple_expression {(o,s2)}

expression:
	s = simple_expression o=opexp_with_simple_expression?
	{
	let result = function
	(s, None) -> ESimple(s)
	|(s,Some (o, s2)) -> EOperation (s,o,s2)
	in result(s,o)
	}


(* Automate : statement *)

expr_or_procid:
	id = ID {Id(id)}
	| e = expression {Expr(e)}

expr_proc:
	LPAR i=separated_nonempty_list(COMA,expr_or_procid) RPAR {i}

single_case:
	c = separated_nonempty_list(COMA,constant) COLON s = statement {(c, s)}

incr_decr:
	TO {To}
	|DOWNTO {Downto}

variable_or_id:
	v = variable {Variable(v)}
	|func = ID {Id2(func)} 

statement:
(* oui statement peut etre vide *)
	|v = variable_or_id COLONEQ e = expression {Affect(v,e)}
	|i = ID e = expr_proc? {let result = function (i, None) -> ProcInit(i,[])| (i, Some x) -> ProcInit(i,x) in result (i,e)}
	|BEGIN s = separated_nonempty_list(SEMICOLON, statement) END {Embedded(s)}
	|IF e = expression THEN s = statement {IfThen(e,s)}   
	|IF e = expression THEN s = statement ELSE s2 = statement {IfThenElse(e,s,s2)}
	|CASE e = expression OF a = separated_nonempty_list(SEMICOLON, single_case) END {Case(e,a)}
	|WHILE e= expression DO s = statement {While(e,s)}
	|REPEAT s = separated_nonempty_list(SEMICOLON, statement) UNTIL e = expression {Repeat(s,e)}
	|FOR i = VARID COLONEQ e = expression inc = incr_decr e2 = expression DO s = statement {For(i,e,inc,e2,s)} 


(* Automate : block *)


(*Constante*)
init_const:
	i = VARID EQ c = constant SEMICOLON 
	{(i,c)}

block_const:
	CONST
	i = init_const+
	{i}


(*Type*)
init_type:
	i = VARID EQ t = type_automate SEMICOLON
	{(i,t)}

block_type:
    TYPE
    i = init_type+
    {i}


(*Var*)
init_var:
	i = separated_nonempty_list(COMA, VARID) COLON t = type_automate SEMICOLON {(i,t)}

block_var:
	VAR
	i = init_var+
	{i}


(* Automate : parameter list*)

mult_parameter:
SEMICOLON p = under_parameter_list {p}

under_parameter_list:
i = separated_nonempty_list(COMA,VARID) COLON t = type_identifier {ClassicParameter(i,t,NoneParameter)}
| i = separated_nonempty_list(COMA,VARID) COLON t = type_identifier mp = mult_parameter {ClassicParameter(i,t,mp)}

| FUNCTION i = separated_nonempty_list(COMA,ID) COLON t = type_identifier {FunctionParameter(i,t,NoneParameter) }
| FUNCTION i = separated_nonempty_list(COMA,ID) COLON t = type_identifier mp = mult_parameter {FunctionParameter(i,t,mp)}

| VAR i = separated_nonempty_list(COMA,VARID) COLON t = type_identifier {VariableParameter(i,t,NoneParameter)}
| VAR i = separated_nonempty_list(COMA,VARID) COLON t = type_identifier mp = mult_parameter {VariableParameter(i,t,mp)}

| PROCEDURE i = separated_nonempty_list(COMA,ID) {ProcedureParameter(i,NoneParameter)}
| PROCEDURE i = separated_nonempty_list(COMA,ID) mp = mult_parameter {ProcedureParameter(i, mp)}

parameter_list:
LPAR u = under_parameter_list RPAR {u}


(* Automate : procedure et function *)

procedure:
	PROCEDURE i = ID pa = parameter_list? SEMICOLON b = block SEMICOLON
	{
		let para = (function None -> NoneParameter | Some x -> x) pa
	in
		{
			proc_name = i;
			proc_parameters = para;
			proc_body = b;
		}
	}	

function_bl:
	FUNCTION i = ID pa = parameter_list? COLON t = type_identifier SEMICOLON b = block SEMICOLON
	{
		let para = (function None -> NoneParameter | Some x -> x) pa
	in
		{
			func_name = i;
			func_parameters = para;
			func_return_type = t;
			func_body = b;
		}
	}



(* Main : block *)

block:
	bc = block_const?
	bt = block_type?
	bv = block_var?
	pro = procedure*
	func = function_bl*
	BEGIN
		s = separated_list(SEMICOLON, statement)
	END
	{ 
			let cons = (function None -> [] | Some x -> x) bc in
			let t = (function None -> [] | Some x -> x) bt in 
			let vars = (function None -> [] | Some x -> x) bv
		in
		{
			constants = cons;
			types = t;
			variables = vars;
			procedures = pro;
			functions = func;
			statements = s;
		}	
	}


(* Pseudo main : Structure principale d'un programme PASCAL *)

program:
	PROGRAM i = ID SEMICOLON
		b = block
	DOT
	{{
		prog_name = i;
		prog_body = b;
	}}

%%
