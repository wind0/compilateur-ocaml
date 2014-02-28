
%%
(* automate : unsigned constant  *)

unsigned_constant : 
	INTC |NIL | STRINGC

(* automate : constant *)
signe : 
	PLUS | MINUS

constant_id_OR_unsigned_number:
	VARID | INTC

unary_signe_with_constant_id_OR_unsigned_number :
	PLUS constant_id_OR_unsigned_number
	| MINUS constant_id_OR_unsigned_number 
	| constant_id_OR_unsigned_number


constant:
	unary_signe_with_constant_id_OR_unsigned_number
	| STRINGC

(* automate : simple type *)

para_recur_id:
	LPAR ID (COMA,ID)* RPAR

type_identifier:
	INTEGER | BOOLEAN

simple_type:
	type_identifier | para_recur_id | constant DOUBLEDOT constant

(* automate : type *)

array_para_recur_simple_type:
	ARRAY LBR simple_type (COMA, simple_type)* RBR OF type_automate

type_automate:
	simple_type | array_para_recur_simple_type | RECORD field_list END

(* automate : field list *)
semicolon_field_list:
	SEMICOLON field_list

line_case_field_list:
	constant (COMA,constant)* COLON LPAR field_list RPAR

field_list:
	ID (COMA, ID)* COLON type_automate semicolon_field_list?
	| CASE ID COLON type_identifier OF line_case_field_list (SEMICOLON,line_case_field_list)*

(* automate : simple expression *)
recur_simple_expression:
	signe term

simple_expression:
	signe? term recur_simple_expression*

(* automate : variable *)

boucle_intern_variable:
	LBR expression (COMA,expression)* RBR
	| DOT VARID

variable :
	VARID boucle_intern_variable*


(* automate factor *)

after_function_identifier:
	LPAR expression (COMA,expression)* RPAR

after_LBR:
	expression (COMA, expression)*

factor:
	unsigned_constant
	| variable
	|ID after_function_identifier? 
	|DOTLPAR expression RPAR
	|NOT factor
	|LBR after_LBR? RBR 

(* automate term *)

operator_term:
	MULT
	|DIV 
	|MOD 
	|PUIS

mult_factor:
	operator_term factor

term:
	factor mult_factor*



(* automate expression *)

opexp:
	EQ
	|NOTEQ 
	|LT 
	|GT 
	|LE 
	|GE 
	|IN 

opexp_with_simple_expression:
	opexp simple_expression

expression:
	simple_expression opexp_with_simple_expression?


(* automate statement *)

expr_or_procid:
	ID	| expression

expr_proc:
	LPAR expr_or_procid (COMA,expr_or_procid)* RPAR

single_case:
	constant (COMA,constant)* COLON statement

incr_decr:
	TO | DOWNTO

variable_or_id:
	variable | ID


statement:
	|variable_or_id COLONEQ expression
	|ID expr_proc? 
	|BEGIN statement (SEMICOLON, statement)* END
	|IF expression THEN statement
	|IF expression THEN statement ELSE statement 
	|CASE expression OF single_case (SEMICOLON, single_case)* END
	|WHILE expression DO statement
	|REPEAT statement (SEMICOLON, statement)* UNTIL expression
	|FOR VARID COLONEQ expression incr_decr expression DO statement

(* BLOCK *)


(*Constante*)
init_const:
	VARID EQ constant SEMICOLON 

block_const:
	CONST
	init_const+


(*Type*)
init_type:
	VARID EQ type_automate SEMICOLON

block_type:
    TYPE
    init_type+


(*Var*)

init_var:
	VARID (COMA, VARID)* COLON type_automate SEMICOLON

block_var:
	VAR
	init_var+


(*Procedure et Function*)
mult_parameter:
	SEMICOLON under_parameter_list

under_parameter_list:
 VARID (COMA,VARID)* COLON type_identifier
| VARID (COMA,VARID)* COLON type_identifier mult_parameter

| FUNCTION ID (COMA,ID)* COLON type_identifier
| FUNCTION ID (COMA,ID)* COLON type_identifier mult_parameter

| VAR VARID (COMA,VARID)* COLON type_identifier
| VAR VARID (COMA,VARID)* COLON type_identifier mult_parameter

| PROCEDURE ID (COMA,ID)* 
| PROCEDURE ID (COMA,ID)* mult_parameter 

parameter_list:
	LPAR under_parameter_list RPAR

procedure:
	PROCEDURE ID parameter_list? SEMICOLON block SEMICOLON

(* fonction *)
function_bl:
	FUNCTION ID parameter_list? COLON type_identifier SEMICOLON block SEMICOLON

(* Main block *)

block:
	block_const?
	block_type?
	block_var?
	procedure*
	function_bl*
	BEGIN
		statement (SEMICOLON, statement)*
	END

(* pseudo main : Structure principale d'un programme PASCAL *)
program:
	PROGRAM ID SEMICOLON
		block
	DOT

%%
