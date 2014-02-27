(* Track 1: Identifier(s?) *) 
(* 	One type to rule them all, One type to define them, 
	One type to bring them all and in the darkness bind them *)
type identifier  = string


(* Track 2: Basic Types *)

type typ = 
TypInteger
|TypBoolean


(* Track 3: Constants and shit*)
type unsigned_constant =
|UInteger of int32
|UString of string
type unsigned_constant_nil =
|UNormal of unsigned_constant
|Nil
(*| String of string *)

type sign = 
|Plus
|Minus
and burne = 
|BIdentified of identifier
|BInteger of int32
and constant = 
(*String of string*)
|SignedBurne of (sign * burne)
|Burne of burne
|CString of string

(* Track 4: Types returns! *)
type simple_type =
|Type_identifier of typ
|ID_list of identifier list (* I guess *)
|Enum of constant * constant

(* Track 5: I don't really understand...and it shows*)
(* please get better! *)
type field_list = 
|Recur of (identifier list ) * type_automate
|RecurPlus of (identifier list) * type_automate * field_list
|FCase of identifier * typ * line_case_field_list list
and line_case_field_list = (*F*** this name*)
constant list * field_list
and type_automate = (*F*** this name too*)
|Simple of simple_type
|Array of array_type_automate
|Record of field_list
and array_type_automate =
simple_type list * type_automate

(*Bonus Track: operators*)
type log_bin_op =
  | Lt                                       
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  | In

type operator_term =
|Times
|Div
|Mod
|Pow

(* Track 6: Variables*)
type boucle_intern_variable =
|Brackety of expression list
|Dotty of identifier

and variable = identifier * boucle_intern_variable list




(* Track 7: Expressions (This is a goodie)*)
and factor =
|UConst of unsigned_constant_nil
|Var of variable
|Function of identifier * expression list
|Expression of expression
|Neg_factor of factor
|Brackets of expression list


and term = factor * (operator_term * factor) list

and simple_expression =
|Signed of sign * term * (sign * term) list
|USigned of term * (sign * term) list

and expression =
|ESimple of simple_expression
|EOperation of simple_expression * log_bin_op * simple_expression

(* Track 8: BRB, vomiting blood *)
type expr_or_procid =(* this name...*)
|Id of identifier
|Expr of expression

type variable_or_id = 
|Variable of variable
|Id2 of identifier

type incr_or_decr = 
|To
|Downto

type single_case = constant list * statement

and statement =
|Affect of variable_or_id * expression
|Wut of identifier * expr_or_procid list (*remplacer Wut par ce que c'est en vrai*)
|IfThen of expression * statement
|IFThenElse of expression * statement * statement
|Case of expression * single_case list
|While of expression * statement
|Repeat of statement list * expression
|For of identifier * expression * incr_or_decr * expression * statement


(* Track 9: How is this not over yet?*)

type init_const = identifier * constant

type block_const = init_const list

type init_type = identifier * type_automate

type block_type = init_type list

type init_var = identifier list * type_automate

type block_var = init_var list

type parameter_list = parameter list (*Well shit*)

and parameter = 
|ClassicParameter of identifier list * typ * parameter list
|FunctionParameter of identifier list * typ * parameter list
|VariableParameter of identifier list * typ * parameter list (*Hummmmm*)
|ProcedureParameter of identifier list * parameter list

(* Track 10: Supa Finishu!*)
(*A voir si ya besoin de les remplacer par des record*)
type procedure = {
proc_name : identifier;
proc_parameters : parameter_list;
proc_body: block;
}

and function_bl = {
func_name : identifier;
func_parameters : parameter_list;
func_return_type : typ;
func_body : block;
}

and block = {
constants : block_const;
types : block_type;
variables : block_var;
procedures : procedure list;
functions : function_bl list;
}

type program = {
prog_name : identifier;
prog_body : block;
}



