(* Identifier *) 
type identifier  = string


(* Basic Types *)

type typ = 
TypInteger
|TypBoolean


(* Constants *)

type unsigned_constant =
|UInteger of int32
|UString of string
type unsigned_constant_nil =
|UNormal of unsigned_constant
|Nil

type sign = 
|Plus
|Minus
and idornumber = 
|BIdentified of identifier
|BInteger of int32
and constant = 
|SignedIdOrNumber of (sign * idornumber)
|IdOrNumber of idornumber
|CString of string


(* Types returns *)

type simple_type =
|Type_identifier of typ
|ID_list of identifier list
|Enum of constant * constant

type field_list = 
|Recur of (identifier list ) * type_automate
|RecurPlus of (identifier list) * type_automate * field_list
|FCase of identifier * typ * line_case_field_list list
and line_case_field_list =
constant list * field_list
and type_automate = 
|Simple of simple_type
|Array of array_type_automate
|Record of field_list
and array_type_automate =
simple_type list * type_automate


(* Operators *)
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


(* Variables *)
type boucle_intern_variable =
|Brackety of expression list
|Dotty of identifier

and variable = identifier * boucle_intern_variable list


(* Expressions *)
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


(* Statement *)
type expr_or_procid =
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
|ProcInit of identifier * expr_or_procid list
|Embedded of statement list
|IfThen of expression * statement
|IfThenElse of expression * statement * statement
|Case of expression * single_case list
|While of expression * statement
|Repeat of statement list * expression
|For of identifier * expression * incr_or_decr * expression * statement


(* Block *)

type init_const = identifier * constant

type block_const = init_const list

type init_type = identifier * type_automate

type block_type = init_type list

type init_var = identifier list * type_automate

type block_var = init_var list

type parameter_list = parameter

and parameter = 
|NoneParameter
|ClassicParameter of identifier list * typ * parameter
|FunctionParameter of identifier list * typ * parameter
|VariableParameter of identifier list * typ * parameter
|ProcedureParameter of identifier list * parameter

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
statements : statement list;
}

type program = {
prog_name : identifier;
prog_body : block;
}



