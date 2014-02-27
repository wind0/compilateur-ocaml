
(* Track 1: Identifier(s?) *) 
(* 	One type to rule them all, One type to define them, 
	One type to bring them all and in the darkness bind them *)
type identifier  = string


(* Track 2: Basic Types *)

type typ = 
TypInteger
|TypBoolean
|TypArray of typ


(* Track 3: Constants and shit*)
type unsigned_constant =
|Identified of identifier
|Integer of int32
type unsigned_constant_nil =
|Normal of unsigned_constant
|Nil
(*| String of string *)

type sign = 
|PLUS
|MINUS
and burne = 
|Identified of identifier
|Integer of int32
and constant = 
(*String of string*)
|SignedBurne of (sign * burne)
|Burne of burne

(* Track 4: Types returns! *)
type simple_type =
|Type_identifier of typ
|ID_list of identifier list (* I guess *)
|Enum of constant * constant

(* Track 5: I don't really understand...*)

type field_list = 
|Recur of (identifier list ) * field_list
|Value of simple_type (*... I guess*)

(*Bonus Track: operators*)
type log_bin_op =
  | Lt                                       
  | Le
  | Gt
  | Ge
  | Eq
  | Neq

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
|Simple of simple_expression
|Operation of simple_expression * log_bin_op * simple_expression

(* Track 8: BRB, vomiting blood *)
type expr_or_procid =(* this name...*)
|Id of identifier
|Expr of expression

type variable_or_id = 
|Variable of variable
|Id of identifier

type incr_or_decr = 
|To
|Downto

type single_case = constant list * real_statement

and statement =
|Affect of variable_or_id * expression
|Wut of identifier * expr_or_procid list (*remplacer Wut par ce que c'est en vrai*)
|IfThen of expression * real_statement
|IFThenElse of expression * real_statement * real_statement
|Case of expression * single_case list
|While of expression * real_statement
|Repeat of real_statement list * expression
|For of identifier * expression * incr_or_decr * expression * real_statement

and real_statement =
|Labelled of int32 * statement
|NotLabelled of statement
