
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

(* Track 6: Variables*)
type boucle_intern_variable =
|Brackety of expression list
|Dotty of identifier

type variable = identifier * boucle_intern_variable list



(*Bonus Track: operators*)
type sign =
  | Add
  | Sub

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


(* Track 7: Expressions (This is a goodie)*)
type factor =
|UConst of unsigned_constant
|Var of variable
|Function of funct * expression list
|Expression of expression
|Neg_factor of factor
|Brackety of expression list


type term = factor * (operator_term * factor) list

type simple_expression =
|Signed of sign * term * (sign * term) list
|USigned of term * (sign * term) list

type expression =
|Simple of simple_expression
|Operation of simple_expression * log_bin_op * simple_expression

(* Track 8: BRB, vomiting blood *)
type expr_or_procid =(* this name...*)
|Id of identifier
|Expr of expression

type variable_or_id = 
|Variable of variable
|Id of identifier

type statement =
|Affect of variable_or_id * expression
|

type real_statement =
|Labelled of int32 * statement
|NotLabelled of statement
