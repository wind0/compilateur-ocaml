

(* 	One type to rule them all, One type to define them, 
	One type to bring them all and in the darkness bind them *)
type identifier  = string

type procedure_identifier = identifier

type function_identifier = identifier

type variable_identifier = identifier

type type_identifier = identifier

type parameter_list_identifier = identifier

type constant_identifier = identifier

type constant_block_identifier = identifier

type simple_type_identifier = identifier


(* le type de base *)

type typ = 
TypInteger
|TypBoolean
|TypArray of typ

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt                                       
  | Le
  | Gt
  | Ge
  | Eq
  | Ne


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


