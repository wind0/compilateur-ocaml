
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
|Recur of (identifier list ) * stuff
|Value of simple_type (*... I guess*)

type

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
