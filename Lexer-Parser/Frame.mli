type frame = {
entry_lbl : int32;
return_lbl : int32;
args_reg : int32 list;
res_reg : int32;
size_stack : int32;	(*=0*)
} 

(*new expressions*)
type nvariable = int32

type nterm = nfactor * (operator_term * nfactor) list 

and simple_nexpression =
|NSigned of sign * nterm * (sign * nterm) list
|NUSigned of nterm * (sign * nterm) list


and nexpression = 
|Simple_nexpression of simple_nexpression
|Noperation of simple_nexpression * log_bin_op * simple_nexpression


and nfactor = 
|NConst of int32
|NString of string (*I don't get constants*)
|NBoolCont of bool
|NVar of nvariable
|NExpression of nexpression
|NNeg_factor of nfactor




type i_unop =
 IPlus
|IMinus
|INot

type i_binop =
 IPlus
|IMinus
|IDiv
|ITimes
|IMod
|ILt
|ILe
|IGt
|IGe
|IEq
|INeq

type i_expression =
 Cst_int of int32
|Cst_dec of float
|Reg of int32
|MemRead of i_expression
|UnaryOp of i_unop * i_expression
|BinaryOp of i_expression * i_binop * i_expression

type label = int64
(*expressions not replaced by i_expression for now*)
type i_instruction = 
 Label of label
|RegWrite of int32 * nexpression
|Woopwoop of nexpression * nexpression
|Jump of nexpression * label * label
|Goto of label
|ProcCall of frame * nexpression list
|FunCall of nexpression * frame * nexpression list

