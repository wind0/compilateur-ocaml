type frame = {
entry_lbl : int32;
return_lbl : int32;
args_reg : int32 list;
res_reg : int32;
size_stack : int32;
} 

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

type i_instruction = 
 Label of label
|RegWrite of int32 * i_expression
|Woopwoop of i_expression * i_expression
|Jump of i_expression * label * label
|Goto of label
|ProcCall of frame * i_expression list
|FunCall of i_expression * frame * i_expression list
