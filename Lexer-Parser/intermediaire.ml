open AST
open Frame
open Compteur
open Typecheck
module Intermediaire = 
struct

let compteurinitlabel = Compteur.compteurinit "labels"

let compteurlabel = Compteur.compteur "labels"

(***************************Registers List Unification*********************)

let unify_funcs = fun funcs ->
let rec aux = fun l res ->
match l with
(_,_,(ts,_,vt,_),_)::t -> aux t (res @ ts @ vt)
|[] -> res
in aux funcs []

let unify_procs = fun procs ->
let rec aux = fun l res ->
match l with
(_,(ts,_,vt,_))::t -> aux t (res @ ts @ vt)
|[] -> res
in aux procs []

(*I take a block and make a unified symbol table*)
let unify = fun (bt:Typecheck.block_table) ->
let funcs = bt.my_funcs 
and procs = bt.my_procs
in bt.my_consts, bt.my_vars@(unify_funcs funcs)@(unify_procs procs)


(***************************Expression evaluation**************************)
let eval_sign = fun s ->
match s with
|Plus -> Int32.one
|Minus -> Int32.minus_one

let rec eval_factor = fun constants f ->
match f with
UConst (UNormal (UInteger i)) -> i
|UConst (UNormal (UString s)) ->let rec slimming = fun (c:Typecheck.constant_table) res->
					begin match c with
					(Cinteger ((s,_,_),a))::t -> slimming t ((s,a)::res)
					|_::t -> slimming t res
					| [] -> res
					end
				in let nconst = slimming constants []
				in List.assoc s nconst
|Expression e -> eval_exp constants e
|_ -> failwith("Invalid Array Declaration")

and eval_term = fun c t ->
match t with
f, [] -> eval_factor c f
|f, (op, f2)::t -> let operation = 
			begin match op with
			Times -> Int32.mul
			|Div -> Int32.div
			|Mod -> Int32.rem
			|Pow -> failwith("NOPE")
			end
			in operation (eval_factor c f) (eval_term c (f2,t))

and eval_list_term = fun c l ->
let rec aux = fun l res ->
match l with
(s,t)::tail -> let nres = Int32.add (Int32.mul (eval_sign s) (eval_term c t))  res
		in aux tail nres
|[] -> res
in aux l Int32.zero

and eval_exp = fun c mon_exp ->
match mon_exp with
EOperation _ -> failwith("logical operation within an array index declaration")
|ESimple simple -> begin match simple with
			|Signed (s1,t1,l) -> let evaluated_t = eval_term c t1
						in let evaluated_st = Int32.mul (eval_sign s1)  evaluated_t
					in Int32.add evaluated_st  (eval_list_term c l)		
			|USigned (t1,l) -> let evaluated_t = eval_term c t1
					in Int32.add evaluated_t  (eval_list_term c l)
		    end



(*****************************Variable Replacement***********************)

let replace_var = fun v (c,table) ->
let rm_scnd = (fun (a,b,c) -> a,c) in 
let ntab = List.map rm_scnd table
in match v with 
v1,[] -> List.assoc v1 ntab
|v1, [Brackety ([e])] -> let mem_reg = List.assoc v1 ntab 
			and index,_,_ = 
				let rm_thrd = (fun (a,b,c) -> a,b) 
				in let ntab2 = List.map rm_thrd table
				in List.assoc v1 ntab2
			in Int32.add (Int32.sub (eval_exp c e) index) mem_reg
|_ -> failwith("help yourself")
let replace_id = fun i table ->
replace_var (i,[]) table


let rec replace_factor = fun f (c,t)  ->
match f with
Var v -> NVar (replace_var v (c,t))
|Expression expr -> NExpression (replace_expression expr (c,t))
|Neg_factor f1 -> NNeg_factor (replace_factor f1 (c,t))
|UConst(UNormal truc) -> begin match truc with 
			UInteger i -> NConst i
			|UString s -> NString s

			end
|_ -> failwith("I can't let you do that Dave")

and replace_term = fun (f,l) table ->
replace_factor f table, List.map (fun (op,fact) -> op, replace_factor fact table) l


and replace_simple_expression = fun se table ->
match se with
Signed (s,t,l) -> NSigned (s, replace_term t table, List.map (fun (signe,terme) -> signe,replace_term terme table) l)
|USigned (t,l) -> NUSigned (replace_term t table, List.map (fun (signe,terme) -> signe,replace_term terme table) l)


and replace_expression = fun e table->
match e with
ESimple se -> Simple_nexpression (replace_simple_expression se table)
|EOperation (se1, op, se2) -> Noperation ((replace_simple_expression se1 table), op, (replace_simple_expression se2 table))

let replace_constant = fun c (constants, table) ->
match c with
|SignedIdOrNumber (s, i_o_n) -> let signe = begin match s with
					Plus -> Int32.one
					|Minus -> Int32.minus_one
					end
				and tmp = begin match i_o_n with
					BInteger i -> i
					|BIdentified id -> 
						let rec slimming = fun (c:Typecheck.constant_table) res->
						begin match c with
						(Cinteger ((s,_,_),a))::t -> slimming t ((s,a)::res)
						|_::t -> slimming t res
						| [] -> res
						end
						in let nconsts = slimming constants []
						in List.assoc id nconsts
					end
				in Int32.mul signe tmp
|IdOrNumber i_o_n -> begin match i_o_n with
				BInteger i -> i
				|BIdentified id -> 
					let rec slimming = fun (c:Typecheck.constant_table) res->
					begin match c with
					(Cinteger ((s,_,_),a))::t -> slimming t ((s,a)::res)
					|_::t -> slimming t res
					| [] -> res
					end
					in let nconsts = slimming constants []
					in List.assoc id nconsts
				end
|CString _ -> failwith("Case Nightmare Green")

(****************************Statement Refactor*************************)

let rec replace_statement = fun stat (constants,table) ->
match stat with
Affect (v_or_id, expr) -> begin match v_or_id with
			Id2 (id) ->[ RegWrite ((replace_id id table), (replace_expression expr table))]
			|Variable var ->[ RegWrite ((replace_var var table), replace_expression expr table)]
			end
|Embedded l->  List.flatten (List.map (fun a -> replace_statement a (constants,table)) l)
|IfThen (expr, s) -> let label1 = Int64.of_int (compteurlabel 1)
		   and label2 = Int64.of_int (compteurlabel 1)
		   in (Jump (replace_expression expr table, label1, label2))::[Label label1]@(replace_statement s( constants, table))@[Label label2]
|IfThenElse (expr, s1, s2) -> let label1 = Int64.of_int (compteurlabel 1)
		   and label2 = Int64.of_int (compteurlabel 1)
		   in (Jump (replace_expression expr table, label1, label2))::[Label label1]@(replace_statement s1(constants,table))@[Label label2]@(replace_statement s2 (constants,table))
|While (expr, s) -> 	let entry = Int64.of_int (compteurlabel 1)
			and fin = Int64.of_int (compteurlabel 1)
			and nexpr = replace_expression expr table
			in (Jump (nexpr,fin,entry))::(Label entry)::(replace_statement s (constants,table))@[Jump (nexpr,fin,entry)]@[Label fin]
|Case (expr, l) ->
		let fin = Int64.of_int (compteurlabel 1)
		in 
			let rec aux1 = fun liste1 res1 ->
			begin match liste1 with
			(cl, s)::t1 -> let label = Int64.of_int (compteurlabel 1)
				in let rec aux2 = fun liste2 res2 ->
				begin	match liste2 with 
				h::t2 -> let tmp = replace_constant h (constants, table)
					and label1 = Int64.of_int (compteurlabel 1)
					in let expr1 =	Noperation(NUSigned ((NExpression (replace_expression expr table),[]),[]), Eq, NUSigned ((NConst tmp,[]),[]))
					in aux2 t2 res2@[Label label1]@[Jump (expr1,label,(Int64.add label1 Int64.one))]
				| [] -> res2
				end
				in aux1 t1 res1@(aux2 cl [])@[Label label]@(replace_statement s (constants,table))@[Goto fin] 
			|[] -> res1
			end
			in (aux1 l [])@[Label fin]
|Repeat (sl, expr) -> let label = Int64.of_int(compteurlabel 1)
			and fin = Int64.of_int(compteurlabel 1)
			in (Label label)::(List.flatten (List.map (fun a -> replace_statement a (constants,table)) sl))@[Jump ((replace_expression expr table),fin,label)]@[Label fin]
| _ -> failwith ("sorry, not yet")

(***********************************WHAT AM I DOING*************************)

let transform = fun (regblock:Typecheck.block_table) (initblock:AST.block) ->
let _ = compteurinitlabel 0
and init_funcs = initblock.functions
and init_procs = initblock.procedures
and reg_funcs = regblock.my_funcs
and reg_procs = regblock.my_procs
in let list_funcs = List.map2 (fun a b -> a,b) reg_funcs init_funcs
and list_procs = List.map2 (fun a b -> a,b) reg_procs init_procs
in let funcs = List.map (fun ((_,_,(params,_,_,_),ret1),b) -> let entry = Int64.of_int (compteurlabel 1)
					and ret = Int64.of_int (compteurlabel 1)
					and args_r = List.map (fun (_,_,r) -> r) params in b.func_name, {entry_lbl = entry;
	return_lbl = ret;
	args_reg = args_r;
	res_reg = ret1;
	size_stack = Int32.zero;},b.func_body.statements) list_funcs
and procs  =  List.map (fun ((_,(params,_,_,_)),b) -> let entry = Int64.of_int (compteurlabel 1)
					and ret = Int64.of_int (compteurlabel 1)
					and args_r = List.map (fun (_,_,r) -> r) params in b.proc_name, {entry_lbl = entry;
	return_lbl = ret;
	args_reg = args_r;
	res_reg = Int32.minus_one;
	size_stack = Int32.zero;},b.proc_body.statements) list_procs
in funcs@procs

end


















