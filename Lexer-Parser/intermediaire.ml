open AST
open Frame
open Compteur

let compteurinitlabel = Compteur.compteurinit "labels"

let compteurlabel = Compteur.compteur "labels"

(***************************Registers List Unification*********************)

let unify_funcs = fun funcs ->
let aux = fun l res ->
match l with
(_,_,(ts,_,vt,_))::t -> aux t (res @ ts @ vt)
in aux funcs []

let unify_procs = fun procs ->
let aux = fun l res ->
match l with
(_,(ts,_,vt,_))::t -> aux t (res @ ts @ vt)
in aux procs []

(*I take a block and make a unified symbol table*)
let unify = fun block_table ->
let funcs = block_table.my_funcs 
and procs = block_table.my_procs
in block_table.const, block_table.my_vars@(unify_funcs funcs)@(unify_procs procs)


(***************************Expression evaluation**************************)
let eval_sign = fun s ->
match s with
|Plus -> Int32.one
|Minus -> Int32.minus_one

let rec eval_factor = fun constants f ->
match f with
UConst (UNormal (UInteger i)) -> i
|UConst (UNormal (UString s)) ->let rec slimming = fun c res->
					begin match c with
					(Cinteger (a))::t -> slimming t (a::res)
					|_::t -> slimming t res
					| [] -> res
					end
				in let nconst = slimming constants []
				in List.assoc s nconst
|Expression e -> eval_exp e
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

and eval_list_term = function c l ->
let rec aux = fun l res ->
match l with
(s,t)::tail -> let nres = Int32.add (Int32.mul (eval_sign s) (eval_term c t))  res
		in aux tail nres
|[] -> res
in aux l Int32.zero

and eval_exp = function c mon_exp ->
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
let rm_scnd = fun a,b,c -> a,c in 
let ntab = rm_scnd table
in match v with 
v1,[] -> List.assoc v1 ntab
v1, [Brackety ([e])] -> let mem_reg = List.assoc v1 ntab 
			and index,_,_ = 
				let rm_thrd = fun a,b,c -> a,b 
				in let ntab2 = rm_thrd table
				in List.assoc v1 ntab2
			in Int32.add (Int32.sub (eval_exp c e) index) mem_reg

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
replace_factor f table, List.map (fun op,fact -> op, replace_factor fact table) l


and replace_simple_expression = fun se table ->
match se with
Signed s,t,l -> NSigned (s, replace_term t table, List.map (fun signe,terme -> signe,replace_term terme table) l)
|USigned t,l -> NUSigned (replace_term t table, List.map (fun signe,terme -> signe,replace_term terme table) l)


and replace_expression = fun e table->
match e with
ESimple se -> Simple_nexpression (replace_simple_expression se table)
|EOperation se1, op, se2 -> Noperation ((replace_simple_expression se1 table), op, (replace_simple_expression se2 table))



(****************************Statement Refactor*************************)

let replace_statement = fun stat table ->
match stat with
Affect v_or_id, expr -> begin match v_or_id with
			Id2 (id) -> RegWrite (replace_id id table), expr
			|Variable var -> RegWrite (replace_var var table), expr
			end
|Embedded l-> List.flatten (List.map replace_statement l)
|IfThen expr, s -> let label1 = Int64.of_int (compteurlabels 1)
		   and label2 = Int64.of_int (compteurlabels 1)
		   in (Jump (expr, label1, label2))::(Label label1)@(replace_statement s)@[Label label2]
|IfThenElse expr, s1, s2 -> let label1 = Int64.of_int (compteurlabels 1)
		   and label2 = Int64.of_int (compteurlabels 1)
		   in (Jump (expr, label1, label2))::(Label label1)@(replace_statement s1)@[Label label2]@(replace_statement s2)
|While expr, s -> (*glob this*)


























