open AST

(***************************Expression evaluation**************************)
let eval_sign = fun s ->
match s with
|Plus -> Int32.one
|Minus -> Int32.minus_one

let rec eval_factor = fun f ->
match f with
UConst (UNormal (UInteger i)) -> i
|Expression e -> eval_exp e
|_ -> failwith("Invalid Array Declaration")

and eval_term = fun t ->
match t with
f, [] -> eval_factor f
|f, (op, f2)::t -> let operation = 
			begin match op with
			Times -> Int32.mul
			|Div -> Int32.div
			|Mod -> Int32.rem
			|Pow -> failwith("NOPE")
			end
			in operation (eval_factor f) (eval_term (f2,t))

and eval_list_term = function l ->
let rec aux = fun l res ->
match l with
(s,t)::tail -> let nres = Int32.add (Int32.mul (eval_sign s) (eval_term t))  res
		in aux tail nres
|[] -> res
in aux l Int32.zero

and eval_exp = function mon_exp ->
match mon_exp with
EOperation _ -> failwith("logical operation within an array index declaration")
|ESimple simple -> begin match simple with
			|Signed (s1,t1,l) -> let evaluated_t = eval_term t1
						in let evaluated_st = Int32.mul (eval_sign s1)  evaluated_t
					in Int32.add evaluated_st  (eval_list_term l)		
			|USigned (t1,l) -> let evaluated_t = eval_term t1
					in Int32.add evaluated_t  (eval_list_term l)
		    end
