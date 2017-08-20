type expr 
  = Const of int
  | Add of  expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr

let rec show_expr (e:expr) : string = 
	match e with 
	|Const x -> Pervasives.string_of_int x
	|Add (x, y) -> "(" ^ show_expr x ^ "+" ^ show_expr y ^ ")"
	|Mul(x,y) ->"(" ^ show_expr x ^ "*" ^ show_expr y ^ ")"
	|Sub(x,y) -> "(" ^ show_expr x ^ "-" ^ show_expr y ^ ")"
	|Div(x,y) -> "(" ^ show_expr x ^ "/" ^ show_expr y ^ ")"

let isConst x = 
	match x with
	|Const x -> true 
	|_ -> false

let rec first_op e = 
	match e with
	|Const x -> raise(Failure "not an operation")
	|Add(_,_) -> 2
	|Mul(_,_) -> 1 
	|Sub(_,_) -> 2
	|Div(_,_) -> 1


let show_pretty_expr (e:expr) : string = 
	let rec helper ee h = 
		match ee with 
		|Const x -> Pervasives.string_of_int x
		|Add (x,y) when isConst x && isConst y = false -> if h = 2 then helper x 2 ^ "+" ^ helper y 3  else "(" ^ helper x 2 ^ "+" ^ helper y 2 ^ ")"
		|Add (x,y) when isConst x = false && isConst y -> if h = 2 then  helper x 2 ^ "+" ^ helper y 2  else "(" ^ helper x 2 ^ "+" ^ helper y 2 ^ ")"
		|Add(x,y) when isConst x && isConst y -> if h = 3 then "(" ^ helper x 2 ^ "+" ^ helper y 2 ^ ")" else helper x 2 ^ "+" ^ helper y 2 
		|Mul(x,y) -> if h = 2 || isConst x then helper x 3 ^ "*" ^ helper y 3  else "(" ^ helper x  1 ^ "*" ^ helper y  1^ ")"
		|Sub(x,y) -> (match x,y with
					|Sub(x,y),Sub(z,t) -> helper x 3 ^ "-" ^ helper y 3^ "-" ^  "(" ^ helper z 3 ^ "-" ^ helper t 3 ^ ")"
					|_ , _-> helper x 3 ^ "-" ^  helper y 3	
					)
		|Div(x,y) -> if h = 2 || isConst x then helper x 3 ^ "*" ^ helper y 3  else "(" ^ helper x  1 ^ "*" ^ helper y  1^ ")"	
		|Add(x,y) -> helper x 2 ^ "+" ^ helper y 2
	in helper e (first_op e)
