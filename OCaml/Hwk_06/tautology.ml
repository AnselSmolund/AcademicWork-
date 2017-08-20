type formula = And of formula * formula
	     | Or  of formula * formula
	     | Not of formula 
	     | Prop of string
	     | True
	     | False



exception KeepLooking 

type subst = (string * bool) list

let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst = show_list show_string_bool_pair


let is_elem v l =
  List.fold_right (fun x in_rest -> if x = v then true else in_rest) l false

let rec explode = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let dedup lst =
  let f elem to_keep =
    if is_elem elem to_keep then to_keep else elem::to_keep
  in List.fold_right f lst []



let rec lookup (n:string) env =
  match env with
  | [] -> raise (Failure ("Identifier " ^ n ^ " is not in scope."))
  | (x,value)::rest when x = n -> value
  | _::rest -> lookup n rest


let rec eval (formula:formula) (env:subst) = 
  match formula with
  |True-> true 
  |False -> false 
  |And (f1, f2) -> 
     ( match eval f1 env, eval f2 env with
       | true, true -> true 
       | _ -> false
     )   
  |Or (f1, f2) -> 
     ( match eval f1 env, eval f2 env with
       | true, true -> true
       | true, false -> true
       | false, true -> true 
       | _ -> false
     )  
  |Not(f1) -> (
  		match eval f1 env with
  		|true -> false
  		|false -> true
  	)        
  |Prop x -> lookup x env   

let rec freevars_helper (formula:formula) : string list = 
  match formula with
  |True -> []
  |False -> []
  |Prop i -> [i]
  |Or(f1,f2) -> freevars_helper f1 @ freevars_helper f2
  |And(f1,f2) -> freevars_helper f1 @ freevars_helper f2
  |Not(f1) -> freevars_helper f1





let tList = [true;true]
let tfList = [true;false]
let ftList = [false;true]
let fList = [false;false]


let switch lst = 
	match lst with 
	|[true;true] -> [true;true]
	|[true;false] -> [true;true]
	|[false;true] -> [false;false]
	|[false;false] -> [false;false]

let try_sub = [fList;tfList;ftList;tList]


let double lst = 
	match lst with
	|[]->[]
	|x::xs -> [x]@[x]

let mix lst lst2 = if List.length lst2 > 1 then List.combine lst2 lst else List.combine (double lst2) (switch lst)

let freevars (formula:formula) = dedup (freevars_helper formula)

let is_tautology (f:formula) show = 
	let rec try_taut props truths = 
	match truths with 
	|[] -> raise KeepLooking
	|x::xs -> if eval f (mix x props) = false then (try show(mix x props) with 
													 |KeepLooking -> try_taut props xs
													 ) else try_taut props xs
	in try try_taut (freevars f) try_sub with 
	|KeepLooking -> None 

let is_tautology_first f = is_tautology f (fun s -> Some s)

let is_tautology_print_all f =
  is_tautology 
    f
    (fun s -> print_endline (show_subst s); 
	      raise KeepLooking)
