module type Comparable = sig 
  type t 
  type m
  val add: t -> t -> t 
  val mult: t -> t -> t 
  val to_string: t -> string 
end 



module type Arithmetic_intf = sig 
	type t 
	type endpoint
  type endpoint_m
	val create: int -> endpoint -> t
	val from_list: endpoint list -> t 
	val to_list: t -> endpoint list 
	val scalar_add: endpoint -> t -> t
	val scalar_mul: endpoint -> t -> t 
	val scalar_prod: t -> t -> endpoint option 
	val to_string: t -> string
	val size: t->int
end 

module Make_vector(Endpoint : Comparable):(Arithmetic_intf) = struct 

  type endpoint_m = Endpoint.m
  type endpoint = Endpoint.t 
  type t = |Vector of Endpoint.t list
        

  let create n v : t = 
    let rec helper n' acc = if n' = 0 then Vector(acc) 
                else helper(n'-1)(v::acc) in helper n []

  let from_list lst = Vector(lst)

  let to_list vec = 
    match vec with
    |Vector(lst) -> lst 

  let scalar_add value vec = 
    match vec with 
    |Vector(lst) -> Vector(List.map(fun x -> Endpoint.add x value)lst)


  let scalar_mul value vec = 
  	match vec with 
  	|Vector(lst) -> Vector(List.map(fun x -> Endpoint.mult x value)lst)

  let scalar_prod vec1 vec2 = 
  	match vec1,vec2 with
  	|Vector([]),Vector([]) -> None 
  	|Vector(lst1),Vector(lst2) -> if List.length lst1 <> List.length lst2 then None else 
  								 let rec helper l1 l2  =
  								  match l1,l2 with
  								  |[],[] -> raise (Failure "impossible")
  								  |[x],[y] -> (Endpoint.mult x y)
  								  |x::xs,y::ys -> Endpoint.add (Endpoint.mult x y) (helper xs ys)  
  								  in Some (helper lst1 lst2)
  let to_string vec = 
  	match vec with 
  	|Vector(lst) -> let start = "<< " ^ (string_of_int (List.length lst)) ^ " | " in 
  					let last = 
  						(let rec helper lst' = 
  							(match lst' with 
  							|[] -> " >>"
  							|[x] -> Endpoint.to_string x ^ helper []
  							|x::xs -> Endpoint.to_string x ^ ", " ^ helper xs) 
  							in helper lst)
  						in start^last

  let size vec =
  	match vec with 
  	|Vector(lst) -> List.length lst 





end
