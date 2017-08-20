module type Comparable = sig 
  type t 
  val add: t -> t -> t 
  val compare: t -> t -> int 
  val to_string: t -> string 
end 


module Make_vector(Endpoint : Comparable) = struct 

 
  type t = |Vector of Endpoint.t list
           |Empty

  let create n v : t = 
    let rec helper n' acc = if n' = 0 then Vector(acc) 
                else helper(n'-1)(v::acc) in helper n []

  let from_list lst = Vector(lst)

  let to_list vec = 
    match vec with
    |Vector(lst) -> lst 

  let scalar_add value vec = 
    match vec with 
    |Vector(lst) -> let rec helper lst' acc= 
                    match lst with 
                    |[] -> Vector(acc)
                    |x::xs -> helper xs ((Endpoint.add x value)::acc) 
                  in helper lst [] 

end
