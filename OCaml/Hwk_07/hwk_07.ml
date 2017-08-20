open Vector

module Int_arithmetic: (Comparable with type t = int) = struct 
  type t = int 
  let add = (+)
  let mult = ( * )
  let to_string = string_of_int
end 

module Complex_arithmetic: (Comparable with type t = float*float) = struct 
  type t = float*float 
  let add (x1,x2) (y1,y2) = ((x1 +. y1),(x2 +. y2))
  let mult (x1,x2) (y1,y2) = ((x1 *. y1 -. x2 *. y2),(x1 *. y2 +. x2 *. y1))
  let to_string (x1,x2) = ("(" ^ string_of_float x1 ^ "+" ^ string_of_float x2 ^ "i" ^ ")")  
end 


module Int_vector = Make_vector(Int_arithmetic)
module Complex_vector = Make_vector(Complex_arithmetic)



