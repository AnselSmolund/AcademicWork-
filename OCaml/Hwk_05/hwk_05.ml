let rec ands (lst:bool list):bool = 
	match lst with
	|[] -> true
	|x::xs when x = false -> false 
	|_::xs -> ands xs 
	
