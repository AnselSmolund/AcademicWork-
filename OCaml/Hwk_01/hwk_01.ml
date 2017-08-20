let even x = 
	if x mod 2 = 0 then true 
	else false;;


let rec euclid x y = 
	if x = y then x 
    else if x > y then euclid (x-y) y
    else euclid x (y-x) ;; 

 let frac_add(x1,y1)(x2,y2) = ((y1+y2),(y2*y1)) ;; 

 let frac_simplify(x,y) = 
 	if euclid x y = 1 then (x,y) else 
 	((x/euclid x y),(y/euclid x y)) ;;

let square_approx upper accuracy = 
	let lower = 1.0 and n = upper in
	let rec helper upper lower accuracy n = 
		if (upper -. lower) > accuracy then 
	    	let guess = (lower +. upper) /. 2.0  in 
	    	if (guess *. guess) > n then helper guess lower accuracy n
	    	else helper upper guess accuracy n 
		else  (lower , upper)
	in helper upper lower accuracy n ;;

let rec max_list xs = 
 	match xs with 
 	|[] -> 0
 	|x::[] -> x
 	|h::tail when h < 0 ->  let b = max_list (tail) in if(h<b) then b else h 
 	|h::tail when h > 0 ->  max h (max_list tail) ;; 

let rec drop drop_val xs = 
		if drop_val = 0 then xs else 
		match xs with 
			|[] -> []  
			|h::tail when drop_val > 1 -> drop (drop_val-1) tail 
			|h::tail -> tail ;; 

let rec rev xs = 
	match xs with 
		|[]->[] 
		|h::t -> (rev t) @ [h];;
	
let perimeter xs = 
	let distance (x,y)(x',y') = 
	sqrt(((x'-.x)*.(x'-.x))+.((y'-.y)*.(y'-.y))) and l = xs 
	in 
	let header head =
	match head with 
		|h::rest -> h 
	in
	let rec helper l =
	match l with 
		|h::(h2::[])-> distance h h2 +. distance h2 (header xs)
		|h::(h2::rest) -> distance h h2 +. helper (h2::rest)
	in helper l ;; 

let rec is_matrix xs' = 
	let length xs = 
		let rec helper n = function
			|[]-> n
			|_::tail -> helper(n+1)tail 
		in helper 0 xs
	in 
	match xs' with 
	|h::[] -> true 
	|h::h2::[] -> if length h = length h2 then true else false 
	|h::h2::tail -> if length h = length h2 then is_matrix tail else false  ;; 

let rec matrix_scalar_add mx to_add = 
	let rec matrix_helper mx to_add = 
	match mx with 
	|head::[] ->[head+to_add]
	|head::tail -> (head+to_add)::matrix_helper tail to_add
	in 
	match mx with 
		|[] -> [] 
		|h::[] -> [matrix_helper h to_add]
		|h::t -> [(matrix_helper h to_add)]@(matrix_scalar_add t to_add) ;; 




