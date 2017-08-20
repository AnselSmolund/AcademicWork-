exception KeepLooking 

let rec is_not_elem set v =
  match set with
  | [] -> true
  | s::ss -> if s = v then false else is_not_elem ss v

type pos = int*int

let final pos = pos = (3,5) || pos = (5,1)

let maze_moves (pos:(int * int)):(int*int) list = 
	match pos with 
	|(1,1) -> [(2,1)]
	|(2,1) -> [(1,1);(3,1)]
	|(3,1) -> [(2,1);(3,2)]
	|(4,1) -> [(4,2)]
	|(5,1) -> [(5,2)]
	|(1,2) -> [(1,3);(2,2)]
	|(2,2) -> [(1,2);(3,2)]
	|(3,2) -> [(2,2);(3,3);(4,2);(3,1)]
	|(4,2) -> [(4,1);(3,2)]
	|(5,2) -> [(5,1);(5,3)]
	|(1,3) -> [(1,2);(2,3);(1,4)]
	|(2,3) -> [(1,3)]
	|(3,3) -> [(3,2);(4,3);(3,4)]
	|(4,3) -> [(3,3);(5,3)]
	|(5,3) -> [(5,2);(4,3);(5,4)]
	|(1,4) -> [(1,3);(1,5)]
	|(2,4) -> [(2,5);(3,4)]
	|(3,4) -> [(2,4);(3,3);(4,4)]
	|(4,4) -> [(3,4);(4,5)]
	|(5,4) -> [(5,3)]
	|(1,5) -> [(1,4);(2,5)]
	|(2,5) -> [(2,4);(1,5)]
	|(3,5) -> [(4,5)]
	|(4,5) -> [(3,5);(4,4);(5,5)]
	|(5,5) -> [(4,5)]


let maze () = 
	let rec go_from state path = 
	if final state then Some path
else 
	match List.filter(is_not_elem path)(maze_moves state) with 
	|[] -> raise KeepLooking
	|[a] -> (go_from a (path @ [a])) 
	|[a;b] -> 
		(try go_from a (path @ [a]) with
			|KeepLooking -> go_from b (path@[b])
		)

	|_ -> raise KeepLooking

	in try go_from (2,3) [(2,3)] with 
	|KeepLooking -> None 


