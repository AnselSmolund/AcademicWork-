(* This file contains a few helper functions and type declarations
   that are to be used in Homework 2. *)

(* Place part 1 functions 'take', 'drop', 'length', 'rev',
   'is_elem_by', 'is_elem', 'dedup', and 'split_by' here. *)


let length lst = List.fold_left (fun n x -> n + 1) 0 lst 

let rec take n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then x::take (n-1) xs else []

let rec drop n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then drop (n-1) xs else l

let rev lst= List.fold_left(fun new_list x -> x::new_list) [] lst

(* val is_elem_by : ('a -> 'b -> bool) -> 'b -> 'a list -> bool = <fun> *)
let is_elem_by f key lst = List.fold_left(fun n x -> f x key || n) false lst 

(*val is_elem : 'a -> 'a list -> bool = <fun>*)
let is_elem x lst = is_elem_by (=) x lst 

(*val dedup : 'a list -> 'a list = <fun>*)
let dedup lst = List.fold_right(fun x xs -> if List.mem x xs then xs else x::xs) lst []


let split_by f lst splt = 
  let add acc rest = acc::rest
  in 
  let(rest,acc) = List.fold_right(fun x (rest,acc) -> if is_elem_by f x splt then (add acc rest,[])
   else (rest,x::acc)) lst ([],[])
in add acc rest



(* Some functions for reading files. *)
let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try 
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try 
    let channel = open_in filename
    in 
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None




type result = OK 
      | FileNotFound of string
      | IncorrectNumLines of int 
      | IncorrectLines of (int * int) list
      | IncorrectLastStanza


type word = char list 

type line = word list 

let break_up_list lst = split_by (=) lst ['\n']

let break_up_line lst =  List.map (fun x-> split_by (=) x [' ';',';'-';':';';';'!';'?';'.';'\''] ) lst 

let filter_helper lst = List.filter(fun a -> (<>) a []) lst 
let convert_to_non_blank_lines_of_words lst = filter_helper (List.map filter_helper ((break_up_line(break_up_list lst))))
(* helper functions for my main paradelle function*)

(* turns all characters to uppercase for comparing purposes*)
let to_up lst = List.map (fun x -> Char.uppercase x)lst

(* turns all characters in a list into their ascii equivalent for sorting purposes*)
let char_to_int lst = List.map (fun x -> Char.code x) lst

(* brings all my helper functions together to compare lines of stanzas and the final stanza to the other lines*)
let compare_helper lst lst2 = List.sort compare (char_to_int(to_up(List.concat(List.concat lst)))) = List.sort compare (char_to_int(to_up(List.concat(List.concat lst2))))

let compare_helper2 lst lst2 = List.sort compare (to_up(List.concat(List.concat(lst)))) = List.sort compare (to_up(List.concat(List.concat(lst2))))

let compare_helper3 lst lst2 = List.sort compare (to_up(List.concat(dedup(List.concat(lst))))) = List.sort compare (to_up(List.concat(dedup(List.concat(lst2)))))

let paradelle filename = 
  let char_option_list = read_file(filename) in 
  match char_option_list with
  |None-> FileNotFound filename
  |Some lst -> let line_list = convert_to_non_blank_lines_of_words lst in
  let list_length = length line_list in if list_length <> 24 then IncorrectNumLines list_length else 
 
  let first_stanz = take 6 line_list in
  let second_stanz = take 6 (drop 6 line_list) in
  let third_stanz = take 6 (drop 12 line_list) in
  let fourth_stanz = take 6 (drop 18 line_list) in 
  
  let truth_list = [
  (*first line and second line first stanza *)   
    compare_helper (take 1 first_stanz) (take 1 (drop 1 first_stanz));
  (*third and fourth line first stanza *)        
    compare_helper (take 1 (drop 2 first_stanz))  (take 1 (drop 3 first_stanz));
  (* if either of previous fail, then just set this to true, else checks if fifth and sixth lines use all the words from
  first four lines*)   
if ((compare_helper (take 1 first_stanz) (take 1 (drop 1 first_stanz))) = false) ||
 ((compare_helper (take 1 (drop 2 first_stanz))  (take 1 (drop 3 first_stanz))) = false) then true 
   else compare_helper2 (take 2 (drop 4 first_stanz)) (take 2 (drop 1 first_stanz));
      (* Same thing for second and third stanza*)
     compare_helper (take 1 second_stanz) (take 1 (drop 1 second_stanz));
     compare_helper (take 1 (drop 2 second_stanz)) (take 1 (drop 3 second_stanz));
if ((compare_helper (take 1 second_stanz) (take 1 (drop 1 second_stanz))) = false) || 
   ((compare_helper (take 1 (drop 2 second_stanz)) (take 1 (drop 3 second_stanz))) = false) then true 
   else compare_helper2 (take 2 (drop 4 second_stanz)) (take 2 (drop 1 second_stanz)); 
      compare_helper (take 1 third_stanz) (take 1 (drop 1 third_stanz));
      compare_helper (take 1 (drop 2 third_stanz)) (take 1 (drop 3 third_stanz));    
if ((compare_helper (take 1 third_stanz) (take 1 (drop 1 third_stanz))) = false) || 
   ((compare_helper (take 1 (drop 2 third_stanz)) (take 1 (drop 3 third_stanz))) = false) then true 
   else compare_helper2 (take 2 (drop 4 third_stanz)) (take 2 (drop 1 third_stanz)); 

(* Checks if final stanza uses all the words from the previous stanzas by sorting and checking equivalence*)
     compare_helper3 (fourth_stanz) ((take 2 (drop 4 third_stanz))@(take 2 (drop 4 second_stanz))@(take 2 (drop 4 first_stanz)))] 
   in 
   
   let line_numbers = [(1,2);(3,4);(5,6);(7,8);(9,10);(11,12);(13,14);(15,16);(17,18);(19,24)] in 
   let new_list = List.combine line_numbers truth_list in (* <-- Associates a bool value with a (int*int)*)
   let filtered_list = List.filter(fun (a,b) -> (<>) b true) new_list in  (* <-- Filters out the good lines *)
   let mapped_list = List.map(fun (x,y) -> x) filtered_list in  (* <-- extracts just the int tuple from the ((int*int)*bool *) 
   if mapped_list = [] then OK else (* if the list was all true values then the poem is a paradelle *)
   if is_elem (19,24) mapped_list then IncorrectLastStanza else IncorrectLines mapped_list 
    (* ^ if the last stanza (lines 19-24) is the only false value then IncorrectLastStanza else list the incorrect lines*)
