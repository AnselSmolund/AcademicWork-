
type expr 
  = Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  
  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr

  | If of expr * expr * expr

  | Id of string
  
  | Let of string * expr * expr
  | LetRec of string * expr * expr

  | App of expr * expr
  | Lambda of string * expr

  | Value of value

and value
  = Int of int
  | Bool of bool
  | Closure of string * expr * environment
  | Ref of value ref 

and environment = (string * value) list


let idVal expr = 
  match expr with 
  |Id x -> x 
  |_ -> raise(Failure "Not an expression")

  let rec freevars (e:expr) : string list = 
  match e with
  |Value v -> []
  |Add (e1, e2) -> freevars e1 @ freevars e2
  |Sub(e1,e2) -> freevars e1 @ freevars e2 
  |Mul(e1,e2)-> freevars e1 @ freevars e2     
  |Div(e1,e2)-> freevars e1 @ freevars e2          
  |App (f, a) -> freevars f @ freevars a
  |Lambda (i, body) -> 
     List.filter 
       (fun fv -> fv <> i)
       (freevars body)
  |Id i -> [i]
  |Let (i, dexpr, body) ->
     freevars dexpr @
     List.filter 
       (fun fv -> fv <> i)
       (freevars body)
  |LetRec(s,e1,e2) -> List.filter(fun fv -> fv<>s)(freevars e2 @freevars e1)
  |If(e1,e2,e3) -> freevars e1 @ freevars e2 @ freevars e3 
  |Eq(e1,e2) -> freevars e1 @ freevars e2
  |Lt(e1,e2) -> freevars e1 @ freevars e2 
  |And(e1,e2) -> freevars e1 @ freevars e2

let toInt expr = 
  match expr with 
  |Int x -> x 
  |_ -> raise(Failure "not an int")

let typeBool expr = 
  match expr with
  |Bool x -> x 
  |_ -> raise(Failure "not a bool")



  let rec lookup (n:string) (env:environment) =
  match env with
  | [] -> raise (Failure ("Identifier " ^ n ^ " is not in scope."))
  | (x,value)::rest when x = n -> value
  | _::rest -> lookup n rest

let rec eval (env:environment) expr = 
  match expr with
  |Value v -> v 
  |Add(e1,e2) -> 
      (match eval env e1, eval env e2 with
        |Int i1, Int i2 -> Int(i1 + i2)
        | _ -> raise(Failure "Incombatible types on Add")
      )
  |Div(e1,e2) -> 
      (match eval env e1, eval env e2 with
        |Int i1, Int i2 -> Int(i1 / i2)
        | _ -> raise (Failure "Incombatible types on Div")
      )
  |Sub (e1, e2) -> 
     ( match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Int (i1 - i2) 
       | _ -> raise (Failure "Incompatible types on Sub")
     )
  |Lt (e1, e2) -> 
     ( match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Bool (i1 < i2) 
       | _ -> raise (Failure "Incompatible types on Lt")
   
     )
   |And (e1, e2) -> 
     ( match eval env e1, eval env e2 with
       | Bool i1, Bool i2 -> Bool (i1 && i2) 
       | _ -> raise (Failure "Incompatible types on Lt")
   
     )
  |Mul(e1,e2) -> 
      (match eval env e1, eval env e2 with
        |Int i1, Int i2 -> Int(i1 * i2)
        | _ -> raise (Failure "Incombatible types on Mul")
      )
  |If(e1,e2,e3) -> if typeBool (eval env e1) = true then eval env e2 else eval env e3
  |Eq (e1, e2) -> 
     ( match eval env e1, eval env e2 with
       | Int i1, Int i2 -> Bool (i1 = i2) 
       | _ -> raise (Failure "Incompatible types on Eq")
     )
  |Let(s,e1,e2) -> eval((s,eval env e1)::env) e2
  |Lambda(s,e1) -> Closure(s,e1,env)
  |App(e1,e2) -> (match eval env e1 with 
               | Closure(a,b,e) -> eval ((a,eval env e2)::e) b
               | _ -> raise(Failure "Incompatible types on App")  
                 )
  |Id x -> (match lookup x env with 
          |Ref value -> !value
          |value -> value
        )             
  |LetRec(x,e1,e2) -> match e1 with 
                      |Lambda(e1',e2')->
                        let recRef = ref(Int 200) in
                        let close = Closure(e1', e2',(x,Ref recRef)::env)in
                        let () = recRef := close in 
                        close 
                      |_ -> raise(Failure "let rec expressions must declare a function")

let evaluate (e:expr) : value = 
      eval [] e 
 


(* Some sample expressions *)

let inc = Lambda ("n", Add(Id "n", Value (Int 1)))

let add = Lambda ("x",
                  Lambda ("y", Add (Id "x", Id "y"))
                 )

(* The 'sumToN' function *)
let sumToN_expr : expr =
    LetRec ("sumToN", 
            Lambda ("n", 
                    If (Eq (Id "n", Value (Int 0)),
                        Value (Int 0),
                        Add (Id "n", 
                             App (Id "sumToN", 
                                  Sub (Id "n", Value (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )
let twenty_one : value = evaluate (App (sumToN_expr, Value (Int 6)))
