(*  
    OCaml - Lecture 3
    23 Jan 2023 
    OCaml - Lecture 4 
    25 Jan 2023
*)
(* Simple examples of recursion on numbers *)
(** [fact n] returns the factorial of [n].
    Precondition: [n] is positive *)
let rec fact n =
  match n with
  | 0 -> 1
  | m -> m * fact (m-1)

let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t
  
let rec fact' n =
  match n with
  | 0 -> 1
  | m when m>0 -> m * fact' (m-1)
  | _ -> failwith "fact: neg input"
let rec repeat (e:'a) (n:int) : 'a list =
  match n with
  | 0 -> []
  | m -> e :: repeat e (m-1)
(* preferred type annotation style, but both are accepted *)
let rec repeat : 'a -> int -> 'a list =
  fun e n ->
  match n with
  | 0 -> []
  | m -> e :: repeat e (m-1)
(* simple examples of recursion on lists *)
let rec length : 'a list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> 1  + length t
let rec sum : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + sum t
let rec rev l =
  match l with
  | [] -> []
  | h::t -> rev t @ [h]
let rec frev l a =
  match l with
  | [] -> a
  | h::t -> frev t (h::a)
(*
    OCaml - Lecture 4 
    25 Jan 2023
*)
let rec mem =
  fun e l ->
  match l with
  | [] -> false
  | h::t -> (h=e) || mem e t
     
let rec has_duplicates : 'a list -> bool =
  fun l ->
  match l with
  | [] -> false
  | h::t -> mem h t || has_duplicates t      
    
(** [remove e l] removes first occurrence of [e] from [l]. Returns an error if [e] 
is not in [l] *)
let rec remove : 'a -> 'a list -> 'a list =
  fun e l ->
  match l with
  | [] -> failwith "remove: element not in list"
  | h::t ->
    if h=e
    then t
    else h :: remove e t
(** [remove_all e l] removes all occurrences of [e] from [l]. *)
let rec remove_all : 'a -> 'a list -> 'a list =
  fun e l ->
  match l with
  | [] -> []
  | h::t ->
    if h=e
    then remove_all e t
    else h :: remove_all e t
           
let rec remove_duplicates : 'a list -> 'a list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if mem h t
    then remove_duplicates t
    else h :: remove_duplicates t
(** [last l] returns the last element in the list.
    It fails if [l] is empty *)
let rec last : 'a list -> 'a =
  fun l ->
  match l with
  | [] -> failwith "last: empty list"
  | [h] -> h
  | _::h2::t -> last (h2::t)
(** [take n l] returns a list with the first [n] elements of [l].
    Precondition: [n] is positive. 
l= [1;2;3]
take 0 l => []
take 1 l => [1]
take 2 l => [1;2]
take 200 l => [1;2;3s]
*)
let rec take n l =
  failwith "complete"
let rec permutations l =
  failwith "complete"
                 
(** [rad l] removes all adjacent duplicates *)
let rec rad l =
  failwith "complete"


(*Lecture 6, 1/30/2023*)

type dow = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let is_Weekend d= 
  match d with
  |Sat | Sun -> true
  | _ -> false

let next d =
  match d with
  |Mon -> Tue
  |Tue -> Wed
  |Wed -> Thu
  |Thu -> Fri
  |Fri -> Sat
  |Sat -> Sun
  |Sun -> Mon

(*Example of ADT whose constructors have arguments*)

type fla = Van | Cho | Str
type ic = Cone of fla | Cup of fla*fla | Bucket of fla list

let ic1 = Cone(Cho)
let ic2 = Cup(Van, Van)
let ic3 = Bucket([Van;Van;Str])

let cost ic =
  match ic with
  |Cone(_) -> 1
  |Cup(_,_) -> 2
  |Bucket(_) -> 5


let is_boring ic =
  match ic with
  |Cone(Van) -> true
  |Cup(Van, Van) -> true
  |Bucket fs -> List.for_all((=)Van) fs
  |_ -> false

(*Example of ADT that is polymorphic*)

type 'a result = Ok of 'a | Error of string

let rec lookup k d =
  match d with
  |[] -> Error "Not found"
  |(k', v)::t ->
    if k=k'
    then Ok v 
    else lookup k t 

(*Another example of a polymporphic ADT*)

type ('a, 'b) either = Left of 'a | Right of 'b

(*

Left 1: (int, bool) either
Left 2: (int, bool) either
Right true: (int, bool) eitherx
Right false: (int, bool) either

*)

(*Example of a recursive (and polymorphic) ADT*)

type 'a bt = Empty | Node of 'a*'a bt* 'a bt

(*
   33
  /  \
 12   44
     /  \
    37  58

*)

let t1: int bt =
Node(33, Node(12, Empty, Empty), Node(44, Node(37, Empty Empty), Node(58, Empty, Empty)))

let rec size t =
  match t with
  |Empty -> 0
  |Node(_, lt, rt) -> 1 + size lt + size rt

let rec sum t =
  match t with
  |Empty -> 0
  |Node(d, lt, rt) -> d + sum lt + Sum rt

let rec m t =
  match t with
  |Empty -> Empty
  |Node(d, lt, rt) -> Node(d, m rt, m lt)

let rec map : ('a -> 'b) -> 'a bt -> 'b bt =
  fun f t ->
  match t with
  |Empty -> Empty
  |Node(d, lt, rt) -> Node(f d, map f lt, map f rt)

let rec m t =
  |Empty -> []
  |Node(d, lt, rt) -> [d] @ m lt @ m rt

let rec pos t = 
  match t with
  |Empty -> []
  |Node(d, lt, rt) ->  pos lt @ pos rt @ [d]

let rec no_of_leaves t = 
  match t with
  |Empty -> 0
  |Node(d, Empty, Empty) -> 1
  |Node(d,lt,rt) -> no_of_leaves lt + no_of_leaves + rt

let rec fold f a t =
  match t with
  |Empty -> a
  |Node(d,lt,rt) -> f d (fold f a lt) (fold f a rt)