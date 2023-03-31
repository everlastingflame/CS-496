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
    
(** [remove e l] removes first occurrence of [e] from [l]. Returns an error if [e] is not in [l] *)
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
