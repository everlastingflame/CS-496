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

(*
    OCaml - Lecture 5
    27 Jan 2023
*)

let succ i = i+1
let upper c = Char.uppercase_ascii c
let isz i = i=0
            
let rec succl : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> succ h :: succl t
              
let rec upperl : char list -> char list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> upper h :: upperl t

let rec is_zerol : int list -> bool list =
  fun l -> 
  match l with
  | [] -> []
  | h::t -> isz h :: is_zerol t

let rec map : ('a -> 'b) -> 'a list -> 'b list =
  fun f l ->
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let succl' l = map succ l
let upperl' l = map upper l
let is_zerol' l = map isz l 

let succl'' l = map (fun i -> i+1) l

let is_pos i = i>0
let is_upper c = c=Char.uppercase_ascii c
let is_ne l = l!=[]
                 
let rec get_gtz : int list -> int list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_pos h
    then h :: get_gtz t
    else get_gtz t
    
let rec get_upper : char list -> char list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_upper h
    then h :: get_upper t
    else get_upper t
        
let rec get_ne : 'a list list -> 'a list list =
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if is_ne h
    then h :: get_ne t
    else get_ne t

let rec filter : ('a -> bool) -> 'a list -> 'a list =
  fun p l ->
  match l with
  | [] -> []
  | h::t ->
    if p h
    then h:: filter p t
    else filter p t
        
let get_gtz' l = filter is_pos l    
let get_upper' l = filter is_upper l        
let get_ne' l = filter is_ne l 

let get_gtz'' l = filter (fun i -> i>0) l    



let rec suml : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + suml t
                
let rec andl : bool list -> bool =
  fun l ->
  match l with
  | [] -> true
  | h::t -> h && andl t

let rec concat : 'a list list -> 'a list =
  fun l ->
  match l with
  | [] -> []
  | h::t -> h @ concat t

let rec foldr : ('a -> 'b -> 'b) -> 'b -> 'a list ->'b =
  fun f a l ->
  match l with
  | [] -> a
  | h::t -> f h (foldr f a t)

let suml' l = foldr (fun i j -> i+j) 0 l
let andl' l = foldr (fun i j -> i &&j) true l
let concat' l = foldr (fun i j -> i @j) [] l

let suml'' l = foldr (+) 0 l
let andl'' l = foldr (&&) true l
let concat'' l = foldr (@) [] l

(* 

foldr f a [x;y;z]

f x (f y (f z a))

*)
    
