(* Quiz 1 - 1 Feb 2023 
   Student name 1: Charles Booth  
   Student name 2: Ben Griepp
*)


(* Note: you may add helper functions *)

let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t

(* Sample Tree *)
let ex = [(12, 7); (12, 43); (7, 4); (43, 33); (43,77)]

(*
      12
      /\ 
     /  \  
    7   43
   /    /\ 
  /    /  \  
 4    33  77
*)
         
(** [outgoing_nodes t n] returns the list of nodes outgoing from node
 ** [n] in the tree [t]. You may assume the node [n] exists in the
 ** tree [t] .
 ** Eg. outgoing_nodes ex 12 => [7; 43]
*)

let rec outgoing_nodes t (n:int) =
  List.map snd (List.filter(fun(src, tgt) -> src = n) t)

    
(**  [nodes t] returns the list of nodes of the tree without
    duplicates. The order of the
   nodes in the list is irrelevant.
   eg. nodes ex => [12; 7; 4; 33; 43; 77]

*)
let rec rem_dups l=
  match l with
  |[] -> []
  |h::t ->
      if List.mem h t
      then rem_dups t
      else h::rem_dups t

let rec nodes t =
  rem_dups(List.flatten (List.map (fun(src, tgt) -> [src;tgt]) t))

(** [leaves t] returns the leaves of the tree [t]
   Eg. leaves ex =>  [4; 33; 77]
*)
let leaves t =
  failwith "complete"

(* 
   Returns the root of a tree
   Eg. root ex =>  [12]
*)
let root t =
  failwith "complete"

(* 
   Returns the boolean indicating if the tree is a binary tree.
   Eg. is_binary ex =>  true
*)
let is_binary t =
  failwith "complete"

(** [subtree t n] returns the subtree rooted at node [n].
 **  Eg. subtree ex 12 => [(43, 33); (43, 77); (7, 4); (12, 7); (12, 43)]
         subtree ex 43 -=> [(43, 33); (43, 77)]
         subtree ex 7 => [(7, 4)]
         subtree ex 4 => []
*)
let rec subtree t (n:int) =
  failwith "complete"

                               

