(*

   Quiz 5 - Mutable Data Structures in OCaml
   31 Mar 2023
   Names: Charles Booth
   Pledge: I Pledge My Honor that I have Abided by the stevens honor codec
*)

type 'a node = {
  mutable data: 'a;
  mutable left: 'a node option;
  mutable right: 'a node option}


type 'a bt = {
  mutable root: 'a node option;
  mutable size: int}

(* Sample binary tree:

      7
     / \
    3   77
       /
      33
*)

let t1:int bt =
  { root = Some { data=7;
                  left = Some {data=3; left=None; right=None};
                  right = Some {data=77;
                                left=Some {data=33; left=None; right=None};
                                right=None} };
    size = 4}


(** [no_of_nodes t] returns the number of nodes of the binary tree [t] 
    NOTE: you CANNOT return the value of the size field. Youb MUST
    travarse the tree itself.
    Eg. 
    # no_of_nodes t1;;
    - : int = 4
*)

let rec no_of_nodes_helper : 'a node option -> int = 
  function 
  | None -> 0
  | Some node -> 1 + no_of_nodes_helper node.left + no_of_nodes_helper node.right (*Recursively call helper function to traverse tree, adding to sum*)

let no_of_nodes : 'a bt -> int =
  fun t ->
  match t.root with 
  | None -> 0       
  | Some root -> no_of_nodes_helper(Some root) 
      

(** [mirror t] swaps all the left and right children of [t] thus
    producing its mirror image.
    Eg. 
    # mirror t1;;
    - : unit = ()
    # t1;;
    - : int bt =
    {root = Some
             {data = 7;
              left = Some
                      {data = 77; left = None;
                       right = Some {data = 33; left = None; right = None}};
              right = Some {data = 3; left = None; right = None}};
     size = 4}
*)

let rec mirror_help : 'a node option -> unit = 
  function 
  | None -> ()
  | Some node -> 
    let tempL = node.left in  (*Create temp to retain left part of tree for swapping*)
    node.left <- node.right;
    node.right <- tempL;
    mirror_help node.left;    (*Recursively call until all cases exhausted*)
    mirror_help node.right

let mirror : 'a bt -> unit =
  fun t ->
  match t.root with
  | None -> ()
  | Some root -> mirror_help(Some root)

(** [add e t] adds [e] to the binary search tree [t]. 
    Eg. 
    # add 44 t1;;
    - : unit = ()
    # t1;;
    - : int bt =
    {root = Some
             {data = 7; left = Some {data = 3; left = None; right = None};
              right = Some
                      {data = 77;
                       left = Some
                               {data = 33; left = None;
                                right = Some {data = 44; left = None; right = None}};
                       right = None}};
     size = 5}
*)

let rec add_help : 'a -> 'a node option ref -> unit = 
  fun e node ->
    match !node with 
    | None -> 
      let new_node = {data = e; left = None; right = None} in (*Create new node at top of tree with no nodes*)
      node := Some new_node
    | Some parent_node ->                                     (*Case for if tree has nodes*)
      if e < parent_node.data then 
        match parent_node.left with                           (*Recursively traverse tree until cases are met*)
        | None ->                      
          let new_node = {data = e; left = None; right = None} in 
          parent_node.left <- Some new_node;                  (*Finally gets to end of tree, adds node*)
          ()
        | Some left_child -> add_help e (ref parent_node.left) (*Recursively call helper*)
      else if e > parent_node.data then                       (*Goes right if greater than parent*)
        match parent_node.right with                        
        | None -> 
          let new_node = {data = e; left = None; right = None} in 
          parent_node.right <- Some new_node;
          ()
        | Some right_child -> add_help e (ref parent_node.right) (*Gets to the end and adds to the tree*)
      else 
        ()

let add : 'a -> 'a bt ->  unit =
  fun e t ->
  add_help e (ref t.root);                       (*Adds node to tree*)
  t.size <- no_of_nodes t          (*Updates tree size with previous function*)
