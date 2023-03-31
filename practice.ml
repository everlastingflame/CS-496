(* Mutable Fields in Records 
   An example - Linked Lists
   22 March 2023
*)


type 'a node = { mutable data: 'a;
                 mutable next: 'a node option}

type 'a ll = { mutable head:'a node option;
               mutable size:int}

let ex1: int ll = { head = None;
                    size = 0}

let ex2: int ll =
  { head = Some { data=1; next=None};
    size = 1 }

let add_first : 'a ll -> 'a -> unit =
  fun ll e ->
  ll.head <- Some {data=e; next= ll.head};
  ll.size <- ll.size+1

let map : ('a -> 'b) -> 'a ll -> unit =
  fun f l ->
  let rec helper =
    fun no ->
    match no with
    | None -> ()
    | Some n ->
      n.data <- f n.data;
      helper n.next
  in helper l.head
    
let rec nth ll n =
  match ll.head with
  | None -> None
  | Some node ->
    if n >= ll.size then
      failwith "Error: n is greater than or equal to the size of the list"
    else if n = 0 then Some node.data
    else nth { head = node.next; size = ll.size - 1 } (n - 1)


let rec last =
  fun ll ->
  failwith "implement"

let rec append =
  fun ll1 ll2 ->
  failwith "implement"

let rec string_of_ll =
  fun ll to_string ->
  failwith "implement"
    
