let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

(*________MAP________*)
let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t

(*________2.2.1________*)
let rec dReverse n = 
  match n with 
  |0 -> 0
  |1 -> 1
  |2 -> 4
  |3 -> 5
  |4 -> 2
  |5 -> 3
  | _ -> failwith "dReverse: Bad input."

let mirror_image = map (fun x -> dReverse x)

(*________2.2.2________*)
let rec nDegree n=
  match n with 
  |0 -> 0
  |1 -> 1
  |2 -> 3
  |3 -> 4
  |4 -> 5
  |5 -> 2
  | _ -> failwith "nDegree: Bad input."

let rotate_90_letter = map (fun x -> nDegree x)

(*________2.2.3________*)
let rotate_90_word = map (fun x -> rotate_90_letter x)

(*________2.2.4________*)
let rec repeat : int -> 'a -> 'a list =
  fun n e ->
  match n with
  | 0 -> []
  | m -> e :: repeat (m-1) e

(*________2.2.5________*)

let rec pantograph (a:int) k =
  match k with
  |[] -> []
  |h::t -> if  h = 1 || h = 0 then [h] @ pantograph a t
  else repeat a h @ pantograph a t

(*________2.2.6________*)

let coverageSumH (n:int) (location:int*int) : (int*int) = 
  match n with
  |0 -> (fst location, snd location)
  |1 -> (fst location, snd location)
  |2 -> (fst location, snd location + 1)
  |3 -> (fst location + 1, snd location)
  |4 -> (fst location, snd location -1)
  |5 -> (fst location -1, snd location)
  |_ -> failwith "coverageSumH: Bad input."
;;

let rec coverageSumT (n:int*int)  (l: int list) : (int*int) list =
    match l with 
    |[] -> []
    |h::t -> coverageSumH h n :: coverageSumT (coverageSumH h n) t 
;;

let coverage (n:int*int) (l:int list) : (int*int) list = n::coverageSumT n l 
;; 

(*________2.2.7________*)
let rec compressHelper (n:int*int) (l:int list) =
  match l with
  |[] -> [n]
  |h::t ->
  if h == fst n then
    compressHelper (fst n, snd n + 1) t
  else 
  (fst n, snd n) :: compressHelper (h, 1) t
  

let compress (l:int list) : (int*int) list =
  match l with 
  |[] -> []
  |h::t -> compressHelper (0,0) l

(*________2.2.8________(a)*)

let uncompressHelp (n:int*int) = 
  repeat (snd n) (fst n)

let rec uncompress (n:(int*int) list) = 
  match n with 
  |[] -> [] 
  |h::t -> uncompressHelp h @ uncompress t

(*________2.2.8________(b)*)

(* let uncompress_mHelp (n:(int*int)) =
  repeat (snd n) (fst n)
  
let rec uncompress_m (n:(int*int) list) = 
  match n with
  |[] -> []
  |h::t -> (map (fun x -> uncompress_mHelp h)) *)

(*________2.2.8________*)
