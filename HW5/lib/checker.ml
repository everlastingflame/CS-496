(*Written by Justin Duran and Charles Booth*)
(*I pledge my honor that I have abided by the stevens honor code*)

open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser
       
let rec chk_expr : expr -> texpr tea_result =
  fun e ->
  match e with
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error "LetRec: Type of recursive function does not match
declaration")
   | Pair(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    return @@ PairType(t1,t2)
  | Unpair(id1,id2,e1,e2) ->
    chk_expr e1 >>= fun t ->
    (match t with
     | PairType(t1,t2) ->
    extend_tenv id1 t1 >>+
    extend_tenv id2 t2 >>+
    chk_expr e2
     | _ -> error "unpair: expected a pair")
      
  (* EXPLICIT-REFS *)
  (*1*)
  | Unit -> return UnitType
  | BeginEnd([]) ->
    return UnitType

  (*2*)
  | BeginEnd(es) ->
    sequence (List.map chk_expr es) >>= fun ts ->
    return (List.hd (List.rev ts))

  (*3*)
  | NewRef(e) ->
    chk_expr e >>= fun t -> return (RefType(t)) 

  (*4*)
  | DeRef(e) ->
    chk_expr e >>= fun cRef ->
    (match cRef with
    | RefType(t) -> return t
    | _ -> error "Error: deref needs a reference")

  (*5*)
  | SetRef(e1,e2) ->
    chk_expr e1 >>= fun cRef ->
    chk_expr e2 >>= fun t ->
    (match cRef with
    | RefType(t') when t = t' -> return UnitType
    | _ -> error "SetRef : Expected a reference type ")

  (* list *)
  | EmptyList(None) ->
    error "Error: type annotation missing"

  | EmptyList(Some t) ->
    return (ListType(t))

  | Cons(h, t) ->
    chk_expr h >>= fun th ->
    chk_expr t >>= fun tt ->
    (match tt with
    | ListType t' when t' = th -> return tt
    | _ -> error "type not matching")
  
  | IsEmpty(e) ->
    chk_expr e >>= fun t ->
    (match t with
    | ListType _ | TreeType _ -> return BoolType
    | _ -> error "needs list or tree type")

    
  | Hd(e) ->
    chk_expr e >>= fun t ->
      (match t with
      | ListType t' -> return t'
      | _ -> error "needs list type")


  | Tl(e) ->
    chk_expr e >>= fun t ->
      (match t with
      | ListType t' -> return (ListType t')
      | _ -> error "needs list type")

  (* tree *)
  | EmptyTree(None) ->
    error "Error: type annotation missing"

  | EmptyTree(Some t) ->
    return (TreeType t)


  | Node(de, le, re) ->
    chk_expr de >>= fun tde ->
      chk_expr le >>= fun tle ->
      chk_expr re >>= fun tre ->
        begin match tle, tre with
        | TreeType tl, TreeType tr when tl = tr ->
            return (TreeType tl)
        | _ -> error "type not matching"
        end


  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->
    chk_expr target >>= fun t_target ->
      (match t_target with
      | UnitType -> chk_expr emptycase
      | TreeType t ->
        extend_tenv id1 t >>+
        extend_tenv id2 (TreeType t) >>+
        extend_tenv id3 (TreeType t) >>+
        chk_expr nodecase
      | _ -> error "mismatch")
  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> failwith "chk_expr: implement"    
and
  chk_exprs =
  fun es ->
  match es with
  | [] -> return []
  | h::tl -> chk_expr h >>= fun t ->
    chk_exprs tl >>= fun ts ->
    return (t::ts)
and
  chk_prog (AProg(_,e)) =
  chk_expr e

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)



