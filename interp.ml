open Parser.plaf.Ast
open Parser_plaf.Parser

let rec eval_expr e = 
  match e with
  |Int n -> n
  |Sub(e1,e2) ->
    (match eval_expr e1 with
    |Error s -> Error s
    |Ok m ->
      (match eval_expr2 e2 with
      |Error s -> Error s
      |Ok n -> Ok (m-n)))
  |Div(e1,e2) ->
  |_ -> failwith "not implemented"

let eval_prog (AProg(_,e)) = 
  eval_expr e

let interp(e:string) : int result = 
