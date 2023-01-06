open Sexpr

exception Bad_expr of string

(* Expressions *)
type expr =
  | ExprUnit
  | ExprBool of bool
  | ExprInt of int
  | ExprIdent of id
  | ExprDef of id * expr
  | ExprLambda of id list * expr
  | ExprFuncAppl of expr * expr list

let rec expr_of_sexpr e =
  match e with
  | SExprAtom AtomUnit -> ExprUnit
  | SExprAtom (AtomBool b) -> ExprBool b
  | SExprAtom (AtomInt i) -> ExprInt i
  | SExprAtom (AtomIdent s) -> ExprIdent s
  | SExprList (hd :: tl) -> (
      let expr_hd = expr_of_sexpr hd in
      match expr_hd with
      | ExprIdent "define" -> (
          match tl |> List.map expr_of_sexpr with
          | [ ExprIdent n; e ] -> ExprDef (n, e)
          | _ -> raise (Bad_expr "not a valid definition"))
      | ExprIdent "lambda" -> (
          match tl with
          | [ SExprList args; e ] ->
              let ee = expr_of_sexpr e in
              let ids =
                args
                |> List.map (fun s ->
                       match s with
                       | SExprAtom (AtomIdent arg) -> arg
                       | _ -> raise (Bad_expr "not an argument of a lambda"))
              in
              ExprLambda (ids, ee)
          | _ -> raise (Bad_expr "not a valid lambda"))
      | ExprIdent s -> ExprFuncAppl (ExprIdent s, List.map expr_of_sexpr tl)
      | ExprLambda _ -> ExprFuncAppl (expr_hd, List.map expr_of_sexpr tl)
      | _ -> raise (Bad_expr "need to have function in front"))
  | _ -> raise (Bad_expr "don't yet accept empty s-exps")

let rec expr_to_string e =
  match e with
  | ExprUnit -> "UNIT"
  | ExprBool true -> "TRUE"
  | ExprBool false -> "FALSE"
  | ExprInt i -> Printf.sprintf "INT(%d)" i
  | ExprIdent s -> Printf.sprintf "ATOM(%s)" s
  | ExprDef (s, d) -> Printf.sprintf "DEF(%s = %s)" s (expr_to_string d)
  | ExprFuncAppl (ExprIdent s, args) ->
      let ss = args |> List.map expr_to_string |> String.concat ", " in
      Printf.sprintf "APPLY(%s, %s)" s ss
  | _ -> raise (Bad_expr "not acceptable expr")
