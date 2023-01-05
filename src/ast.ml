open Sexpr

exception Bad_expr of string

(* Expressions *)
type expr =
  | ExprUnit
  | ExprBool     of bool
  | ExprInt      of int
  | ExprIdent    of id
  | ExprFuncAppl of expr * expr list

let rec expr_of_sexpr e =
  match e with
  | SExprAtom AtomUnit -> ExprUnit
  | SExprAtom (AtomBool b) -> ExprBool b
  | SExprAtom (AtomInt i) -> ExprInt i
  | SExprAtom (AtomIdent s) -> ExprIdent s
  | SExprList (hd::tl) ->
      let expr_hd = expr_of_sexpr hd in
      (match expr_hd with
      | ExprIdent s -> ExprFuncAppl(ExprIdent s, List.map expr_of_sexpr tl)
      | _ -> raise (Bad_expr "need to have function in front"))
  | _ -> raise (Bad_expr "don't yet accept empty s-exps")

let rec expr_to_string e =
  match e with
  | ExprUnit -> "UNIT"
  | ExprBool true -> "TRUE"
  | ExprBool false -> "FALSE"
  | ExprInt i -> Printf.sprintf "INT(%d)" i
  | ExprIdent s -> Printf.sprintf "ATOM(%s)" s
  | ExprFuncAppl(ExprIdent s, args) ->
      let ss = args |> List.map expr_to_string |> String.concat ", " in
      Printf.sprintf "APPLY(%s, %s)" s ss
  | _ -> raise (Bad_expr "not acceptable expr")
