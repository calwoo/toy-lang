open Sexpr

(* Expressions *)
type expr =
  | ExprUnit
  | ExprBool     of bool
  | ExprInt      of int
  | ExprIdent    of id
  | ExprFuncAppl of expr * expr list

