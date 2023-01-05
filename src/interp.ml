(* Toy interpreter for this language *)

open Core
open Ast

exception Bad_interp of string

type id = string

type value =
  | ValUnit
  | ValBool of bool
  | ValInt of int
  | ValPrim of (value list -> value)
  | ValLambda of env * id list * expr list

and env_t = (id, value, String.comparator_witness) Map.t
and env = { parent : env option; bindings : env_t }

let value_to_string v =
  match v with
  | ValUnit -> "#u"
  | ValBool true -> "#t"
  | ValBool false -> "#f"
  | ValInt i -> string_of_int i
  | _ -> "?"

let eval_primitive (prim : string) : value list -> value =
  match prim with
  | "+" ->
      fun args ->
        List.fold_left
          ~f:(fun a b ->
            match (a, b) with
            | ValInt x, ValInt i -> ValInt (x + i)
            | _ -> raise (Bad_interp "can only add numbers"))
          ~init:(ValInt 0) args
  | "*" ->
      fun args ->
        List.fold_left
          ~f:(fun a b ->
            match (a, b) with
            | ValInt x, ValInt i -> ValInt (x * i)
            | _ -> raise (Bad_interp "can only add numbers"))
          ~init:(ValInt 1) args
  | "bool?" -> (
      fun args ->
        match args with
        | [ ValBool _ ] -> ValBool true
        | [ _ ] -> ValBool false
        | _ -> raise (Bad_interp "too many arguments"))
  | _ -> raise (Bad_interp "not a builtin primitive")

let rec eval exp env =
  match exp with
  | ExprUnit -> ValUnit
  | ExprInt i -> ValInt i
  | ExprBool b -> ValBool b
  | ExprIdent s -> (
      try ValPrim (eval_primitive s)
      with Bad_interp _ -> raise (Bad_interp "we only accept builtins now"))
  | ExprFuncAppl (f, args) -> (
      let f_eval = eval f env in
      let args_eval = List.map ~f:(fun e -> eval e env) args in
      match f_eval with
      | ValPrim p -> p args_eval
      | _ -> raise (Bad_interp "not yet implemented"))

(* Initialized environment *)
let empty_env = { parent = None; bindings = Map.empty (module String) }
let to_list t = Map.to_alist t

let add_to_env env k v =
  { env with bindings = Map.set env.bindings ~key:k ~data:v }
