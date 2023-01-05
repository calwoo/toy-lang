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
  (* Arithmetic functions *)
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
  (* Integer builtins *)
  | "zero?" -> (
      fun args ->
        match args with
        | [ ValInt i ] -> ValBool (i = 0)
        | [ _ ] -> ValBool false
        | _ -> raise (Bad_interp "too many arguments"))
  | "int?" -> (
      fun args ->
        match args with
        | [ ValInt _ ] -> ValBool true
        | [ _ ] -> ValBool false
        | _ -> raise (Bad_interp "too many arguments"))
  (* Boolean builtins *)
  | "bool?" -> (
      fun args ->
        match args with
        | [ ValBool _ ] -> ValBool true
        | [ _ ] -> ValBool false
        | _ -> raise (Bad_interp "too many arguments"))
  | "not" -> (
      fun args ->
        match args with
        | [ ValBool b ] -> ValBool (not b)
        | _ -> raise (Bad_interp "not a valid input to not"))
  | _ -> raise (Bad_interp "not a builtin primitive")

(* Initialized environment *)
let empty_env = { parent = None; bindings = Map.empty (module String) }
let to_list t = Map.to_alist t

let add_to_env env k v =
  { env with bindings = Map.set env.bindings ~key:k ~data:v }

exception Not_found_in_env

let rec lookup_in_env env k =
  match Map.find env.bindings k with
  | Some v -> v
  | None -> (
      match env.parent with
      | None -> raise Not_found_in_env
      | Some eenv -> lookup_in_env eenv k)

type interp_value = { value : value; env : env }

let rec eval exp env =
  match exp with
  | ExprUnit -> { value = ValUnit; env }
  | ExprInt i -> { value = ValInt i; env }
  | ExprBool b -> { value = ValBool b; env }
  | ExprIdent s -> (
      try { value = ValPrim (eval_primitive s); env }
      with Bad_interp _ -> (
        try { value = lookup_in_env env s; env }
        with Not_found_in_env ->
          raise (Bad_interp ("undefined variable " ^ s))))
  | ExprDef (name, v) ->
      let v_eval = eval v env in
      let next_env = add_to_env env name v_eval.value in
      { value = ValUnit; env = next_env }
  | ExprFuncAppl (f, args) -> (
      let f_eval = eval f env in
      let args_eval = List.map ~f:(fun e -> (eval e env).value) args in
      match f_eval.value with
      | ValPrim p -> { value = p args_eval; env }
      | _ -> raise (Bad_interp "not yet implemented"))
