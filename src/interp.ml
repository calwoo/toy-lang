(* Toy interpreter for this language *)

open Core
open Ast.Ast_expr
open Util

exception Bad_interp of string

type id = string

type value =
  | ValUnit
  | ValBool of bool
  | ValInt of int
  | ValPrim of (value list -> value)
  | ValLambda of closure
  | ValList of value list

and env_t = (id, value, String.comparator_witness) Map.t
and env = { parent : env option; bindings : env_t }
and closure = { name : id option; env : env; argnames : id list; body : expr }

let rec value_to_string v =
  match v with
  | ValUnit -> "#u"
  | ValBool true -> "#t"
  | ValBool false -> "#f"
  | ValInt i -> string_of_int i
  | ValPrim _ -> "<prim fn>"
  | ValLambda { name; _ } -> (
      match name with
      | Some s -> Printf.sprintf "<func %s>" s
      | None -> "<lambda>")
  | ValList vs ->
      let vss = vs |> List.map ~f:value_to_string |> String.concat ~sep:", " in
      Printf.sprintf "(%s)" vss

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
  | "-" -> (
      fun args ->
        match args with
        | [ ValInt a; ValInt b ] -> ValInt (a - b)
        | _ -> raise (Bad_interp "not a valid input to minus"))
  | "*" ->
      fun args ->
        List.fold_left
          ~f:(fun a b ->
            match (a, b) with
            | ValInt x, ValInt i -> ValInt (x * i)
            | _ -> raise (Bad_interp "can only multiply numbers"))
          ~init:(ValInt 1) args
  | "<" -> (
      fun args ->
        match args with
        | [ ValInt a; ValInt b ] -> ValBool (a < b)
        | _ -> raise (Bad_interp "not a valid input to less-than"))
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
  | "=" -> (
      fun args ->
        match args with
        | [ ValInt a; ValInt b ] -> ValBool (a = b)
        | _ -> raise (Bad_interp "not a valid input to ="))
  (* List builtins *)
  | "list?" -> (
      fun args ->
        match args with
        | [ ValList _ ] -> ValBool true
        | [ _ ] -> ValBool false
        | _ -> raise (Bad_interp "too many arguments"))
  (* Misc *)
  | "print" -> (
      fun args ->
        match args with
        | [ v ] ->
            print_endline (value_to_string v);
            ValUnit
        | _ -> raise (Bad_interp "not a valid input to print"))
  | _ -> raise (Bad_interp "not a builtin primitive")

(* Initialized environment *)
let empty_env = { parent = None; bindings = Map.empty (module String) }
let to_list t = Map.to_alist t

let print_env env =
  to_list env.bindings
  |> List.map ~f:(fun (name, v) ->
         Printf.sprintf "%s -> %s" name (value_to_string v))
  |> String.concat ~sep:"\n" |> print_endline;
  print_endline "==="

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

exception Not_enough_args

type interp_value = { value : value; env : env }

let rec eval exp env =
  match exp with
  | ExprUnit -> { value = ValUnit; env }
  | ExprInt i -> { value = ValInt i; env }
  | ExprBool b -> { value = ValBool b; env }
  | ExprLambda (args, body) ->
      let scoped_env =
        { parent = Some env; bindings = Map.empty (module String) }
      in
      {
        value =
          ValLambda { name = None; env = scoped_env; argnames = args; body };
        env;
      }
  | ExprIdent s -> (
      try { value = ValPrim (eval_primitive s); env }
      with Bad_interp _ -> (
        try { value = lookup_in_env env s; env }
        with Not_found_in_env ->
          raise (Bad_interp ("undefined variable " ^ s))))
  | ExprDef (name, v) ->
      let v_eval =
        match (eval v env).value with
        (* if we see a lambda, give it a name! this is for recursion down the line *)
        | ValLambda closure -> ValLambda { closure with name = Some name }
        | _ -> (eval v env).value
      in
      let next_env = add_to_env env name v_eval in
      { value = ValUnit; env = next_env }
  | ExprBlock exprs ->
      exprs
      |> List.fold
           ~f:(fun acc e -> eval e acc.env)
           ~init:{ value = ValUnit; env }
  | ExprIf (cond, then_branch, else_branch) -> (
      let cond_val = (eval cond env).value in
      match cond_val with
      | ValBool true -> eval then_branch env
      | ValBool false -> eval else_branch env
      | _ -> raise (Bad_interp "condition must evaluate to a boolean"))
  | ExprFuncAppl (f, args) -> (
      let f_eval = (eval f env).value in
      let args_eval = List.map ~f:(fun e -> (eval e env).value) args in
      match f_eval with
      | ValPrim p -> { value = p args_eval; env }
      | ValLambda closure -> (
          match closure.name with
          | Some name ->
              (* Allow for recursive functions by adding lambda into environment *)
              let rec_env = add_to_env closure.env name f_eval in
              {
                value =
                  eval_lambda closure.body ~argnames:closure.argnames
                    ~argvalues:args_eval ~env:rec_env;
                env;
              }
          | None ->
              {
                value =
                  eval_lambda closure.body ~argnames:closure.argnames
                    ~argvalues:args_eval ~env:closure.env;
                env;
              })
      | _ -> raise (Bad_interp "not a function to be applied"))
  | ExprList exprs ->
      let vals = exprs |> List.map ~f:(fun e -> (eval e env).value) in
      { value = ValList vals; env }

and eval_lambda exp ~argnames ~argvalues ~env =
  if not (List.length argnames = List.length argvalues) then
    raise Not_enough_args
  else
    let arg_assignments = zip_lists argnames argvalues in
    let lambda_env =
      List.fold
        ~f:(fun acc (arg, v) -> add_to_env acc arg v)
        ~init:env arg_assignments
    in
    (eval exp lambda_env).value
