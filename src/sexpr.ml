(* Identifiers *)
type id = string

(* S-Expressions *)
type atom =
  | AtomUnit
  | AtomBool  of bool
  | AtomInt   of int
  | AtomIdent of id

type sexpr =
  | SExprAtom of atom
  | SExprList of sexpr list

let rec sexpr_to_string e =
  match e with
  | SExprAtom AtomUnit -> "UNIT"
  | SExprAtom (AtomBool true) -> "TRUE"
  | SExprAtom (AtomBool false) -> "FALSE"
  | SExprAtom (AtomInt i) -> string_of_int i
  | SExprAtom (AtomIdent s) -> s
  | SExprList l ->
      let ss = l |> List.map sexpr_to_string |> String.concat " " in
      "(" ^ ss ^ ")"
