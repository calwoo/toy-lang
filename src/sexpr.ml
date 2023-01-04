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
