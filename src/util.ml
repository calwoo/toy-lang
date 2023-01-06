open Base

let rec zip_lists al bl =
  match (al, bl) with
  | a :: atl, b :: btl -> (a, b) :: zip_lists atl btl
  | [], [] -> []
  | _ -> raise (Failure "lists need to be of same length")
