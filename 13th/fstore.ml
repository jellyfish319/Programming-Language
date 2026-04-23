type t = (string * string list * Ast.stmt list) list

let empty : t = []

let rec find (name : string) (ls : t) : (string list * Ast.stmt list) =
  match ls with
  | [] -> failwith ("Unbound function: " ^ name)
  | (n, args, stmts) :: u ->
      if n = name then (args, stmts) else find name u

let add (name : string) (args : string list) (stmts : Ast.stmt list) (ls : t) : t =
  let rec rec_remove ls acc =
    match ls with
    | [] -> acc
    | (n, a, s) :: tl ->
        if n = name then
          rec_remove tl acc
        else
          rec_remove tl ((n, a, s) :: acc)
  in
  (name, args, stmts) :: List.rev (rec_remove ls [])