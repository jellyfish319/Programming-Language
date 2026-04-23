type t = (string * Ast.typ) list

let empty : t = []

let rec find (name : string) (ls : t) : Ast.typ =
  match ls with
  | [] -> failwith ("Something wrong at " ^ name)
  | (n, t1) :: u ->
      if n = name then t1 else find name u

let add (name : string) (typ : Ast.typ) (ls : t) : t =
  let rec rec_remove ls acc =
    match ls with
    | [] -> acc
    | (n, t1) :: tl ->
        if n = name then
          rec_remove tl acc
        else
          rec_remove tl ((n, t1) :: acc)
  in
  (name, typ) :: List.rev (rec_remove ls [])