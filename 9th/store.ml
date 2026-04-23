(*store.ml*)

type t = (string * value) list
and value =
  | NumV of int
  | ClosureV of string * Ast.expr * t (*ClosureV : Var * Expr * Store*)
  | BoolV of bool

let empty : t = []

let rec find (name : string) (ls : t) : value =
  match ls with
  | [] -> failwith ("Free identifier: " ^ name)
  | (n, v1)::u -> if n = name then v1 else find name u

let add (name : string) (v : value) (ls : t) : t =
  let rec rec_remove ls acc =
    match ls with
    | [] -> acc
    | (n, v1) :: tl ->
        if n = name then
          rec_remove tl acc
        else
          rec_remove tl ((n, v1) :: acc)
  in
  (name, v) :: List.rev (rec_remove ls [])