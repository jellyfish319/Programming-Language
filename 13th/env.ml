type addr = int

type t = (string * addr) list

let empty : t = []

let rec find (name : string) (ls : t) : addr =
  match ls with
  | [] -> failwith ("Free identifier: " ^ name)
  | (n, a)::u -> if n = name then a else find name u

let add (name : string) (address : addr) (ls : t) : t =
  let rec rec_remove ls acc =
    match ls with
    | [] -> acc
    | (n, v1) :: tl ->
        if n = name then
          rec_remove tl acc
        else
          rec_remove tl ((n, v1) :: acc)
  in
  (name, address) :: List.rev (rec_remove ls [])