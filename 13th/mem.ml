type t = (Env.addr * Value.t) list

let empty : t = []

let rec find (name : Env.addr) (ls : t) : Value.t =
  match ls with
  | [] -> failwith ("Free address: " ^ string_of_int name)
  | (n, v1)::u -> if n = name then v1 else find name u

let add (name : Env.addr) (v : Value.t) (ls : t) : t =
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