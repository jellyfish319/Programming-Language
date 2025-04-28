type t = (string * (string list * Ast.expr)) list

let rec find (n : string) (fs:t) : string list * Ast.expr =
  match fs with
  | [] -> failwith ("Undefined function: " ^ n)
  | (name, (parameter, body))::u -> if n = name then (parameter, body) else find n u


let add (fn:string) (p:string list) (f:Ast.expr) (fs:t) : t =
  let rec rec_remove ls acc =
    match ls with
    | [] -> acc
    | (name, (params, body))::u ->
        if fn = name then
          rec_remove u acc
        else
          rec_remove u ((name, (params, body))::acc)
  in
  (fn, (p, f)) :: List.rev (rec_remove fs [])

(*
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

let rec find (name : string) (ls : t) : value =
  match ls with
  | [] -> failwith ("Free identifier: " ^ name)
  | (n, v1)::u -> if n = name then v1 else find name u
*)