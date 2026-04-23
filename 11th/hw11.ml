(* hw11 *)

(*interp_expr*)

let rec interp_expr (exp : Ast.expr) (ls : Store.t) : Store.value = 
  match exp with
  | Ast.Num n -> Store.NumV n
  | Ast.Bool b -> Store.BoolV b
  | Ast.Name id -> Store.find id ls
  | Ast.Add (e1, e2) -> 
    let n1 = (interp_expr e1 ls) in
    let n2 = (interp_expr e2 ls) in
    (match n1, n2 with
    | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 + n2)
    | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp_expr exp))
  | Ast.Sub (e1, e2) ->
    let n1 = (interp_expr e1 ls) in
    let n2 = (interp_expr e2 ls) in
    (match n1, n2 with
    | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 - n2)
    | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp_expr exp))
  | Ast.Lt (e1, e2) ->
    let n1 = (interp_expr e1 ls) in
    let n2 = (interp_expr e2 ls) in
    (match n1, n2 with
    | Store.NumV n1, Store.NumV n2 -> Store.BoolV (n1 < n2)
    | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp_expr exp))
  | Ast.Gt (e1, e2) ->
    let n1 = (interp_expr e1 ls) in
    let n2 = (interp_expr e2 ls) in
    (match n1, n2 with
    | Store.NumV n1, Store.NumV n2 -> Store.BoolV (n1 > n2)
    | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp_expr exp))
    | Ast.Eq (e1, e2) ->
      let v1 = interp_expr e1 ls in
      let v2 = interp_expr e2 ls in
      (match v1, v2 with
      | Store.NumV n1, Store.NumV n2 -> Store.BoolV (n1 = n2)
      | Store.BoolV b1, Store.BoolV b2 -> Store.BoolV (b1 = b2)
      | Store.NumV _, Store.BoolV _ -> Store.BoolV false
      | Store.BoolV _, Store.NumV _ -> Store.BoolV false)
      | Ast.And (e1, e2) ->
    let v1 = interp_expr e1 ls in
    let v2 = interp_expr e2 ls in
    (match v1, v2 with
    | Store.BoolV b1, Store.BoolV b2 -> Store.BoolV (b1 && b2)
    | _, _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp_expr exp))
  | Ast.Or (e1, e2) ->
    let v1 = interp_expr e1 ls in
    let v2 = interp_expr e2 ls in
    (match v1, v2 with
    | Store.BoolV b1, Store.BoolV b2 -> Store.BoolV (b1 || b2)
    | _, _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp_expr exp))
(* interp_stmt *)
(* s1bar, s2bar : statement list *)

let rec interp_stmt_bar (s : Ast.stmt list) (ls : Store.t) : Store.t =
  match s with
  | [] -> ls
  | h::t -> interp_stmt_bar t (interp_stmt h ls)
and interp_stmt (st : Ast.stmt) (ls : Store.t) : Store.t = 
  match st with
  | Ast.AssignStmt (name, e) ->
    let v = interp_expr e ls in
    Store.add name v ls
  | Ast.IfStmt (e, s1bar, s2bar) ->
    let v1 = interp_expr e ls in
      (match v1 with
      | Store.BoolV true -> interp_stmt_bar s1bar ls
      | Store.BoolV false -> interp_stmt_bar s2bar ls
      | _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp_expr e))
  | Ast.LoopStmt (e, sbar) ->
    let v1 = interp_expr e ls in
      (match v1 with
      | Store.BoolV true -> 
          let new_ls = interp_stmt_bar sbar ls in
          interp_stmt (Ast.LoopStmt (e, sbar)) new_ls
      | Store.BoolV false -> ls
      | _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp_expr e))

(*interp_prog*)

let interp_prog (p : Ast.prog) : Store.t =
  match p with
  | Ast.Program s -> interp_stmt_bar s []