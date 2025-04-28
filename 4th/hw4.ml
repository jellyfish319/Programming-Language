let add_numv (numv1:Value.t) (numv2:Value.t) =
  match numv1, numv2 with
  | Value.NumV n1, Value.NumV n2 -> Value.NumV (n1 + n2)

let sub_numv (numv1:Value.t) (numv2:Value.t) =
  match numv1, numv2 with
  | Value.NumV n1, Value.NumV n2 -> Value.NumV (n1 - n2) 

let rec interp (exp : Ast.expr) : Value.t =
  match exp with
  | Ast.Num n -> Value.NumV n 
  | Ast.Add (e1, e2) -> 
    let n1 = interp e1 in
    let n2 = interp e2 in
    add_numv n1 n2
  | Ast.Sub (e1, e2) -> sub_numv (interp e1) (interp e2)
  