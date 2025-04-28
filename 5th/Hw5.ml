let add_numv (numv1:Store.value) (numv2:Store.value) =
  match numv1, numv2 with
  | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 + n2)

let sub_numv (numv1:Store.value) (numv2:Store.value) =
  match numv1, numv2 with
  | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 - n2) 

let rec interp (exp : Ast.expr) (ls : Store.t) : Store.value =
  match exp with
  | Ast.Num n -> Store.NumV n
  | Ast.Id x -> Store.find x ls
  | Ast.Add (e1, e2) -> add_numv (interp e1 ls) (interp e2 ls)
  | Ast.Sub (e1, e2) -> sub_numv (interp e1 ls) (interp e2 ls)
  (*Let x = e1 in e2 : e1의 값(=n2)을 x에 넣고 그 메모리에서 e2를 계산한 값(=n2)을 출력*)
  | Ast.LetIn (name, e1, e2) ->
    let n1 = (interp e1 ls) in
    let n2 = (interp e2 (Store.add name n1 ls)) in n2
