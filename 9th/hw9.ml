let add_numv expr (numv1:Store.value) (numv2:Store.value) =
  match numv1, numv2 with
  | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 + n2)
  | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp expr)

let sub_numv expr (numv1:Store.value) (numv2:Store.value) =
  match numv1, numv2 with
  | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 - n2) (*failwith (Format.asprintf “Not a function: %a” Ast.pp expr)*)
  | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp expr)

let rec interp (exp : Ast.expr) (ls : Store.t) : Store.value =
  match exp with
  | Ast.Num n -> Store.NumV n
  | Ast.Bool b -> Store.BoolV b
  | Ast.Id x -> Store.find x ls
  | Ast.Add (e1, e2) -> add_numv exp (interp e1 ls) (interp e2 ls)
  | Ast.Sub (e1, e2) -> sub_numv exp (interp e1 ls) (interp e2 ls)
  (*Let x = e1 in e2 : e1의 값(=n2)을 x에 넣고 그 메모리에서 e2를 계산한 값(=n2)을 출력*)
  | Ast.LetIn (name, e1, e2) ->
    let n1 = (interp e1 ls) in
    let n2 = (interp e2 (Store.add name n1 ls)) in n2
  (*e1 e2 : e1을 계산해서 함수가 나오고 e1(=n1)을 계산한 추상메모리 내에서 e2(=n2)를 계산한 값을 출력*)
  | Ast.App (e1, e2) -> 
    let v1 = interp e2 ls in
      (match interp e1 ls with
      | Store.ClosureV (par, e3, sigma) ->
          let v2 = interp e3 (Store.add par v1 sigma) in v2
      | _ -> failwith (Format.asprintf "Not a function: %a" Ast.pp e1))
  (*Lambda par e : par, 이름 없는 함수를 나타냄*)
  | Ast.Lambda (par, e) ->
    Store.ClosureV (par, e, ls) (*<λx.e, σ> 꼴?*)
  (*e1?e2:e3: e1을 계산해서 true면 e2를 계산하고 false면 e3을 계산한 값을 출력*)
  | Ast.Cond (e1, e2, e3) ->
    let v1 = interp e1 ls in
      (match v1 with
      | Store.BoolV true -> interp e2 ls
      | Store.BoolV false -> interp e3 ls
      | _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp e1))
  (*e1 < e2 : e1 < e2를 계산해서 true면 true, false면 false를 출력*)
  | Ast.LessThan (e1, e2) ->
    let v1 = interp e1 ls in
      (match v1 with
      | Store.NumV n1 ->
        let v2 = interp e2 ls in
          (match v2 with
          | Store.NumV n2 -> Store.BoolV (n1 < n2)
          | _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp exp))
      | _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp exp))