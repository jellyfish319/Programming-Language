let add_numv (numv1:Store.value) (numv2:Store.value) =
  match numv1, numv2 with
  | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 + n2)

let sub_numv (numv1:Store.value) (numv2:Store.value) =
  match numv1, numv2 with
  | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 - n2) 

let rec interp_expr (fs : Fstore.t) (ls : Store.t) (exp : Ast.expr) : Store.value =
  match exp with
  | Ast.Num n -> Store.NumV n
  | Ast.Id x -> Store.find x ls
  | Ast.Add (e1, e2) -> add_numv (interp_expr fs ls e1) (interp_expr fs ls e2)
  | Ast.Sub (e1, e2) -> sub_numv (interp_expr fs ls e1) (interp_expr fs ls e2)
  (*Let x = e1 in e2 : e1의 값(=n2)을 x에 넣고 그 메모리에서 e2를 계산한 값(=n2)을 출력*)
  | Ast.LetIn (name, e1, e2) ->
    let n1 = (interp_expr fs ls e1) in
    let n2 = (interp_expr fs (Store.add name n1 ls) e2) in n2
  (*
  Call
  step1 우선 e11~e1k를 계산해서 얻음
  step2 함수를 find
  step3 찾은 함수에 얻은 값들을 넣어서 계산
  *)
  | Ast.Call (name, e) -> 
    let (param_list, e2) = Fstore.find name fs in
    if List.length param_list <> List.length e then failwith "Unmatched number of arguments" else
      let arg_values = List.map (interp_expr fs ls) e in
      let new_ls = List.combine param_list arg_values in
    interp_expr fs new_ls e2

let interp_def (Ast.FunDef (name, param_list, exp)) (mem: Fstore.t) : Fstore.t =
  Fstore.add name param_list exp mem

let rec def_lambda ls acc =
  match ls with
  | [] -> acc
  | e1::e -> def_lambda e (interp_def e1 acc)

let interp_prog (Ast.Prog (fun_list, exp)) : Store.value =
  let lambda = def_lambda fun_list [] in
  interp_expr lambda [] exp

