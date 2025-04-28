module F = Format

type expr = Num of int
  | Add of expr * expr
  | Sub of expr * expr

let rec pp fmt e = 
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp e1 pp e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp e1 pp e2

let rec pp_ast fmt e = 
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n
  | Add (e1, e2) -> F.fprintf fmt "(Add, %a, %a)" pp_ast e1 pp_ast e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub, %a, %a)" pp_ast e1 pp_ast e2
