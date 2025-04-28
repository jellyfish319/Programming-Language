module F = Format

type expr = Num of int
  | Id of string
  | Add of expr * expr
  | Sub of expr * expr
  | LetIn of string * expr * expr
  | Call of string * expr list

type fundef = FunDef of string * string list * expr

type prog = Prog of fundef list * expr

let rec pp_expr fmt e = 
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Id x -> F.fprintf fmt "%s" x
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp_expr e1 pp_expr e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp_expr e1 pp_expr e2
  | LetIn (x, e1, e2) -> F.fprintf fmt "let %s = %a in %a" x pp_expr e1 pp_expr e2
  | Call (x, elst) -> F.fprintf fmt "%s(%a)" x (F.pp_print_list 
      ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") 
      (fun fmt e -> F.fprintf fmt "%a" pp_expr e)) elst

let rec pp_expr_ast fmt e = 
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n
  | Id x -> F.fprintf fmt "(Id %s)" x
  | Add (e1, e2) -> F.fprintf fmt "(Add (%a, %a))" pp_expr_ast e1 pp_expr_ast e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub (%a, %a))" pp_expr_ast e1 pp_expr_ast e2
  | LetIn (x, e1, e2) -> F.fprintf fmt "(LetIn (%s, %a, %a))" x pp_expr_ast e1 pp_expr_ast e2
  | Call (x, elst) -> F.fprintf fmt "(Call (%s, [%a]))" x (F.pp_print_list 
      ~pp_sep:(fun fmt () -> F.fprintf fmt "; ") 
      (fun fmt e -> F.fprintf fmt "%a" pp_expr_ast e)) elst

let pp_fundef fmt (FunDef (x, xlst, e)) = 
  F.fprintf fmt "def %s %a = %a endef" x (F.pp_print_list 
      ~pp_sep:(fun fmt () -> F.fprintf fmt " ") 
      (fun fmt x -> F.fprintf fmt "%s" x)) xlst pp_expr e

let pp_fundef_ast fmt (FunDef (x, xlst, e)) = 
  F.fprintf fmt "(FunDef (%s, [%a], %a))" x (F.pp_print_list 
      ~pp_sep:(fun fmt () -> F.fprintf fmt "; ") 
      (fun fmt x -> F.fprintf fmt "%s" x)) xlst pp_expr_ast e

let pp_prog fmt p = 
  match p with
  | Prog ([], e) -> F.fprintf fmt "%a" pp_expr e
  | Prog (dlst, e) -> F.fprintf fmt "%a\n%a" (F.pp_print_list 
      ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") 
      (fun fmt d -> F.fprintf fmt "%a" pp_fundef d)) dlst pp_expr e

let pp_prog_ast fmt (Prog (dlst, e)) = 
  F.fprintf fmt "(Prog ([%a], %a))" (F.pp_print_list 
    ~pp_sep:(fun fmt () -> F.fprintf fmt "; ") 
    (fun fmt d -> F.fprintf fmt "%a" pp_fundef_ast d)) dlst pp_expr_ast e
