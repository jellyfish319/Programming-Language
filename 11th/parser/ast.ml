module F = Format

type expr = Num of int 
  | Name of string 
  | Bool of bool 
  | Add of expr * expr
  | Sub of expr * expr 
  | Lt of expr * expr 
  | Gt of expr * expr 
  | Eq of expr * expr
  | And of expr * expr 
  | Or of expr * expr

type stmt = AssignStmt of string * expr
  | IfStmt of expr * stmt list * stmt list
  | LoopStmt of expr * stmt list

type prog = Program of stmt list

let rec pp_expr fmt e = 
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Name id -> F.fprintf fmt "%s" id
  | Bool b -> F.fprintf fmt "%b" b
  | Add (e1, e2) -> F.fprintf fmt "%a + %a" pp_expr e1 pp_expr e2
  | Sub (e1, e2) -> F.fprintf fmt "%a - %a" pp_expr e1 pp_expr e2
  | Lt (e1, e2) -> F.fprintf fmt "%a < %a" pp_expr e1 pp_expr e2
  | Gt (e1, e2) -> F.fprintf fmt "%a > %a" pp_expr e1 pp_expr e2
  | Eq (e1, e2) -> F.fprintf fmt "%a == %a" pp_expr e1 pp_expr e2
  | And (e1, e2) -> F.fprintf fmt "%a && %a" pp_expr e1 pp_expr e2
  | Or (e1, e2) -> F.fprintf fmt "%a || %a" pp_expr e1 pp_expr e2

let rec pp_stmt fmt s = 
  match s with 
  | AssignStmt (x, e) -> F.fprintf fmt "%s = %a;" x pp_expr e
  | IfStmt (e, sl1, sl2) -> F.fprintf fmt "if %a { %a } else { %a }" pp_expr e pp_stmt_list sl1 pp_stmt_list sl2
  | LoopStmt (e, sl) -> F.fprintf fmt "while(%a) { %a }" pp_expr e pp_stmt_list sl

and pp_stmt_list fmt sl = 
  F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") 
    (fun fmt s -> F.fprintf fmt "%a" pp_stmt s)
    fmt
    sl

let pp fmt p =
  match p with
  | Program sl -> pp_stmt_list fmt sl
