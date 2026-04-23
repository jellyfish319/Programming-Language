module F = Format

type typ =
  | TUnit
  | TInt
  | TBool
  | TPtr of typ
  | TArrow of typ * typ

type expr = 
  | Num of int 
  | Ref of string 
  | Deref of expr
  | Id of string 
  | Bool of bool 
  | Add of expr * expr
  | Sub of expr * expr 
  | Lt of expr * expr 
  | Gt of expr * expr 
  | Eq of expr * expr
  | And of expr * expr 
  | Or of expr * expr

type stmt = 
  | DefStmt of typ * string * expr
  | StoreStmt of expr * expr
  | IfStmt of expr * stmt list * stmt list
  | LoopStmt of expr * stmt list
  | ReturnStmt of expr
  | CallStmt of string * string * expr list
  | InputStmt of string

type def = FunDef of typ * string * (typ * string) list * stmt list
type prog = Program of def list * stmt list

let rec pp_typ fmt e =
  match e with
  | TUnit -> F.fprintf fmt "unit" 
  | TInt -> F.fprintf fmt "int" 
  | TBool -> F.fprintf fmt "bool" 
  | TPtr t -> F.fprintf fmt "%a*" pp_typ t
  | TArrow (t1, t2) -> F.fprintf fmt "%a -> %a" pp_typ t1 pp_typ t2

let rec pp_expr fmt e = 
  match e with
  | Num n -> F.fprintf fmt "%d" n
  | Id id -> F.fprintf fmt "%s" id
  | Ref id -> F.fprintf fmt "&%s" id
  | Deref e -> F.fprintf fmt "*%a" pp_expr e
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
  | DefStmt (t, x, e) -> F.fprintf fmt "%a %s = %a;" pp_typ t x pp_expr e
  | StoreStmt (e1, e2) -> F.fprintf fmt "*%a = %a;" pp_expr e1 pp_expr e2
  | IfStmt (e, sl1, sl2) -> F.fprintf fmt "if %a { %a } else { %a }" pp_expr e pp_stmt_list sl1 pp_stmt_list sl2
  | LoopStmt (e, sl) -> F.fprintf fmt "while(%a) { %a }" pp_expr e pp_stmt_list sl
  | ReturnStmt e -> F.fprintf fmt "return %a;" pp_expr e
  | InputStmt x -> F.fprintf fmt "%s = input();" x
  | CallStmt (x, f, el) -> F.fprintf fmt "%s = %s(%a);" x f 
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") (fun fmt e -> F.fprintf fmt "%a" pp_expr e)) el

and pp_stmt_list fmt sl = 
  F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n") 
    (fun fmt s -> F.fprintf fmt "%a" pp_stmt s)
    fmt
    sl

let pp_def fmt d = 
  match d with
  | FunDef (t, f, pl, sl) ->
      F.fprintf fmt "%a %s(%a) {\n%a\n}"
      pp_typ t
      f
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") (fun fmt (t, p) -> F.fprintf fmt "%a %s" pp_typ t p)) pl
      pp_stmt_list sl

let pp_def_list fmt dl =
  F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "\n")
  (fun fmt d -> F.fprintf fmt "%a" pp_def d)
  fmt
  dl

let pp fmt p =
  match p with
  | Program (dl, sl) -> (pp_def_list fmt dl; pp_stmt_list fmt sl)
