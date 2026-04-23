(*
type checking sequence
Gamma_l: 타이핑 환경 Gamma_l 에서 표현식 e 는 타입 t를 갖는다.
Gamma_g: 타이핑 환경 Gamma_g 에서 함수 정의 fd는 타입 t를 갖는다.

그렇다면 먼저 타이핑 환경을 생성?
parser가 parsing후 AST를 출력
타입 체커가 AST를 받아 타입을 입력받음
*)


let rec tc_expr (exp : Ast.expr) (gamma : LocalTEnv.t) : Type.t = (*local = (string * Ast.typ) list*)
  match exp with
  | Ast.Num _ -> Ast.TInt
  | Ast.Bool _ -> Ast.TBool
  | Ast.Id id ->
    let (name, _) = gamma in
    LocalTEnv.find name gamma
  | Ast.Ref r ->
    let (name, _) = gamma in
    let typ = LocalTEnv.find name gamma in
    Ast.TPtr typ
  | Ast.Deref e ->
    let t = tc_expr e gamma in
    (match t with
     | Ast.TRef t' -> t'
     | _ -> failwith (Format.asprintf “[Ill-typed] %a” Ast.pp_expr e))
  | Ast.Add (e1, e2) ->
    let t1 = tc_expr e1 gamma in
    let t2 = tc_expr e2 gamma in
    (match t1, t2 with
     | Ast.TInt, Ast.TInt -> Ast.TInt
     | _ -> failwith (Format.asprintf “[Ill-typed] %a” Ast.pp_expr exp))
  | Ast.Sub (e1, e2) ->
    let t1 = tc_expr e1 gamma in
    let t2 = tc_expr e2 gamma in
    (match t1, t2 with
     | Ast.TInt, Ast.TInt -> Ast.TInt
     | _ -> failwith (Format.asprintf “[Ill-typed] %a” Ast.pp_expr exp))
  | Ast.Lt (e1, e2) ->
    let n1 = (tc_expr e1 ls) in
    let n2 = (tc_expr e2 ls) in
    (match n1, n2 with
    | Ast.Tint _, Ast.Tint _ -> Ast.TBool
    | _, _ -> failwith (Format.asprintf “[Ill-typed] %a” Ast.pp_expr exp))
  | Ast.Gt (e1, e2) ->
    let n1 = (tc_expr e1 ls) in
    let n2 = (tc_expr e2 ls) in
    (match n1, n2 with
    | Ast.Tint _, Ast.Tint _ -> Ast.TBool
    | _, _ -> failwith (Format.asprintf “[Ill-typed] %a” Ast.pp_expr exp))
  | Ast.Eq (e1, e2) ->
    let v1 = tc_expr e1 gamma in
    let v2 = tc_expr e2 gamma in
    (match v1, v2 with
    | Ast.TInt, Ast.TInt -> Ast.TBool
    | Ast.TBool, Ast.TBool -> Ast.TBool
    | Ast.TPtr t1, Ast.TPtr t2 when t1 = t2 -> Ast.TBool
    | _ -> failwith (Format.asprintf “[Ill-typed] %a” Ast.pp_expr exp))
  | Ast.And (e1, e2) ->
    let v1 = tc_expr e1 gamma in
    let v2 = tc_expr e2 gamma in
    (match v1, v2 with
    | Ast.TBool, Ast.TBool -> Ast.TBool
    | _ -> failwith (Format.asprintf “[Ill-typed] %a” Ast.pp_expr exp))
  | Ast.Or (e1, e2) ->
    let v1 = tc_expr e1 gamma in
    let v2 = tc_expr e2 gamma in
    (match v1, v2 with
    | Ast.TBool, Ast.TBool -> Ast.TBool
    | _ -> failwith (Format.asprintf “[Ill-typed] %a” Ast.pp_expr exp))

let rec tc_stmt (stmt : Ast.stmt) (ls : GlobalTEnv.t) = 
  match stmt with
  | Ast.DefStmt (_, name, exp) ->
    let final_type = tc_expr exp ls in
    LocalTEnv.add name final_type ls
  | Ast.StoreStmt (exp1, exp2) -> (* e1 = *e2 statement *)
    let (env, mem) = ls in
    let addr_val = interp_expr exp1 ls in
    let value = interp_expr exp2 ls in
    (match addr_val with
      | Value.AddrV addr -> 
          let new_mem = Mem.add addr value mem in
          (env, new_mem)
      | _ -> failwith (Format.asprintf "Not an address: %a" Ast.pp_expr exp1))

let tc_prog (p : Ast.prog) : (GlobalTEnv.t * LocalTEnv.t) =
  match p with
  | Ast.Program (defbar, stmtbar) ->
    