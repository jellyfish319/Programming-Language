(* hw13 *)
(*interp_expr*)

let rec interp_expr (exp : Ast.expr) (ls : Env.t * Mem.t) : Value.t =
  match exp with
  | Ast.Num n -> Value.NumV n
  | Ast.Bool b -> Value.BoolV b
  | Ast.Id id -> 
    let (env, mem) = ls in
    let addr = Env.find id env in
    Mem.find addr mem
  | Ast.Ref r -> 
    let (env, _) = ls in
    let addr = Env.find r env in
    Value.AddrV addr
  | Ast.Deref e ->
    let (_, mem) = ls in
    let addr_val = interp_expr e ls in
    (match addr_val with
      | Value.AddrV addr -> Mem.find addr mem
      | _ -> failwith (Format.asprintf "Not an address: %a" Ast.pp_expr e))
  | Ast.Add (e1, e2) ->
    let n1 = (interp_expr e1 ls) in
    let n2 = (interp_expr e2 ls) in
    (match n1, n2 with
    | Value.NumV n1, Value.NumV n2 -> Value.NumV (n1 + n2)
    | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp_expr exp))
  | Ast.Sub (e1, e2) ->
    let n1 = (interp_expr e1 ls) in
    let n2 = (interp_expr e2 ls) in
    (match n1, n2 with
    | Value.NumV n1, Value.NumV n2 -> Value.NumV (n1 - n2)
    | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp_expr exp))
  | Ast.Lt (e1, e2) ->
    let n1 = (interp_expr e1 ls) in
    let n2 = (interp_expr e2 ls) in
    (match n1, n2 with
    | Value.NumV n1, Value.NumV n2 -> Value.BoolV (n1 < n2)
    | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp_expr exp))
  | Ast.Gt (e1, e2) ->
    let n1 = (interp_expr e1 ls) in
    let n2 = (interp_expr e2 ls) in
    (match n1, n2 with
    | Value.NumV n1, Value.NumV n2 -> Value.BoolV (n1 > n2)
    | _, _ -> failwith (Format.asprintf "Not a number: %a" Ast.pp_expr exp))
  | Ast.Eq (e1, e2) ->
      let v1 = interp_expr e1 ls in
      let v2 = interp_expr e2 ls in
      (match v1, v2 with
      | Value.NumV n1, Value.NumV n2 -> Value.BoolV (n1 = n2)
      | Value.BoolV b1, Value.BoolV b2 -> Value.BoolV (b1 = b2)
      | Value.AddrV a1, Value.AddrV a2 -> Value.BoolV (a1 = a2)
      | Value.NumV _, Value.BoolV _ -> Value.BoolV false
      | Value.BoolV _, Value.NumV _ -> Value.BoolV false
      | Value.AddrV _, Value.BoolV _ -> Value.BoolV false
      | Value.BoolV _, Value.AddrV _ -> Value.BoolV false
      | Value.AddrV _, Value.NumV _ -> Value.BoolV false
      | Value.NumV _, Value.AddrV _ -> Value.BoolV false
      )
  | Ast.And (e1, e2) ->
    let v1 = interp_expr e1 ls in
    let v2 = interp_expr e2 ls in
    (match v1, v2 with
    | Value.BoolV b1, Value.BoolV b2 -> Value.BoolV (b1 && b2)
    | _, _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp_expr exp))
  | Ast.Or (e1, e2) ->
    let v1 = interp_expr e1 ls in
    let v2 = interp_expr e2 ls in
    (match v1, v2 with
    | Value.BoolV b1, Value.BoolV b2 -> Value.BoolV (b1 || b2)
    | _, _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp_expr exp))

(*interp_fundef*)

let interp_fundef (f : Ast.def) (sigma : Fstore.t) : Fstore.t = (* fun f(xbar){sbar} *)
  match f with
  | Ast.FunDef (_, name, args, stmts) ->
    let rec first_elements = function
  | [] -> []
  | (a, _) :: rest -> a :: first_elements rest
  in
    let params = first_elements args in
    let new_sigma = Fstore.add name params stmts sigma in
    new_sigma
    

(* interp_stmt *)
(* s1bar, s2bar : statement list *)
let rec interp_stmt_bar (stmts : Ast.stmt list) (sigma : Fstore.t) (ls : Env.t * Mem.t) : Env.t * Mem.t =
  match stmts with
  | [] -> ls  (* 빈 리스트면 현재 상태 반환 *)
  | stmt :: rest ->
      let new_ls = interp_stmt stmt sigma ls in
      interp_stmt_bar rest sigma new_ls
and interp_stmt (stmt : Ast.stmt) (sigma : Fstore.t) (ls : Env.t * Mem.t) : Env.t * Mem.t =
  match stmt with
  | Ast.DefStmt (_, str, exp) ->
    let (env, mem) = ls in
    let v = interp_expr exp ls in
    let addr = AddrManager.new_addr () in (* 새로운 주소를 만듦 *)
    let new_env = Env.add str addr env in
    let new_mem = Mem.add addr v mem in
    (new_env, new_mem)
  | Ast.StoreStmt (exp1, exp2) -> (* e1 = *e2 statement *)
    let (env, mem) = ls in
    let addr_val = interp_expr exp1 ls in
    let value = interp_expr exp2 ls in
    (match addr_val with
      | Value.AddrV addr -> 
          let new_mem = Mem.add addr value mem in
          (env, new_mem)
      | _ -> failwith (Format.asprintf "Not an address: %a" Ast.pp_expr exp1))
  | Ast.IfStmt (exp, s1bar, s2bar) ->
    let (env, _) = ls in
    let cond = interp_expr exp ls in
    (match cond with
      | Value.BoolV true ->
        let (_, mem1) = interp_stmt_bar s1bar sigma ls in (env, mem1)
      | Value.BoolV false ->
        let (_, mem2) = interp_stmt_bar s2bar sigma ls in (env, mem2)
      | _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp_expr exp)
      )
  | Ast.LoopStmt (exp, s1bar) ->
    let (env, mem) = ls in
    let cond = interp_expr exp ls in
    (match cond with
      | Value.BoolV true ->
        let (_, mem1) = interp_stmt_bar s1bar sigma ls in
        interp_stmt (Ast.LoopStmt (exp, s1bar)) sigma (env, mem1)
      | Value.BoolV false -> (env, mem)
      | _ -> failwith (Format.asprintf "Not a bool: %a" Ast.pp_expr exp)
      )
  | Ast.ReturnStmt (exp) ->
    let (env, mem) = ls in
    let value = interp_expr exp ls in
    (* sigma, m'[sigma(x) mapsto m'(alpha@ret)] *)
    (env, (Mem.add AddrManager.ret_addr value mem))
  (* Ast.CallStmt 구현 *)
  | Ast.CallStmt (str1, str2, expbar) -> (* x=f(ebar) *)
    (* 1. 함수 찾기 *)
    let (env, mem) = ls in
    let (formal_args, body_stmts) = Fstore.find str2 sigma in
    
    (* 2. 인수 개수 확인 *)
    let actual_args = List.length expbar in
    let formal_params = List.length formal_args in
    if actual_args <> formal_params then
      failwith (Format.sprintf "The number of arguments not matched: actual %d, expected %d" 
                actual_args formal_params);
    (* 3. 실제 인수 값들 계산 *)
    let arg_values = List.map (fun exp -> interp_expr exp ls) expbar in
    
    (* 4. 함수용 새 환경 생성 (매개변수 바인딩) *)
    let func_env = List.fold_left2 (fun acc_env param_name _ ->
      let new_addr = AddrManager.new_addr () in
      Env.add param_name new_addr acc_env
    ) Env.empty formal_args arg_values in
    
    (* 5. 함수용 새 메모리 생성 (매개변수 값 저장) *)
    let func_mem = List.fold_left2 (fun acc_mem param_name arg_value ->
      let param_addr = Env.find param_name func_env in
      Mem.add param_addr arg_value acc_mem
    ) mem formal_args arg_values in
    
    (* 6. 함수 본문 실행 *)
    let (_, final_mem) = interp_stmt_bar body_stmts sigma (func_env, func_mem) in
    
    (* 7. 반환값 처리 *)
    let return_value = Mem.find AddrManager.ret_addr final_mem in
    
    (* 8. 결과 변수에 반환값 저장 *)
    let result_addr = Env.find str1 env in
    let new_mem = Mem.add result_addr return_value mem in
    (env, new_mem) (* 기존 환경 + 바뀐 메모리 *)
  | Ast.InputStmt var =
    let (env, mem) = ls in
    let n = read_int () in
    let x_addr = Env.find input var in
    let new_mem = Mem.add x_addr (Value.NumV n) mem in
    (env, new_mem)

(*interp_prog*)

let interp_prog (p : Ast.prog) : Env.t * Mem.t =
  match p with
  | Ast.Program (funbar, stmtbar) ->
    (* 1. 주소 초기화 *)
    let _ = AddrManager.init () in
    
    (* 2. 초기 상태 *)
    let initial_env = Env.empty in
    let initial_mem = Mem.empty in
    let initial_fstore = Fstore.empty in
    
    (* 3. 모든 함수 정의를 함수 저장소에 등록 *)
    let fstore_list = List.fold_left (fun acc_sigma fundef ->
      interp_fundef fundef acc_sigma
    ) initial_fstore funbar in
    
    (* 4. 메인 문장들 실행 *)
    interp_stmt_bar stmtbar fstore_list (initial_env, initial_mem)
