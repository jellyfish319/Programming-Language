let%test _ = Hw11.interp_prog (ParserMain.parse "x = 3;") = [("x", Store.NumV 3)]

let%test _ =
try
  let _ = Hw11.interp_prog (ParserMain.parse "x = 3;\ny = 4;\nif x {\nx = x - 2;\n}") in false
with
| Failure msg -> msg = "Not a bool: x"

let%test _ =
try
  let _ = Hw11.interp_prog (ParserMain.parse "x = 3;\ny = true;\nz = x && y;") in false
with
| Failure msg -> msg = "Not a bool: x && y"

let%test _ =
try
  let _ = Hw11.interp_prog (ParserMain.parse "x = 3;\ny = true;\nz = x || y;") in false
with
| Failure msg -> msg = "Not a bool: x || y"

let%test _ =
try
  let _ = Hw11.interp_expr (Ast.Add (Ast.Num 1, Ast.Bool true)) [] in false
with
| Failure msg -> msg = "Not a number: 1 + true"

let%test _ = Hw11.interp_expr (Ast.Eq (Ast.Num 1, Ast.Bool true)) [] = Store.BoolV false
let%test _ = Hw11.interp_expr (Ast.Eq (Ast.Bool true, Ast.Num 1)) [] = Store.BoolV false
let%test _ = Hw11.interp_expr (Ast.Eq (Ast.Num 1, Ast.Num 1)) [] = Store.BoolV true
let%test _ = Hw11.interp_expr (Ast.Eq (Ast.Bool true, Ast.Bool true)) [] = Store.BoolV true

(* 기본 데이터 타입과 변수 테스트 *)
let%test _ = Hw11.interp_expr (Ast.Num 42) [] = Store.NumV 42
let%test _ = Hw11.interp_expr (Ast.Bool false) [] = Store.BoolV false
let%test _ = 
  let store = [("x", Store.NumV 10)] in
  Hw11.interp_expr (Ast.Name "x") store = Store.NumV 10

(* 산술 연산 테스트 *)
let%test _ = Hw11.interp_expr (Ast.Sub (Ast.Num 10, Ast.Num 3)) [] = Store.NumV 7
let%test _ = 
try
  let _ = Hw11.interp_expr (Ast.Sub (Ast.Num 5, Ast.Bool true)) [] in false
with
| Failure msg -> msg = "Not a number: 5 - true"

(* 비교 연산 테스트 *)
let%test _ = Hw11.interp_expr (Ast.Lt (Ast.Num 3, Ast.Num 5)) [] = Store.BoolV true
let%test _ = Hw11.interp_expr (Ast.Lt (Ast.Num 5, Ast.Num 5)) [] = Store.BoolV false
let%test _ = Hw11.interp_expr (Ast.Gt (Ast.Num 4, Ast.Num 4)) [] = Store.BoolV false
let%test _ = Hw11.interp_expr (Ast.Gt (Ast.Num 8, Ast.Num 4)) [] = Store.BoolV true
let%test _ =
try
  let _ = Hw11.interp_expr (Ast.Lt (Ast.Bool true, Ast.Num 3)) [] in false
with
| Failure msg -> msg = "Not a number: true < 3"
let%test _ =
try
  let _ = Hw11.interp_expr (Ast.Gt (Ast.Bool true, Ast.Num 3)) [] in false
with
| Failure msg -> msg = "Not a number: true > 3"

(* 논리 연산 테스트 *)
let%test _ = Hw11.interp_expr (Ast.And (Ast.Bool true, Ast.Bool true)) [] = Store.BoolV true
let%test _ = Hw11.interp_expr (Ast.And (Ast.Bool false, Ast.Bool true)) [] = Store.BoolV false
let%test _ = Hw11.interp_expr (Ast.Or (Ast.Bool false, Ast.Bool true)) [] = Store.BoolV true
let%test _ = Hw11.interp_expr (Ast.Or (Ast.Bool false, Ast.Bool false)) [] = Store.BoolV false
let%test _ = Hw11.interp_expr (Ast.Or (Ast.Bool true, Ast.Bool false)) [] = Store.BoolV true

(* 여러 문장 실행 테스트 *)
let%test _ = Hw11.interp_prog (ParserMain.parse "x = 3; y = 4; z = x + y;") = 
             [("z", Store.NumV 7); ("y", Store.NumV 4); ("x", Store.NumV 3)]

(* If-Else 테스트 *)
let%test _ = Hw11.interp_prog (ParserMain.parse "x = 3; if true { x = 7; } else { x = 10; }") = 
             [("x", Store.NumV 7)]
let%test _ = Hw11.interp_prog (ParserMain.parse "x = 3; if false { x = 7; } else { x = 10; }") = 
             [("x", Store.NumV 10)]

(* Loop 테스트 *)
let%test _ = Hw11.interp_prog (ParserMain.parse "x = 5; y = 0; while(x > 0) { y = y + 1; x = x - 1; }") = 
             [("x", Store.NumV 0); ("y", Store.NumV 5)]
let%test _ = Hw11.interp_prog (ParserMain.parse "x = 0; while(false) { x = 42; }") = 
             [("x", Store.NumV 0)]
let%test _ =
try
  let _ = Hw11.interp_prog (ParserMain.parse "x = 0; while(42) { x = 1; }") in false
with
| Failure msg -> msg = "Not a bool: 42"

(* 정의되지 않은 변수 참조 테스트 *)
let%test _ =
try
  let _ = Hw11.interp_expr (Ast.Name "undefined") [] in false
with
| Failure msg -> msg = "Free identifier: undefined"

(* 복잡한 중첩 표현식 테스트 *)
let%test _ = Hw11.interp_expr 
             (Ast.Add (Ast.Num 1, 
                      Ast.Sub (Ast.Num 5, 
                              Ast.Add (Ast.Num 2, Ast.Num 1)))) [] = Store.NumV 3

let%test _ = Hw11.interp_expr
             (Ast.And (Ast.Lt (Ast.Num 3, Ast.Num 5),
                      Ast.Or (Ast.Bool false, Ast.Eq (Ast.Num 1, Ast.Num 1)))) [] = Store.BoolV true