(* env.ml *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "owo = 1;") in false
with
| Failure msg -> msg = "Free identifier: owo"
| _ -> false

(* mem.ml *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "fun f() { } def opo = 0; opo = f();") in false
with
| Failure msg -> msg = "Free address: -1"
| _ -> false

(* fstore.ml *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "def omo = 0; omo = f();") in false
with
| Failure msg -> msg = "Unbound function: f"
| _ -> false

(* store, load *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "def x = 0; x = *1;") in false
with
| Failure msg -> msg = "Not an address: 1"
| _ -> false

let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "*1 = 1;") in false
with
| Failure msg -> msg = "Not an address: 1"
| _ -> false

(* argument mismatch *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "fun f (x) { return 1; } def y = 0; y = f();") in false
with
| Failure msg -> msg = "The number of arguments not matched: actual 0, expected 1"
| _ -> false

(* ===== ADDITIONAL COMPREHENSIVE TESTS ===== *)

(* Basic expressions - success cases *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 42;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.NumV 42 -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = true;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV true -> true
  | _ -> false

(* Arithmetic operations *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 3 + 4;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.NumV 7 -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 10 - 3;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.NumV 7 -> true
  | _ -> false

(* Comparison operations *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 5 < 10;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV true -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 10 > 5;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV true -> true
  | _ -> false

(* Equality operations *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 5; def y = (x == 5);") in
  let addr = Env.find "y" env in
  match Mem.find addr mem with
  | Value.BoolV true -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = true; def y = (x == false);") in
  let addr = Env.find "y" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

(* Cross-type equality *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = (5 == true);") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

(* Logical operations *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = true && false;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = true || false;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV true -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = true || true;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV true -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = false || false;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = false || true;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV true -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = false || false;") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false
(* Reference operations *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 42; def y = &x;") in
  let addr_x = Env.find "x" env in
  let addr_y = Env.find "y" env in
  match Mem.find addr_y mem with
  | Value.AddrV addr -> addr = addr_x
  | _ -> false

(* Store and Load operations - success cases *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 42; def y = &x; *y = 100; def z = x;") in
  let addr = Env.find "z" env in
  match Mem.find addr mem with
  | Value.NumV 100 -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 42; def y = &x; def z = 0; z = *y;") in
  let addr = Env.find "z" env in
  match Mem.find addr mem with
  | Value.NumV 42 -> true
  | _ -> false

(* If statements *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 0; if (true) { x = 42; }") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.NumV 42 -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 0; if (false) { x = 42; } else { x = 99; }") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.NumV 99 -> true
  | _ -> false

(* Loop statements *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def i = 0; while (i < 3) { i = i + 1; }") in
  let addr = Env.find "i" env in
  match Mem.find addr mem with
  | Value.NumV 3 -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 0; while (false) { x = 42; }") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.NumV 0 -> true
  | _ -> false

(* Function definition and call - success cases *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "fun add(a, b) { return a + b; } def x = 0; x = add(3, 4);") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.NumV 7 -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "fun get42() { return 42; } def x = 0; x = get42();") in
  let addr = Env.find "x" env in
  match Mem.find addr mem with
  | Value.NumV 42 -> true
  | _ -> false

(* Error cases for arithmetic operations *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "def x = true + 1;") in false
with
| Failure msg -> msg = "Not a number: true + 1"
| _ -> false

let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "def x = 5 - false;") in false
with
| Failure msg -> msg = "Not a number: 5 - false"
| _ -> false

let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "def x = true < 5;") in false
with
| Failure msg -> msg = "Not a number: true < 5"
| _ -> false

let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "def x = 5 > false;") in false
with
| Failure msg -> msg = "Not a number: 5 > false"
| _ -> false

(* Error cases for logical operations *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "def x = 5 && true;") in false
with
| Failure msg -> msg = "Not a bool: 5 && true"
| _ -> false

let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "def x = true || 42;") in false
with
| Failure msg -> msg = "Not a bool: true || 42"
| _ -> false

(* Error cases for if statements *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "if (42) { def x = 1; }") in false
with
| Failure msg -> msg = "Not a bool: 42"
| _ -> false

(* Error cases for loop statements *)
let%test _ = 
try
  let _ = Hw12.interp_prog (ParserMain.parse "while (42) { def x = 1; }") in false
with
| Failure msg -> msg = "Not a bool: 42"
| _ -> false

(* Address equality test *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 42; def y = &x; def z = &x; def result = (y == z);") in
  let addr = Env.find "result" env in
  match Mem.find addr mem with
  | Value.BoolV true -> true
  | _ -> false

(* Cross-type address equality test *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 42; def y = &x; def result = (y == 5);") in
  let addr = Env.find "result" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

(* Cross-type equality combinations *)
let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def result = (5 == true);") in
  let addr = Env.find "result" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def result = (false == 42);") in
  let addr = Env.find "result" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 10; def y = &x; def result = (y == true);") in
  let addr = Env.find "result" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 10; def y = &x; def result = (false == y);") in
  let addr = Env.find "result" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 10; def y = &x; def result = (y == 42);") in
  let addr = Env.find "result" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

let%test _ = 
  let (env, mem) = Hw12.interp_prog (ParserMain.parse "def x = 10; def y = &x; def result = (99 == y);") in
  let addr = Env.find "result" env in
  match Mem.find addr mem with
  | Value.BoolV false -> true
  | _ -> false

