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
  let _ = Hw12.interp_prog (ParserMain.parse "x = *1;") in false
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
