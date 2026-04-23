let%test _ = Hw14_interp.interp_prog (ParserMain.parse "def x : int array = int[3](0);") = ([("x", 0)], [(0, Value.ArrayV (3, [Value.NumV 0;Value.NumV 0;Value.NumV 0]))])

let%test _ = Hw14_tc.tc_prog (ParserMain.parse "def x: int array = int[3](0);") = ([], [("x", Ast.TArray (Ast.TInt))])

let%test _ = Hw14_interp.interp_prog (ParserMain.parse "def x : int * bool = (1, true);") = ([("x", 0)], [(0, Value.TupleV (Value.NumV 1, Value.BoolV true))])

let%test _ = Hw14_tc.tc_prog (ParserMain.parse "def x : int * bool = (1, true);") = ([], [("x", Ast.TTuple (Ast.TInt, Ast.TBool))])

let%test _ =
try
  let _ = Hw14_tc.tc_prog (ParserMain.parse "def x : int array = int[3](0); x[1] = true;") in false
with
| Failure msg -> msg = "[Ill-typed] x[1] = true;"
| _ -> false

let%test _ =
try
  let _ = Hw14_tc.tc_prog (ParserMain.parse "def x : int = 0; *x = x._1;") in false
with
| Failure msg -> msg = "[Ill-typed] x"
| _ -> false
