let%test _ = Fstore.add "add" ["a"; "b"] (Add ((Id "a"), (Id "b"))) []  = ["add", (["a"; "b"], (Add ((Id "a"), (Id "b"))))]
let%test _ = Fstore.find "add" ["add", (["a"; "b"], (Add ((Id "a"), (Id "b"))))]  = (["a"; "b"], (Add ((Id "a"), (Id "b"))))
let%test _ = try
  let _ = Fstore.find "add" [] in
  false
  with
  |Failure msg -> msg = "Undefined function: add"
  |_ -> false
let%test _ = Hw6.interp_expr [("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))] [] (Call ("add", [Num 2;Num 3])) = NumV 5
let%test _ = Hw6.interp_def (Ast.FunDef ("add", ["a"; "b"], (Add ((Id "a"), (Id "b"))))) [] = [("add", (["a"; "b"], (Add ((Id "a"), (Id "b")))))]
let%test _ = Hw6.interp_prog (Ast.Prog ([], (Ast.Add (Num 3, Num 1)))) = NumV 4

let%test _ =
  let fs = Hw6.interp_def (Ast.FunDef ("zero", [], Num 42)) [] in
  Hw6.interp_expr fs [] (Call ("zero", [])) = NumV 42

let%test _ =
  let prog =
    Ast.Prog (
      [Ast.FunDef ("zero", [], Num 100)],
      Call ("zero", [])
    )
  in
  Hw6.interp_prog prog = NumV 100

let%test _ =
  let prog =
    Ast.Prog (
      [Ast.FunDef ("unused", ["x"], Sub (Id "x", Num 1))],
      Num 999
    )
  in
  Hw6.interp_prog prog = NumV 999

let%test _ =
  try
    let fs = Hw6.interp_def (Ast.FunDef ("f", ["x"; "y"], Add (Id "x", Id "y"))) [] in
    let _ = Hw6.interp_expr fs [] (Call ("f", [Num 1])) in
    false
  with Failure msg -> msg = "Unmatched number of arguments" | _ -> false

let%test _ =
  let fs1 = Hw6.interp_def (Ast.FunDef ("add1", ["x"], Add (Id "x", Num 1))) [] in
  let fs2 = Hw6.interp_def (Ast.FunDef ("add2", ["x"], Call ("add1", [Call ("add1", [Id "x"])]))) fs1 in
  Hw6.interp_expr fs2 [] (Call ("add2", [Num 3])) = NumV 5

let%test _ =
  Hw6.interp_expr [] [] (LetIn ("x", Num 10, Add (Id "x", Num 5))) = NumV 15

let%test _ =
  let fs = Hw6.interp_def (Ast.FunDef ("inc2", ["x"],
                LetIn ("y", Add (Id "x", Num 1), Add (Id "y", Num 1)))) [] in
  Hw6.interp_expr fs [] (Call ("inc2", [Num 3])) = NumV 5

let%test _ =
  Hw6.interp_expr [] [] (
    LetIn ("a", Num 1,
      LetIn ("b", Num 2,
        Add (Id "a", Id "b")))) = NumV 3
let%test _ = 
  Hw6.interp_prog (ParserMain.parse "def f1 x y = x + y endef def f2 x y = x - y endef f1(3, 4) + f2(4, 7)") = NumV 4