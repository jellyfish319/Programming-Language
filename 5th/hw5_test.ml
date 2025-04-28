(* --- add 테스트 --- *)
let%test _ = Store.add "x" (NumV 1) [] = [("x", NumV 1)]
let%test _ = Store.add "x" (NumV 2) [("x", NumV 1)] = [("x", NumV 2)]
let%test _ = Store.add "y" (NumV 3) [("x", NumV 2)] = [("y", NumV 3); ("x", NumV 2)]
let%test _ = Store.add "x" (NumV 2) [("l", NumV (-1)) ; ("z", NumV 7) ; ("y", NumV 6) ; ("x", NumV 1)] = [("x", NumV 2) ; ("l", NumV (-1)) ; ("z", NumV 7) ; ("y", NumV 6)]
let%test _ = Store.add "x" (NumV 2) [("l", NumV (-1)) ; ("z", NumV 7) ; ("x", NumV 1) ; ("u", NumV 0) ; ("y", NumV 6)] = [("x", NumV 2) ; ("l", NumV (-1)) ; ("z", NumV 7) ; ("u", NumV 0) ; ("y", NumV 6)]

(* --- find 테스트 --- *)
let%test _ = Store.find "x" [("x", NumV 42)] = NumV 42
let%test _ = Store.find "a" [("a", NumV 1); ("b", NumV 2)] = NumV 1
let%test _ =
  try let _ = Store.find "z" [("a", NumV 1); ("b", NumV 2)] in false
  with Failure msg -> msg = "Free identifier: z" | _ -> false

(* --- add_numv 테스트 --- *)
let%test _ = Hw5.add_numv (NumV 2) (NumV 3) = NumV 5
let%test _ = Hw5.add_numv (NumV 0) (NumV 0) = NumV 0
let%test _ = Hw5.add_numv (NumV (-1)) (NumV 1) = NumV 0

(* --- sub_numv 테스트 --- *)
let%test _ = Hw5.sub_numv (NumV 10) (NumV 3) = NumV 7
let%test _ = Hw5.sub_numv (NumV 4) (NumV 4) = NumV 0
let%test _ = Hw5.sub_numv (NumV 3) (NumV 10) = NumV (-7)

(* --- interp 테스트 --- *)
let%test _ = Hw5.interp (Num 99) [] = NumV 99
let%test _ = Hw5.interp (Add (Num 1, Num 2)) [] = NumV 3
let%test _ = Hw5.interp (Sub (Num 7, Num 2)) [] = NumV 5

let%test _ =
Hw5.interp (LetIn ("x", Num 3, Add (Id "x", Num 4))) [] = NumV 7

(* --- interp: integration test using parser --- *)
let%test _ =
Hw5.interp (ParserMain.parse "let x = 11 in let y = 1 in x + y") Store.empty = Store.NumV 12

let%test _ =
Hw5.interp (ParserMain.parse "let x = 11 + 7 in x + 22") Store.empty = NumV 40

let%test _ =
Hw5.interp (ParserMain.parse "let x = let y = 3 in y + 3 in x + 22") [] = NumV 28

let%test _ =
Hw5.interp (ParserMain.parse "let x = let y = let z = 9 in z + 9 in  y + 9 in x + 9") [] = NumV 36

let%test _ =
Hw5.interp (ParserMain.parse "let x = let x = let x = 8 + 2 in x + x in let y = x + 10 in x + y in x + x") [] = NumV 100