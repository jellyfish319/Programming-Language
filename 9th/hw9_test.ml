let%test _ = Hw9.interp (ParserMain.parse "1 + 2") [] = Store.NumV 3
let%test _ = Hw9.interp (ParserMain.parse "1 - 2") [] = Store.NumV (-1)

let%test _ =
try
  let _ = Hw9.interp (ParserMain.parse "1 + true") [] in false
with
|Failure msg -> msg = "Not a number: 1 + true"

let%test _ =
try
  let _ = Hw9.interp (ParserMain.parse "1 - true") [] in false
with
|Failure msg -> msg = "Not a number: 1 - true"

let%test _ =
try
  let _ = Hw9.interp (ParserMain.parse "let x = 1 in x + true") [] in false
with
|Failure msg -> msg = "Not a number: x + true"
let%test _ = Hw9.interp (ParserMain.parse "let x = 1 in x + 2") [] = Store.NumV 3

let%test _ =
Hw9.interp (ParserMain.parse "let x = 1 in (fun y -> x + y) 2") [] = Store.NumV 3

let%test _ = Hw9.interp (ParserMain.parse "3 < 2") [] = Store.BoolV false
let%test _ = Hw9.interp (ParserMain.parse "1 < 2") [] = Store.BoolV true
let%test _ = Hw9.interp (ParserMain.parse "if 1 < 2 then 3 else 4") [] = Store.NumV 3
let%test _ = Hw9.interp (ParserMain.parse "if 3 < 2 then 3 else 4") [] = Store.NumV 4

let%test _ =
  try
    let _ = Hw9.interp (ParserMain.parse "true < 2") [] in false
  with
  |Failure msg -> msg = "Not a number: true < 2"

let%test _ =
  try
    let _ = Hw9.interp (ParserMain.parse "2 < true") [] in false
  with
  |Failure msg -> msg = "Not a number: 2 < true"

let%test _ =
  try
    let _ = Hw9.interp (ParserMain.parse "if 1 then 1 else 2") [] in false
  with
  |Failure msg -> msg = "Not a bool: 1"

let%test _ =
  try
    let _ = Hw9.interp (ParserMain.parse "1 1") [] in false
  with
  |Failure msg -> msg = "Not a function: 1"

let%test _ =
  try
    let _ = Hw9.interp (ParserMain.parse "x 1") [] in false
  with
  |Failure msg -> msg = "Free identifier: x"

