(*Problem 1 - lex*)

type token = 
| IDENT of string
| NUMBER of string
| KW_LET
| KW_IN
| OP_EQ
| OP_PLUS
| OP_MINUS

type state = 
| Q0
| Q1
| Q2
| Q3
| Q4
| Q5
| Q6
| Q7
| Q8
| Q9
| Q10


(* ([a − z][a − zA − Z_′] * ) | ([1 − 9][0 − 9] * ) | let | in | = | + | − , 이 정규표현식을 인식하여 토큰을 반환*)

let lexing_error str =
    failwith ("Lexing error: " ^ str)

(*종료상태 : 1(OP_EQ),2(OP_PLUS),3(OP_MINUS),4(NUMBER of string),7 (IDENT of string),9(let),10(in), l, le i*)

let determine_token str state : token =
    match state with
    | Q1 -> OP_EQ
    | Q2 -> OP_PLUS
    | Q3 -> OP_MINUS
    | Q4 -> NUMBER str
    | Q5 -> IDENT str
    | Q6 -> IDENT str
    | Q7 -> IDENT str
    | Q8 -> IDENT str
    | Q9 -> KW_LET
    | Q10 -> KW_IN
    | _ -> lexing_error str

let aux (s:state) (c:char) (stf:string) : state = (*tokenization을 수행*)
    match s, c with
    | Q0, '=' -> Q1 (*OP_EQ*)
    | Q0, '+' -> Q2 (*OP_PLUS*)
    | Q0, '-' -> Q3 (*OP_MINUS*)
    | Q0, '1'..'9' -> Q4 (*NUMBER of string*)
    | Q4, '0'..'9' -> Q4 (*NUMBER of string*)
    | Q4, _ -> lexing_error stf
    | Q0, alph -> if alph = 'l' then Q5 else
        if alph = 'i' then Q6 else (
            match alph with
            | 'a'..'z' -> Q7 (*Q7 : IDENT of string*)
            | _ -> lexing_error stf
        )
    | Q5, alph -> if alph = 'e' then Q8 else ( (*le까지 완성*)
        match alph with
        | 'a'..'z' -> Q7
        | 'A'..'Z' -> Q7
        | '_' -> Q7
        | '\'' -> Q7
        | _ -> lexing_error stf
    )
    | Q8, alph -> if alph = 't' then Q9 else ( (*let 완성*)
        match alph with
        | 'a'..'z' -> Q7
        | 'A'..'Z' -> Q7
        | '_' -> Q7
        | '\'' -> Q7
        | _ -> lexing_error stf
        )
    | Q9, alph -> ( match alph with
        | 'a'..'z' -> Q7
        | 'A'..'Z' -> Q7
        | '_' -> Q7
        | '\'' -> Q7
        | _ -> lexing_error stf
    )
    | Q6, alph -> if alph = 'n' then Q10 else ( (*in 완성*)
        match alph with
        | 'a'..'z' -> Q7
        | 'A'..'Z' -> Q7
        | '_' -> Q7
        | '\'' -> Q7
        | _ -> lexing_error stf
    )
    | Q10, alph -> ( match alph with
        | 'a'..'z' -> Q7
        | 'A'..'Z' -> Q7
        | '_' -> Q7
        | '\'' -> Q7
        | _ -> lexing_error stf
    )
    | Q7, alph -> ( match alph with
        | 'a'..'z' -> Q7
        | 'A'..'Z' -> Q7
        | '_' -> Q7
        | '\'' -> Q7
        | _ -> lexing_error stf
    )
    | _, _ -> lexing_error stf

let rec traverse state ls stf =
    match ls with
    | [] -> state
    | h::t -> traverse (aux state h stf) t stf

let lex (stf : string) : token = 
    determine_token stf (traverse Q0 (List.of_seq (String.to_seq stf)) stf)

(*종료상태 : 1(OP_EQ),2(OP_PLUS),3(OP_MINUS),4(NUMBER of string),7 (IDENT of string),9(let),10(in)*)

(*Problem 2 - parse*)

(*
구현해야할 함수
shift
reduce
tokenize

필요한 자료구조
input_buffer (token list) / stack / temp_stack

절차
1. input string이 들어옴
2. 해당 string을 전부 tokenize (lexing error가 발생할 수 있음)
3. tokenize된 string을 list로 변환 (input_buffer 化)
4. buffer의 첫 번째 토큰부터 shift 및 reduce를 stack에 수행 -> buffer가 빌 때까지
5. buffer가 빈 후 stack에 하나의 expr이 남으면 accept 아니면 fail / 단, reduce가 가능할 경우 reduce를 수행
6. expr 출력
*)

type expr =
    | LetIn of string * expr * expr
    | Plus of expr * expr
    | Minus of expr * expr
    | Num of string
    | Id of string

type parse_stack_elem = 
    | T of token
    | E of expr

let rev ls =
  let rec inner_rev ls acc =
    match ls with
    | [] -> acc
    | h :: t -> if t = [] then h::acc else
    inner_rev t (h::acc)
  in inner_rev ls []

let tokenize str : token list =
    let ls = String.split_on_char ' ' str in
    let rec tail_tokenize (str_ls : string list) (acc : token list) : (token list) =
        match str_ls with
        | [] -> rev acc
        | h::t -> tail_tokenize t ((lex h)::acc)
    in tail_tokenize ls [] 

let shift t stack =
    T t::stack

(*
Expression Rule : e ::=  KW_LET IDENT OP_EQ e KW_IN e | e OP_PLUS e | e OP_MINUS e | NUMBER | IDENT
Right Associative : shift 우선
if lookahead = KW_IN then if stack top = KW_LET ... | PLUS | MINUS then reduce else shift
if stack top = IDENT then if lookahead = OP_EQ then reduce else shift
*)

let reduce (stack : parse_stack_elem list) =
    match stack with
    | E e2 :: T KW_IN :: E e1 :: T OP_EQ :: T IDENT x :: T KW_LET :: t -> E (LetIn (x, e1, e2)) :: t
    | E e2 :: T OP_PLUS :: E e1 :: t -> E (Plus (e1, e2)) :: t
    | E e2 :: T OP_MINUS :: E e1 :: t -> E (Minus (e1, e2)) :: t
    | T (NUMBER n) :: t -> E (Num n) :: t
    | T (IDENT x) :: t -> E (Id x) :: t
    | _ -> stack

let rec last_reduce stack =
  let reduced = reduce stack in
  if reduced = stack then stack
  else last_reduce reduced


let parsing (stack : parse_stack_elem list) (lookahead : token) =
    match stack, lookahead with
    | [], lookahead -> shift lookahead stack
    | T KW_LET :: _, NUMBER _  -> reduce (shift lookahead stack)
    | T OP_EQ :: _, NUMBER _ -> reduce (shift lookahead stack)
    | T (NUMBER _) :: _, NUMBER _ -> reduce (shift lookahead stack)
    | T KW_IN :: _, NUMBER _ -> reduce (shift lookahead stack)
    | T OP_PLUS ::  _, NUMBER _ -> reduce (shift lookahead stack)
    | T OP_MINUS :: _, NUMBER _ -> reduce (shift lookahead stack)
    | E Num _ :: _, NUMBER _ -> reduce (shift lookahead stack)


    | T KW_LET :: _, lookahead -> shift lookahead stack
    | T OP_EQ :: _, lookahead-> shift lookahead stack
    | T (NUMBER _) :: _, lookahead -> shift lookahead stack
    | T KW_IN :: _, lookahead -> shift lookahead stack
    | T OP_PLUS :: _, lookahead -> shift lookahead stack
    | T OP_MINUS :: _, lookahead -> shift lookahead stack
    | E Num _ :: _, lookahead -> if lookahead = KW_IN then shift lookahead (last_reduce stack) else shift lookahead stack
    | E Id _ :: _, lookahead -> if lookahead = KW_IN then shift lookahead (last_reduce stack) else shift lookahead stack
    
    (*Rule 1*)
    | E _ :: T KW_IN :: E _ :: T OP_EQ :: T IDENT _ :: T KW_LET :: _, lookahead -> if lookahead = KW_IN then shift lookahead (last_reduce stack) else shift lookahead stack
    | E _ :: T OP_PLUS :: E _ :: _ , lookahead -> if lookahead = KW_IN then shift lookahead (last_reduce stack) else shift lookahead stack
    | E _ :: T OP_MINUS :: E _ :: _ , lookahead -> if lookahead = KW_IN then shift lookahead (last_reduce stack) else shift lookahead stack

    (*Right Associative*)
    | E (LetIn (_, _, _)) :: _, lookahead -> shift lookahead stack
    | E (Plus (_, _)) :: _, lookahead -> shift lookahead stack
    | E (Minus (_, _)) :: _, lookahead -> shift lookahead stack
    
    (*Rule 2*)
    | T IDENT _ :: _, lookahead -> if lookahead = OP_EQ then shift lookahead stack else if lookahead = KW_IN then shift lookahead (last_reduce stack) else shift lookahead (reduce stack)
    
    
let rec parsing_traverse stack str_list = 
    match str_list with
    | [] -> stack
    | h::t -> parsing_traverse (parsing stack h) t


let parse (str : string) : expr =
    match last_reduce (parsing_traverse [] (tokenize str)) with
    | [] -> failwith "Parsing error: empty stack"
    | [E e] -> e
    | _::_ -> failwith ("Parsing error: " ^ str)
