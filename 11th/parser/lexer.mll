{
  open Parser

  exception LexingError of string
}

(* Decimal number *)
let pos_digit = ['1'-'9']
let digit = ['0'-'9']
let pos_number = pos_digit digit*
let number = "0" | pos_number | '-' pos_number

(* Identifier *)
let id = ['a'-'z'] ['a'-'z''A'-'Z''_''\'''0'-'9']*

(* White space *)
let ws = [' ''\t''\n']*

rule read = 
  parse
  | ";" { SEMICOLON }
  | "if" { IF }
  | "else" { ELSE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "while" { WHILE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LEFT_PARENTHESIS }
  | ")" { RIGHT_PARENTHESIS }
  | "{" { LEFT_BRACE }
  | "}" { RIGHT_BRACE }
  | "=" { EQ }
  | "<" { LESSTHAN }
  | ">" { GREATERTHAN }
  | "&&" { AND }
  | "||" { OR }
  | number as n { NUMBER (int_of_string n) }
  | id as i { ID i }
  | ws { read lexbuf }
  | eof { EOF }
  | _ { raise (LexingError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }


