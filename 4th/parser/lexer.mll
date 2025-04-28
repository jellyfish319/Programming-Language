{
  open Parser

  exception LexingError of string
}

(* Decimal number *)
let pos_digit = ['1'-'9']
let digit = ['0'-'9']
let pos_number = pos_digit digit*
let number = "0" | pos_number

(* White space *)
let ws = [' ''\t''\n']*

rule read = 
  parse
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LEFT_PARENTHESIS }
  | ")" { RIGHT_PARENTHESIS }
  | number as n { NUMBER (int_of_string n) }
  | ws { read lexbuf }
  | eof { EOF }
  | _ { raise (LexingError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }


