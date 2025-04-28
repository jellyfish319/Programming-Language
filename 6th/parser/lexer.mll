{
  open Parser

  exception LexingError of string
}

(* Decimal number *)
let pos_digit = ['1'-'9']
let digit = ['0'-'9']
let pos_number = pos_digit digit*
let number = "0" | pos_number | '-' pos_number

(* White space *)
let ws = [' ''\t''\n']*

(* Identifier *)
let ident = ['a'-'z']['a'-'z''A'-'Z''0'-'9''_''\'']*

rule read = 
  parse
  | "+" { PLUS }
  | "-" { MINUS }
  | "(" { LEFT_PARENTHESIS }
  | ")" { RIGHT_PARENTHESIS }
  | "~" { NEG }
  | "let" { KW_LET }
  | "in" { KW_IN }
  | "def" { KW_DEF }
  | "endef" { KW_ENDEF }
  | "=" { EQ }
  | "," { COMMA }
  | number as n { NUMBER (int_of_string n) }
  | ident as id { ID id }
  | ws { read lexbuf }
  | eof { EOF }
  | _ { raise (LexingError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }


