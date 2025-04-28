%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token KW_LET KW_IN 
%token PLUS MINUS NEG EQ
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS 
%token EOF
%right KW_LET
%left PLUS MINUS
%nonassoc NEG
%nonassoc high
%start <Ast.expr> parse
%%

parse: 
  | e=expr EOF { e }
;
expr:
  | KW_LET x=ID EQ e1=expr KW_IN e2=expr %prec KW_LET { LetIn (x, e1, e2) }
  | x=ID { Id x }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr %prec high { Sub (e1, e2) }
  | n=NUMBER { Num n }
  | MINUS n=NUMBER { Num (-1 * n) }
  | NEG e=expr { Sub (Num 0, e)}
  | e=paren_expr { e }
  ;
paren_expr:
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { e }
  ;
