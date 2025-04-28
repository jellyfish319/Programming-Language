%{
  open Ast
%}
%token <int> NUMBER
%token PLUS MINUS
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS 
%token EOF
%left PLUS MINUS
%nonassoc high
%start <Ast.expr> parse
%%

parse: 
  | e=expr EOF { e }
;
expr:
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr %prec high { Sub (e1, e2) }
  | n=NUMBER { Num n }
  | MINUS n=NUMBER { Num (-1 * n) }
  | e=paren_expr { e }
  ;
paren_expr:
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { e }
  ;
