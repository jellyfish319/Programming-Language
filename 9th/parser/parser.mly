%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token KW_LET KW_IN KW_FUN KW_IF KW_THEN KW_ELSE
%token TRUE FALSE PLUS MINUS NEG EQ ARROW LESSTHAN
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS 
%token EOF

%nonassoc low
%nonassoc ID NUMBER TRUE FALSE 
%nonassoc LEFT_PARENTHESIS 
%right KW_LET
%left LESSTHAN
%left PLUS MINUS 
%nonassoc NEG KW_IF
%nonassoc high
%start <Ast.expr> parse
%%

parse: 
  | e=expr EOF { e }
  ;
expr:
  | KW_LET x=ID EQ e1=expr KW_IN e2=expr %prec KW_LET { LetIn (x, e1, e2) }
  | e1=expr e2=expr %prec high { App (e1, e2) }
  | KW_LET x=ID xlst=var_list EQ e1=expr KW_IN e2=expr %prec KW_LET { 
      LetIn (x, List.fold_right (fun x e -> Lambda (x, e)) xlst e1, e2)
    }
  | LEFT_PARENTHESIS KW_FUN xlst=var_list ARROW e=expr RIGHT_PARENTHESIS { 
      List.fold_right (fun x e -> Lambda (x, e)) xlst e
    }
  | KW_IF e1=expr KW_THEN e2=expr KW_ELSE e3=expr %prec low { Cond (e1, e2, e3) }
  | x=ID { Id x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | e1=expr LESSTHAN e2=expr { LessThan (e1, e2) }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | n=NUMBER { Num n }
  | NEG e=paren_expr { Sub (Num 0, e)}
  | e=paren_expr { e }
  ;
var_list:
  | x=ID { [x] }
  | x=ID xlst=var_list { x :: xlst }
  ;
paren_expr:
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { e }
  ;
