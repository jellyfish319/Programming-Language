%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token IF ELSE TRUE FALSE SEMICOLON WHILE
%token PLUS MINUS LESSTHAN GREATERTHAN EQ AND OR
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_BRACE RIGHT_BRACE
%token EOF
%left PLUS MINUS 
%left EQ 
%left LESSTHAN GREATERTHAN AND OR 
%start <Ast.prog> parse
%%

parse: 
  | sl=stmt* EOF { Program sl }
  ;
stmt:
  | WHILE e=expr LEFT_BRACE sl=stmt* RIGHT_BRACE { LoopStmt (e, sl) }
  | x=ID EQ e=expr SEMICOLON { AssignStmt (x, e) }
  | IF e=expr LEFT_BRACE sl=stmt* RIGHT_BRACE { IfStmt (e, sl, []) }
  | IF e=expr LEFT_BRACE sl1=stmt* RIGHT_BRACE ELSE LEFT_BRACE sl2=stmt* RIGHT_BRACE { IfStmt (e, sl1, sl2) }
  ;
expr:
  | n=NUMBER { Num n }
  | x=ID { Name x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { e }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | e1=expr LESSTHAN e2=expr { Lt (e1, e2) }
  | e1=expr GREATERTHAN e2=expr { Gt (e1, e2) }
  | e1=expr EQ EQ e2=expr { Eq (e1, e2) }
  | e1=expr AND e2=expr { And (e1, e2) }
  | e1=expr OR e2=expr { Or (e1, e2) }
  ;
