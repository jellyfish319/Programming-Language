%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token KW_LET KW_IN KW_DEF KW_ENDEF
%token PLUS MINUS NEG EQ COMMA
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS 
%token EOF
%right KW_LET
%left PLUS MINUS
%nonassoc high
%start <Ast.prog> parse
%%

parse: 
  | e=expr EOF { Prog ([], e) }
  | dlst=decl_list e=expr EOF { Prog (dlst, e) }
  ;
decl_list: 
  | d=decl { [d] }
  | d=decl dlst=decl_list { d :: dlst }
  ;
decl: 
  | KW_DEF x=ID EQ e=expr KW_ENDEF { FunDef (x, [], e) }
  | KW_DEF x=ID vlst=var_list EQ e=expr KW_ENDEF { FunDef (x, vlst, e) }
  ;
var_list:
  | x=ID { [x] }
  | x=ID vlst=var_list { x :: vlst }
  ;
expr:
  | KW_LET x=ID EQ e1=expr KW_IN e2=expr %prec KW_LET { LetIn (x, e1, e2) }
  | x=ID LEFT_PARENTHESIS RIGHT_PARENTHESIS { Call (x, []) }
  | x=ID LEFT_PARENTHESIS elst=expr_list RIGHT_PARENTHESIS { Call (x, elst) }
  | x=ID { Id x }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr %prec high { Sub (e1, e2) }
  | n=NUMBER { Num n }
  | NEG e=paren_expr { Sub (Num 0, e)}
  | e=paren_expr { e }
  ;
expr_list:
  | e=expr { [e] }
  | e=expr COMMA elst=expr_list { e :: elst }
  ;
paren_expr:
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { e }
  ;
