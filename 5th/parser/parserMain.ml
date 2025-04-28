let parse (s : string) : Ast.expr = 
  let lbuf = Lexing.from_string s in
  Parser.parse Lexer.read lbuf
