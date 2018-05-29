type token =
  | IDENT of (Syntax.symbol)
  | INT64 of (Int64.t)
  | BOOL
  | ELSE
  | FALSE
  | IF
  | INT
  | PRINT
  | RETURN
  | TRUE
  | VOID
  | WHILE
  | EQ
  | LT
  | GT
  | LE
  | GE
  | EQEQ
  | BANGEQ
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PIPEPIPE
  | AMPAMP
  | EOF
  | BANG
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE

val xprogram :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.pprogram
val xexpr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.pexpr
val xstmt :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.pstmt
