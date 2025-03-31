type token =
  | BOOL_LITERAL of (
# 19 "parser.mly"
        bool
# 6 "parser.mli"
)
  | INT_LITERAL of (
# 20 "parser.mly"
        int
# 11 "parser.mli"
)
  | FLOAT_LITERAL of (
# 21 "parser.mly"
        float
# 16 "parser.mli"
)
  | ID of (
# 22 "parser.mly"
        string
# 21 "parser.mli"
)
  | INPUT of (
# 22 "parser.mly"
        string
# 26 "parser.mli"
)
  | PRINT
  | BOOL
  | INT
  | FLOAT
  | VECTOR
  | MATRIX
  | AND
  | OR
  | NOT
  | XOR
  | ABS
  | SQRT
  | PLUS
  | MINUS
  | MUL
  | DIV
  | MOD
  | POWER
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | DOT
  | MAG
  | DIM
  | ANGLE
  | TRANS
  | DET
  | TRACE
  | ASSIGN
  | IF
  | ELSE
  | FOR
  | WHILE
  | SEMICOLON
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
