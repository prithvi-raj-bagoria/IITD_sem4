type token =
  | BOOL_LITERAL of (
# 8 "parser.mly"
        bool
# 6 "parser.mli"
)
  | INT_LITERAL of (
# 9 "parser.mly"
        int
# 11 "parser.mli"
)
  | FLOAT_LITERAL of (
# 10 "parser.mly"
        float
# 16 "parser.mli"
)
  | ID of (
# 11 "parser.mly"
        string
# 21 "parser.mli"
)
  | INPUT of (
# 11 "parser.mly"
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
  | INVERSE
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
