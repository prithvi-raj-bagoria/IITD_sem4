(* Token type definition for our language *)
type token =
    (* I/O Commands *)
    | INPUT of string
    | PRINT 

    (* Literals *)
    | BOOL_LITERAL of bool     (* Boolean true/false values *)
    | INT_LITERAL of int       (* Integer literals *)
    | FLOAT_LITERAL of float   (* Floating point literals *)
    | EXP_LITERAL of string     (* Exponential literals *)
    | STRING_LITERAL of string (* String literals *)

    (* Control keywords *)
    | IF
    | ELSE
    | ELSEIF
    | FOR
    | WHILE
    | LET

    (* Type keywords *)
    | BOOLEAN         (* for boolean type *)
    | INTEGER         (* for integer type *)
    | FLOAT          (* for float/FLOAT type *)
    | VECTOR          (* for vector type *)
    | MATRIX          (* for matrix type *)

    (* Identifiers for variables *)
    | ID of string

    (* Operators *)
    (* Arithmetic *)
    | PLUS | MINUS | MUL | DIV | MOD  | DOT | ABS
    (* Comparison *)
    | EQ | NEQ | LT | GT | LEQ | GEQ
    (* Boolean *)
    | AND | OR | NOT | XOR
    (* Assignment *)
    | ASSIGN          (* for ":=" assignment *)

    (* Delimiters and punctuation *)
    | SEMICOLON
    | LBRACE | RBRACE        (* { } *)
    | LPAREN | RPAREN        (* ( ) *)
    | LBRACKET | RBRACKET    (* [ ] *)
    | COMMA

    | EOF