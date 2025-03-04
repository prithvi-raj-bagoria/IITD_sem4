(* Token type definition for our language *)
type token =
    (* I/O Commands *)
    | INPUT 
    | PRINT 

    (* Literals *)
    | BOOL_LITERAL of bool     (* Boolean true/false values *)
    | INT_LITERAL of int       (* Integer literals *)
    | FLOAT_LITERAL of float   (* Floating point literals *)
    | STRING_LITERAL of string (* String literals *)

    (* Control keywords *)
    | IF
    | THEN
    | ELSE
    | FOR
    | WHILE
    | LET

    (* Type keywords *)
    | BOOLEAN         (* for boolean type *)
    | INTEGER         (* for integer type *)
    | SCALAR          (* for float/scalar type *)
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