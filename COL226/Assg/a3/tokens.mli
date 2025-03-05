(* Token type definition for our language *)
type token =
    (* I/O Commands *)
    | INPUT of string
    | PRINT 

    (* Literals *)
    | BOOL_LITERAL of bool     (* Boolean true/false values *)
    | INT_POS_LITERAL of int       (* Integer literals *)
    | FLOAT_LITERAL of float   (* Floating point literals *)
    | STRING_LITERAL of string (* String literals *)
   
    (* Control keywords *)
    | IF
    | ELIF
    | ELSE
    | FOR
    | WHILE
    | DO

    (* Type keywords *)
    | BOOLEAN         (* for boolean type *)
    | INT_POS         (* for integer type *)
    | FLOAT          (* for float/FLOAT type *)
    | VECTOR          (* for vector type *)
    | MATRIX          (* for matrix type *)

    (* Identifiers for variables *)
    | ID of string

    (* ---Operators--- *)
    | ABS
    
    (*Bit operations*)
    | BAND | BOR | BXOR | BNOT | LSHIFT | RSHIFT

    (* Arithmetic *)
    | PLUS | MINUS | MUL | DIV | MOD 

    (* Comparison *)
    | EQ | NEQ | LT | GT | LEQ | GEQ

    (* Boolean *)
    | AND | OR | NOT | XOR

    (* Assignment *)
    | ASSIGN          (* for ":=" assignment *)

    (* Vector and Matrix operations *)
    | DOT | DIM | MAG | TRANS | ANGLE

    (* Delimiters and punctuation *)
    | SEMICOLON
    | LBRACE | RBRACE        (* { } *)
    | LPAREN | RPAREN        (* ( ) *)
    | LBRACKET | RBRACKET    (* [ ] *)
    | COMMA

    | EOF