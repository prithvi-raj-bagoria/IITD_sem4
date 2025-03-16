(* Token type definition for our language *)
type token =
    (* I/O Commands *)
    | INPUT of string
    | PRINT 

    (* Identifiers for variables *)
    | ID of string

    (* Type keywords *)
    | BOOLEAN         (* for boolean type *)
    | INT_POS         (* for integer type *)
    | FLOAT          (* for float/FLOAT type *)
    | VECTOR          (* for vector type *)
    | MATRIX          (* for matrix type *)
    
    (*Data Section*)
    (* Constants *)
    | BOOL_LITERAL of bool     (* Boolean true/false values *)
    | INT_POS_LITERAL of int       (* positive Integer literals *)
    | FLOAT_LITERAL of float   (* Floating point literals *)
    | STRING_LITERAL of string (* String literals *)
   
    (* ---Operators--- *)
    | ABS

    (* Arithmetic *)
    | PLUS | MINUS | MUL | DIV | MOD 

    (* Comparison *)
    | EQ | NEQ | LT | GT | LEQ | GEQ

    (* Boolean *)
    | AND | OR | NOT | XOR

    (* Vector and Matrix operations *)
    | DOT | DIM | MAG | TRANS | ANGLE | DET


    (* Control keywords *)
    | ASSIGN          (* for ":=" assignment *)
    | IF
    | ELSE
    | FOR
    | WHILE
    | DO
    
    (* Delimiters and punctuation *)
    | SEMICOLON
    | LBRACE | RBRACE        (* { } *)
    | LPAREN | RPAREN        (* ( ) *)
    | LBRACKET | RBRACKET    (* [ ] *)
    | COMMA

    | EOF