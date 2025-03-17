{
  (* Header Section *)
  open Printf (* Required for string formatting *)
  open Lexing (* Required for lexbuf type *)
  exception LexError of string  (* Custom exception for lexical errors *)

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
}

(* Definitions Section *)
let letter = ['a'-'z' 'A'-'Z']              (* Letters are case-insensitive *)
let digit = ['0'-'9']                      (* Digits 0-9 *)
let id = letter (letter | digit | '_')*   (* Identifier: starts with a letter *)

let int_pos_literal = digit+               (* Positive integers only *)
let float_literal = int_pos_literal? '.' digit*         (* 1.2, 0.2, 0. *)
let exp_literal = float_literal ('e' | 'E') ('+' | '-')? digit+ (* 1.2e3, 1.2E3 *)

let whitespace = [' ' '\t' '\r' '\n']+     (* Whitespace including newlines *)
let string_literal = "\"" [^'"']* "\""     (* Text inside double quotes *)
let sl_comment = "//" [^'\n']*             (* Single-line comment *)

(* Rules Section *)
rule token = parse
  (* Skip whitespace and comments *)
  | whitespace | sl_comment  { token lexbuf }

  | "/*"                     { comment 1 lexbuf }
  
  (* I/O Commands *)
  | "input()" { INPUT "" }  (* input without filename *)
  | "input(" ([^')']+ as filename) ")" { INPUT filename }  (* input with filename *)
  | "print"  { PRINT }
  
  (* Type keywords *)
  | "bool"   { BOOLEAN } 
  | "int"    { INT_POS } 
  | "float"  { FLOAT }
  | "vector" { VECTOR }  
  | "matrix" { MATRIX }
  | "true"   { BOOL_LITERAL true }  
  | "false"  { BOOL_LITERAL false }
  
  (* Comparison Operators *)
  | "=="     { EQ }     
  | "!="     { NEQ }
  | "<="     { LEQ }    
  | ">="     { GEQ }
  | "<"      { LT }     
  | ">"      { GT }
  
  (* Special operators *)
  | "abs"    { ABS }

  (* Vector and Matrix operations *)
  | "."      { DOT }    
  | "dim"    { DIM }
  | "mag"    { MAG }
  | "trans"  { TRANS }
  | "angle"  { ANGLE }
  | "det"    { DET }

  (* Logical Operators *)
  | "and"    { AND }    
  | "or"     { OR }     
  | "not"    { NOT } 
  | "xor"    { XOR }
  
  (* Arithmetic Operators *)
  | "+"      { PLUS }   
  | "-"      { MINUS }
  | "*"      { MUL }    
  | "/"      { DIV }    
  | "%"      { MOD }
  
  (* Control keywords *)
  | "if"     { IF }     
  | "else"   { ELSE }
  | "for"    { FOR }    
  | "while"  { WHILE }  
  | "do"     { DO }
  
  (* Assignment *)
  | ":="     { ASSIGN }
  
  (* Delimiters *)
  | ";"      { SEMICOLON }
  | "{"      { LBRACE }  
  | "}"      { RBRACE }
  | "("      { LPAREN }  
  | ")"      { RPAREN }
  | "["      { LBRACKET }
  | "]"      { RBRACKET }
  | ","      { COMMA }
  
  (* Literals *)
  | (int_pos_literal? '.' digit* | digit+ '.' digit*) (('e'|'E') ('+' | '-')? digit+)? 
                         { FLOAT_LITERAL(float_of_string (Lexing.lexeme lexbuf)) }
  | int_pos_literal as i { INT_POS_LITERAL(int_of_string i) }
  | string_literal as s  { STRING_LITERAL(String.sub s 1 (String.length s - 2)) }
  | id as identifier     { ID identifier }
  
  | eof      { EOF }
  | _ as c   { raise (LexError (sprintf "Invalid token: %c" c)) }

(* Handling nested comments *)
and comment level = parse
  | "/*"     { comment (level + 1) lexbuf }
  | "*/"     { if level = 1 then token lexbuf else comment (level - 1) lexbuf }
  | _        { comment level lexbuf }
  | eof      { raise (LexError "Unterminated comment") }
