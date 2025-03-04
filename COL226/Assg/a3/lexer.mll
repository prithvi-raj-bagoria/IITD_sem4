{
  (* Header Section *)
  open Printf
  open Lexing
  open Tokens
  exception LexError of string
}

(* Definitions Section *)
let letter       = ['a'-'z' 'A'-'Z']
let digit        = ['0'-'9']
let id           = letter (letter | digit | ['_' '\''])*
let int_literal  = '-'? digit+
let float_literal = '-'? digit+ "." digit*
let whitespace   = [' ' '\t' '\r' '\n']+
let string_literal = "\"" [^'"']* "\""
let sl_comment   = "//" [^'\n']*

(* Rules Section *)
rule token = parse
  (* Skip whitespace and comments *)
  | whitespace | sl_comment  { token lexbuf }
  | "/*"                     { comment 1 lexbuf }
  
  (* Keywords and I/O - made consistent as lowercase *)
  | "input"  { INPUT }  | "print"  { PRINT }
  | "if"     { IF }     | "then"   { THEN }   | "else"   { ELSE }
  | "for"    { FOR }    | "while"  { WHILE }  | "let"    { LET }
  
  (* Type keywords *)
  | "bool"   { BOOLEAN } | "int"    { INTEGER } | "float"  { SCALAR }
  | "vector" { VECTOR }  | "matrix" { MATRIX }
  | "true"   { BOOL_LITERAL true }  | "false"  { BOOL_LITERAL false }
  
  (* Comparison Operators *)
  | "=="     { EQ }     | "!="     { NEQ }
  | "<="     { LEQ }    | ">="     { GEQ }
  | "<"      { LT }     | ">"      { GT }
  
  (* Special operators *)
  | "."      { DOT }    | "abs"    { ABS }

  (* Logical Operators - fixed to use standard symbols *)
  | "&&"     { AND }    | "||"     { OR }     | "!"      { NOT } | "^" { XOR }
  
  (* Arithmetic Operators *)
  | "+"      { PLUS }   | "-"      { MINUS }
  | "*"      { MUL }    | "/"      { DIV }    | "%"      { MOD }
  
  (* Assignment *)
  | ":="     { ASSIGN }
  
  (* Delimiters *)
  | ";"      { SEMICOLON }
  | "{"      { LBRACE }  | "}"      { RBRACE }
  | "("      { LPAREN }  | ")"      { RPAREN }
  | "["      { LBRACKET }| "]"      { RBRACKET }
  | ","      { COMMA }
  
  (* Literals *)
  | float_literal as f   { FLOAT_LITERAL (float_of_string f) }
  | int_literal as i     { INT_LITERAL (int_of_string i) }
  | string_literal as s  { STRING_LITERAL (String.sub s 1 (String.length s - 2)) }
  | id as identifier     { ID identifier }
  
  | eof                  { EOF }
  | _                 { raise (LexError "Invalid token") }

(* Handling nested comments *)
and comment level = parse
  | "/*" { comment (level + 1) lexbuf }
  | "*/" { if level = 1 then token lexbuf else comment (level - 1) lexbuf }
  | _    { comment level lexbuf }
  | eof  { raise (LexError "Unterminated comment") }
