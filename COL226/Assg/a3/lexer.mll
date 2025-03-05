{
  (* Header Section *)
  open Printf (* Required for string formatting *)
  open Lexing (* Required for lexbuf type *)
  open Tokens (* Required for token type *)
  exception LexError of string  (* Custom exception for lexical errors *)
}

(* Definitions Section *)
let letter = ['a'-'z' 'A'-'Z']              (* Letters are case-insensitive *)
let digit = ['0'-'9']                      (* Digits 0-9 *)
let id = (letter | digit | '_' | '\'') (letter | digit | '_' | '\'')*   (* Identifier: starts with a letter *)

let int_pos_literal   =  digit+                     (* Positive Integer *)
let whitespace    = [' ' '\t' '\r' '\n' ]+          (* One or more whitespace characters *)
let string_literal = '\"' [^'"']* '\"'              (* Text inside double quotes *)
let sl_comment    = "//" [^'\n']*                  (* Single-line comment starting with // *)

(* Rules Section *)
rule token = parse
  (* Skip whitespace and comments *)
  | whitespace | sl_comment   { token lexbuf }
  | "/*"    { comment 1 lexbuf } (* Nested comments *)
  
  (*input() and input(filename) filename are without quotes*)
  | "input()" { INPUT "" }  (* input without filename *)
  | "input(" ([^')']+ as filename) ")" { INPUT filename }  (* input with filename *)  
  | "print"  { PRINT }

  (*Control Constrcts*)
  | "if"  { IF }   | "elif"   { ELIF } | "else"   { ELSE } 
  | "for"    { FOR }    | "while"  { WHILE } | "do" { DO }
  
  (* Type keywords *)
  | "bool"   { BOOLEAN } | "int"    { INT_POS } | "float"  { FLOAT }
  | "vector" { VECTOR }  | "matrix" { MATRIX }
  | "true"   { BOOL_LITERAL true }  | "false"  { BOOL_LITERAL false }
  
  (*---Operations---*)

  (* Special operators *)
  | "abs"    { ABS }

  (*Bit Operations*)
  | "&"      { BAND }   | "|"    { BOR }    | "^"      { BXOR } 
  | "~"      { BNOT }   | "<<"     { LSHIFT } | ">>"     { RSHIFT }

  (* Arithmetic Operators *)
  | "+"      { PLUS }   | "-"      { MINUS }
  | "*"      { MUL }    | "/"      { DIV }    | "%"      { MOD }

  (* Comparison Operators *)
  | "=="     { EQ }     | "!="     { NEQ }
  | "<="     { LEQ }    | ">="     { GEQ }
  | "<"      { LT }     | ">"      { GT }
  
  (* Logical Operators - fixed to use standard symbols *)
  | "and"     { AND }    | "or"     { OR }     | "not"  { NOT } | "xor" { XOR }

  (* Assignment *)
  | ":="     { ASSIGN }

  (* Vector and Matrix operations *)
  | "."      { DOT }
  | "len"    { DIM }
  | "mag"    { MAG }
  | "trans"  { TRANS }
  | "angle"  { ANGLE }
  
  (* Delimiters *)
  | ";"      { SEMICOLON }
  | "{"      { LBRACE }  | "}"      { RBRACE }
  | "("      { LPAREN }  | ")"      { RPAREN }
  | "["      { LBRACKET }| "]"      { RBRACKET }
  | ","      { COMMA }
  
  (* Literals *)
  | (digit+ '.' digit* | '.' digit+) (('e'|'E') ('+'|'-')? digit+)? as f  { FLOAT_LITERAL(float_of_string f) } (*Float number e/E int number*)
  | digit+ ('e'|'E') ('+'|'-')? digit+ as f                               { FLOAT_LITERAL(float_of_string f) } (*Int numbe e/E int number*)
  | int_pos_literal as i     { INT_POS_LITERAL(int_of_string i) }
  | string_literal as s  { STRING_LITERAL(String.sub s 1 (String.length s - 2)) }
  | id as identifier     { ID identifier }
  
  | eof      { EOF }
  | _ as c       { raise (LexError (sprintf "Invalid token: %c" c)) }

(* Handling nested comments *)
and comment level = parse
  | "/*"     { comment (level + 1) lexbuf }
  | "*/"     { if level = 1 then token lexbuf else comment (level - 1) lexbuf }
  | _        { comment level lexbuf }
  | eof      { raise (LexError "Unterminated comment") }

{
  (* Function to convert token to string for printing *)
  let string_of_token = function
    | INPUT s -> sprintf "INPUT(%s)" s
    | PRINT -> "PRINT"
    | BOOL_LITERAL b -> sprintf "BOOL_LITERAL(%B)" b
    | INT_POS_LITERAL i -> sprintf "INT_LITERAL(%d)" i
    | FLOAT_LITERAL f -> sprintf "FLOAT_LITERAL(%f)" f
    | STRING_LITERAL s -> sprintf "STRING_LITERAL(%s)" s
    | IF -> "IF"
    | ELIF -> "ELIF"
    | ELSE -> "ELSE"
    | FOR -> "FOR"
    | WHILE -> "WHILE"
    | DO -> "DO"
    | BOOLEAN -> "BOOLEAN"
    | INT_POS -> "INTEGER"
    | FLOAT -> "FLOAT"
    | VECTOR -> "VECTOR"
    | MATRIX -> "MATRIX"
    | ID id -> sprintf "ID(%s)" id
    | BAND -> "BAND"
    | BOR -> "BOR"
    | BXOR -> "BXOR"
    | BNOT -> "BNOT"
    | LSHIFT -> "LSHIFT"
    | RSHIFT -> "RSHIFT"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | MUL -> "MUL"
    | DIV -> "DIV"
    | MOD -> "MOD"
    | DOT -> "DOT"
    | ABS -> "ABS"
    | DIM -> "DIM"      
    | MAG -> "MAG"       
    | TRANS -> "TRANS"   
    | ANGLE -> "ANGLE" 
    | EQ -> "EQ"
    | NEQ -> "NEQ"
    | LT -> "LT"
    | GT -> "GT"
    | LEQ -> "LEQ"
    | GEQ -> "GEQ"
    | AND -> "AND"
    | OR -> "OR"
    | NOT -> "NOT"
    | XOR -> "XOR"
    | ASSIGN -> "ASSIGN"
    | SEMICOLON -> "SEMICOLON"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACKET -> "LBRACKET"
    | RBRACKET -> "RBRACKET"
    | COMMA -> "COMMA"
    | EOF -> "EOF"

  (* Function to print all tokens from a channel *)
  let print_all_tokens channel =
    let lexbuf = Lexing.from_channel channel in
    let rec print_tokens () =
      try
        let tok = token lexbuf in
        printf "%s\n" (string_of_token tok);
        if tok <> EOF then print_tokens ()
      with
        | LexError msg -> printf "Lexical error: %s\n" msg
        | _ -> printf "Unexpected error during lexical analysis\n"
    in
    print_tokens ()

  (* Function to print all tokens from a string *)
  let print_tokens_from_string str =
    let lexbuf = Lexing.from_string str in
    let rec print_tokens () =
      try
        let tok = token lexbuf in
        printf "%s\n" (string_of_token tok);
        if tok <> EOF then print_tokens ()
      with
        | LexError msg -> printf "Lexical error: %s\n" msg
        | _ -> printf "Unexpected error during lexical analysis\n"
    in
    print_tokens ()
}