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
let id = letter (letter | digit | '_')*   (* Identifier: starts with a letter *)

let int_literal   =  digit+                     (* Optional '-' followed by digits *)
let float_literal = int_literal? '.' digit*         (*1.2,0.2,0.*)
let exp_literal = float_literal ('e' | 'E') ('+' | '-')? digit+ (*1.2e3, 1.2E3, 1.2e+3, 1.2e-3*)

let whitespace    = [' ' '\t' '\r' '\n' ]+          (* One or more whitespace characters *)
let newline      = "\\n"                      (* Newline character *)
let string_literal = "\"" [^'"']* "\""              (* Text inside double quotes *)
let sl_comment    = "//" [^'\n']*                  (* Single-line comment starting with // *)

(* Rules Section *)
rule token = parse
  (* Skip whitespace and comments *)
  | whitespace | sl_comment | newline  { token lexbuf }
  
  (*input() and input(filename) filename are without quotes*)
  | "input()" { INPUT "" }  (* input without filename *)
  | "input(" ([^')']+ as filename) ")" { INPUT filename }  (* input with filename *)
  
  (* Keywords and I/O - made consistent as lowercase *)
  | "print"  { PRINT }
  | "else if" { ELSEIF }
  | "if"     { IF }     | "else"   { ELSE }
  | "for"    { FOR }    | "while"  { WHILE }  | "let"    { LET }
  
  (* Type keywords *)
  | "bool"   { BOOLEAN } | "int"    { INTEGER } | "float"  { FLOAT }
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
  | exp_literal as e    { EXP_LITERAL(e) }
  | float_literal as f   { FLOAT_LITERAL(float_of_string f) }
  | int_literal as i     { INT_LITERAL(int_of_string i) }
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
    | INT_LITERAL i -> sprintf "INT_LITERAL(%d)" i
    | FLOAT_LITERAL f -> sprintf "FLOAT_LITERAL(%f)" f
    | EXP_LITERAL e -> sprintf "EXP_LITERAL(%s)" e
    | STRING_LITERAL s -> sprintf "STRING_LITERAL(%s)" s
    | IF -> "IF"
    | ELSEIF -> "ELSEIF"
    | ELSE -> "ELSE"
    | FOR -> "FOR"
    | WHILE -> "WHILE"
    | LET -> "LET"
    | BOOLEAN -> "BOOLEAN"
    | INTEGER -> "INTEGER"
    | FLOAT -> "FLOAT"
    | VECTOR -> "VECTOR"
    | MATRIX -> "MATRIX"
    | ID id -> sprintf "ID(%s)" id
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | MUL -> "MUL"
    | DIV -> "DIV"
    | MOD -> "MOD"
    | DOT -> "DOT"
    | ABS -> "ABS"
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