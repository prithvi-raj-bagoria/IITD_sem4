{
  (* Header Section - Use Parser tokens *)
  open Parser
  open Printf
  open Lexing
  
  (* Custom exception for lexical errors *)
  exception SyntaxError of string

  (* Update the current line number *)
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum
    }

  (* Error reporting *)
  let report_error lexbuf msg =
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol in
    Printf.fprintf stderr "Lexical error at line %d, character %d: %s\n" line col msg;
    failwith "Lexical error"
}

(* Definitions Section *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = (letter | digit) (letter | digit | '_' | '\'')*

let int_literal = digit+  (*Only for positive integers*)
let float_literal = digit+ '.' digit* | digit* '.' digit+
let exp_literal = (float_literal | int_literal) ['e''E'] ['-''+']? digit+
let whitespace = [' ' '\t' '\r' ]+
let string_literal = '"' [^'"']* '"'
let sl_comment = "//" [^'\n']*

(* Rules Section *)
rule token = parse
  (* Skip whitespace and comments *)
  | whitespace | sl_comment    { token lexbuf }

  | '\n'      { incr_lineno lexbuf; token lexbuf } (* Handle newlines and update position *)

  | "/*"          { comment 1 lexbuf }
  
  (* I/O Commands *)
  | "input()" { INPUT("") }
  | "input(" ([^')']+ as filename) ")" { INPUT(filename) }
  | "print"  { PRINT }
  
  (* Type keywords *)
  | "bool"   { BOOL } 
  | "int"    { INT } 
  | "float"  { FLOAT }
  | "vector" { VECTOR }  
  | "matrix" { MATRIX }

  | "abs"   { ABS }

    (* Vector and Matrix operations *)
  | "."     { DOT }    
  | "dim"   { DIM }
  | "mag"   { MAG }
  | "trans" { TRANS }
  | "angle" { ANGLE }
  | "det"   { DET }
  
  (* Control keywords - MOVED BEFORE IDENTIFIERS *)
  | "if"    { IF }     
  | "then"  { THEN }
  | "else"  { ELSE }
  | "for"   { FOR }    
  | "while" { WHILE }  
  | "do"    { DO }
  
  (*--Data Section--*)
  (* Literals/Constants *)
  | "true" | "false" as bl {BOOL_LITERAL(bool_of_string bl)}
  | exp_literal as e  { FLOAT_LITERAL(float_of_string e) }
  | float_literal as f { FLOAT_LITERAL(float_of_string f) }
  | int_literal as i   { INT_LITERAL(int_of_string i) }
  | string_literal as s  { STRING_LITERAL(String.sub s 1 (String.length s - 2)) }
  | id as name      { ID(name) }  (* Moved identifiers AFTER keywords *)

  (* Logical Operators *)
  | "&&"    { AND }    
  | "||"    { OR }     
  | "!"     { NOT } 
  | "^"     { XOR }

  (* Arithmetic Operators some are overloade for matrix/vector addition or multiplication*)
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { MUL }
  | "/"     { DIV }
  | "%"     { MOD }

  (* Comparison Operators *)
  | "=="    { EQ }     
  | "!="    { NEQ }
  | "<="    { LEQ }    
  | ">="    { GEQ }
  | "<"     { LT }     
  | ">"     { GT }
  


  (* Control keywords *)
  | ":="    { ASSIGN }
  
  (* Delimiters *)
  | ";"     { SEMICOLON }
  | "{"     { LBRACE }  
  | "}"     { RBRACE }
  | "("     { LPAREN }  
  | ")"     { RPAREN }
  | "["     { LBRACKET }
  | "]"     { RBRACKET }
  | ","     { COMMA }
  
  | eof     { EOF }
  | _ as c  { report_error lexbuf ("Unexpected character: " ^ String.make 1 c) }

(* Handling nested comments *)
and comment level = parse
  | "/*"     { comment (level + 1) lexbuf }
  | "*/"     { if level = 1 then token lexbuf else comment (level - 1) lexbuf }
  | _        { comment level lexbuf }
  | eof      { raise (SyntaxError "Unterminated comment") }
