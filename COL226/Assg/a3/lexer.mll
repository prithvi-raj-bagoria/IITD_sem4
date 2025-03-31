 {
  (* Header Section - Use tokens defined in parser.mly*)
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
}

(* Definitions Section *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = letter (letter | digit | '_' | '\'')* (*Identifier starts with letter only*)

let int_literal = digit+  (*Only for positive integers to handle 2-3 as INT_LITERAL(2) MINUS INT_LITERAL(3) otherwise it will give INT_LITERAL(2) INT_LITERAL(-3)*)
let float_literal = digit+ '.' digit* | digit* '.' digit+ (*Same only handling positive floats*)
let exp_literal = (float_literal | int_literal) ['e''E'] ['-''+']? digit+ (*Handling numbers integer|float e|E integer*)
let whitespace = [' ' '\t' '\r' ]+
let string_literal = '"' [^'"']* '"'
let sl_comment = "//" [^'\n']*

(* Rules Section *)
rule token = parse
  (* Skip whitespace and comments *)
  | whitespace { token lexbuf }
  | sl_comment "\n" { incr_lineno lexbuf; token lexbuf } (* Handle single-line comments separately *)

  | '\n'      { incr_lineno lexbuf; token lexbuf } (* Handle newlines and update position *)

  | "/*"          { comment 1 lexbuf }
  
  | "input(" [' ' '\t' '\r']* ([^')']* as filename) [' ' '\t' '\r']* ")" { INPUT(String.trim filename) } (*supp whitespace and also trimmed whitespace from filename*)
  | "print"  { PRINT }
  
  (* Type keywords *)
  | "bool"   { BOOL } 
  | "int"    { INT } 
  | "float"  { FLOAT }
  | "vector" { VECTOR }  
  | "matrix" { MATRIX }

  | "abs"   { ABS }
  | "sqrt"  { SQRT }  

  (* Vector and Matrix operations *)
  | "dot"     { DOT }    
  | "dim"   { DIM }
  | "mag"   { MAG }
  | "trans" { TRANS }
  | "angle" { ANGLE }
  | "det"   { DET }
  | "trace" { TRACE }
  
  (* Control keywords - MOVED BEFORE IDENTIFIERS *)
  | "if"    { IF }     
  | "else"  { ELSE }
  | "for"   { FOR }    
  | "while" { WHILE }  
  
  (*--Data Section--*)
  (* Literals/Constants *)
  | "true" | "false" as bl {BOOL_LITERAL(bool_of_string bl)}
  | exp_literal as e  { FLOAT_LITERAL(float_of_string e) }
  | float_literal as f { FLOAT_LITERAL(float_of_string f) }
  | int_literal as i   { INT_LITERAL(int_of_string i) }
  | id as name      { ID(name) }

  (* Logical Operators *)
  | "&&"    { AND }    
  | "||"    { OR }     
  | "!"     { NOT } 
  | "^"     { XOR }

  (* Arithmetic Operators some are overloade for matrix/vector addition or multiplication*)
  | "**"    { POWER }
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
  | _ as c { raise (SyntaxError ("Unexpected Character: " ^ (String.make 1 c))) }

(* Handling nested comments *)
and comment level = parse
  | "/*"     { comment (level + 1) lexbuf }
  | "*/"     { if level = 1 then token lexbuf else comment (level - 1) lexbuf }
  | '\n'     { incr_lineno lexbuf; comment level lexbuf }  (* Make sure to update line numbers in comments *)
  | _        { comment level lexbuf }
  | eof      { raise (SyntaxError "Unterminated comment") }
