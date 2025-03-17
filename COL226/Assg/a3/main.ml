open Ast
open Lexer
open Parser
open Lexing

(* Helper for printing positions *)
let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  "line " ^ string_of_int pos.pos_lnum ^ 
  ", character " ^ string_of_int (pos.pos_cnum - pos.pos_bol)

(* Function to convert token to string for display *)
let token_to_string = function
  | BOOL_LITERAL(b) -> "BOOL_LITERAL(" ^ string_of_bool b ^ ")"
  | INT_LITERAL(i) -> "INT_LITERAL(" ^ string_of_int i ^ ")"
  | FLOAT_LITERAL(f) -> "FLOAT_LITERAL(" ^ string_of_float f ^ ")"
  | STRING_LITERAL(s) -> "STRING_LITERAL(\"" ^ s ^ "\")"
  | ID(s) -> "ID(\"" ^ s ^ "\")"
  | INPUT(s) -> "INPUT(\"" ^ s ^ "\")"
  | PRINT -> "PRINT"
  | BOOL -> "BOOL"
  | INT -> "INT"
  | FLOAT -> "FLOAT"
  | VECTOR -> "VECTOR"
  | MATRIX -> "MATRIX"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | XOR -> "XOR"
  | ABS -> "ABS"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | GT -> "GT"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"
  | DOT -> "DOT"
  | MAG -> "MAG"
  | DIM -> "DIM"
  | ANGLE -> "ANGLE"
  | TRANS -> "TRANS"
  | DET -> "DET"
  | ASSIGN -> "ASSIGN"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | DO -> "DO"
  | SEMICOLON -> "SEMICOLON"
  | COMMA -> "COMMA"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | EOF -> "EOF"

(* Function to collect all tokens from input *)
let collect_tokens lexbuf =
  let rec loop tokens =
    let token = Lexer.token lexbuf in
    match token with
    | EOF -> List.rev (token :: tokens)  (* Include EOF token *)
    | _ -> loop (token :: tokens)
  in
  loop []

(* Function to pretty-print ASTs *)
let rec string_of_expr = function
  | BoolLit(b) -> "BoolLit(" ^ string_of_bool b ^ ")"
  | IntLit(i) -> "IntLit(" ^ string_of_int i ^ ")"
  | FloatLit(f) -> "FloatLit(" ^ string_of_float f ^ ")"
  | StringLit(s) -> "StringLit(\"" ^ s ^ "\")"
  | Var(id) -> "Var(\"" ^ id ^ "\")"
  | PLUS(e1, e2) -> "PLUS(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | MINUS(e1, e2) -> "MINUS(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | TIMES(e1, e2) -> "TIMES(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | DIV(e1, e2) -> "DIV(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | MOD(e1, e2) -> "MOD(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | NEG(e) -> "NEG(" ^ string_of_expr e ^ ")"
  | AND(e1, e2) -> "AND(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | OR(e1, e2) -> "OR(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | XOR(e1, e2) -> "XOR(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | NOT(e) -> "NOT(" ^ string_of_expr e ^ ")"
  | EQ(e1, e2) -> "EQ(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | NEQ(e1, e2) -> "NEQ(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | LT(e1, e2) -> "LT(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | GT(e1, e2) -> "GT(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | LEQ(e1, e2) -> "LEQ(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | GEQ(e1, e2) -> "GEQ(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | DOT(e1, e2) -> "DOT(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | MAG(e) -> "MAG(" ^ string_of_expr e ^ ")"
  | ABS(e) -> "ABS(" ^ string_of_expr e ^ ")"
  | DIM(e) -> "DIM(" ^ string_of_expr e ^ ")"
  | ANGLE(e1, e2) -> "ANGLE(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | TRANS(e) -> "TRANS(" ^ string_of_expr e ^ ")"
  | DET(e) -> "DET(" ^ string_of_expr e ^ ")"
  | VectorLit(dim, elements) -> 
      let dim_str = match dim with 
        | d -> string_of_int d 
      in
      "Vector(" ^ dim_str ^ ", [" ^ String.concat "; " (List.map string_of_expr elements) ^ "])"
  | MatrixLit(rows, cols, elements) -> 
      let rows_str =  string_of_int rows in
      let cols_str =   string_of_int cols in
      "Matrix(" ^ rows_str ^ " x " ^ cols_str ^ ", [" ^ 
        String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_expr row) ^ "]") elements) ^ "])"
  | Index(e, idx1, idx2_opt) ->
      let idx2_str = match idx2_opt with
        | None -> ""
        | Some idx2 -> ", " ^ string_of_expr idx2
      in
      "Index(" ^ string_of_expr e ^ ", " ^ string_of_expr idx1 ^ idx2_str ^ ")"
  | Input(None) -> "Input()"
  | Input(Some s) -> "Input(\"" ^ s ^ "\")"  (* Changed: removed Some *)
  | Print(e) -> "Print(" ^ string_of_expr e ^ ")"
  | Assign(id, e) -> "Assign(\"" ^ id ^ "\", " ^ string_of_expr e ^ ")"

let rec string_of_stmt = function
  | ExprStmt(e) -> "ExprStmt(" ^ string_of_expr e ^ ")"
  | DeclStmt(id, typ, None) -> "DeclStmt(\"" ^ id ^ "\", " ^ string_of_type typ ^ ")"
  | DeclStmt(id, typ, Some e) -> "DeclStmt(\"" ^ id ^ "\", " ^ string_of_type typ ^ ", " ^ string_of_expr e ^ ")"
  | AssignStmt(id, e) -> "AssignStmt(\"" ^ id ^ "\", " ^ string_of_expr e ^ ")"
  | IfStmt(cond, then_block, else_block_opt) ->
      let else_str = match else_block_opt with
        | None -> "no else"
        | Some block -> string_of_block block
      in
      "IfStmt(" ^ string_of_expr cond ^ ", " ^ string_of_block then_block ^ 
      (if else_block_opt <> None then ", else: " ^ else_str else "") ^ ")"
  | WhileStmt(cond, block) -> "WhileStmt(" ^ string_of_expr cond ^ ", " ^ string_of_block block ^ ")"
  | DoWhileStmt(block, cond) -> "DoWhileStmt(" ^ string_of_block block ^ ", " ^ string_of_expr cond ^ ")"
  | ForStmt(init, cond, update, block) ->
      "ForStmt(" ^ string_of_stmt init ^ ", " ^ string_of_expr cond ^ ", " ^ 
      string_of_stmt update ^ ", " ^ string_of_block block ^ ")"
  | Block(stmts) -> string_of_block stmts

and string_of_block stmts = "Block[" ^ String.concat "; " (List.map string_of_stmt stmts) ^ "]"

and string_of_type = function
  | BoolType -> "bool"
  | IntType -> "int"
  | FloatType -> "float"
  | VectorType(dim) -> "vector(" ^ string_of_int dim ^ ")"
  | MatrixType(r, c) -> 
      if r = 0 && c = 0 then "matrix" 
      else "matrix(" ^ string_of_int r ^ "," ^ string_of_int c ^ ")"

let string_of_program = function
  | Program(stmts) -> "Program([" ^ String.concat "; " (List.map string_of_stmt stmts) ^ "])"

(* Function to pretty-print ASTs with tree structure *)
let rec string_of_expr_tree expr indent =
  let node_indent = indent ^ "├── " in
  let last_indent = indent ^ "└── " in
  let child_indent = indent ^ "│   " in
  let last_child_indent = indent ^ "    " in
  
  match expr with
  | BoolLit(b) -> node_indent ^ "BoolLit: " ^ string_of_bool b
  | IntLit(i) -> node_indent ^ "IntLit: " ^ string_of_int i
  | FloatLit(f) -> node_indent ^ "FloatLit: " ^ string_of_float f
  | StringLit(s) -> node_indent ^ "StringLit: \"" ^ s ^ "\""
  | Var(id) -> node_indent ^ "Var: " ^ id
  | PLUS(e1, e2) -> 
      node_indent ^ "PLUS\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | MINUS(e1, e2) -> 
      node_indent ^ "MINUS\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | TIMES(e1, e2) -> 
      node_indent ^ "TIMES\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | DIV(e1, e2) -> 
      node_indent ^ "DIV\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | MOD(e1, e2) -> 
      node_indent ^ "MOD\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | NEG(e) -> 
      node_indent ^ "NEG\n" ^ 
      string_of_expr_tree e last_child_indent
  | AND(e1, e2) -> 
      node_indent ^ "AND\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | OR(e1, e2) -> 
      node_indent ^ "OR\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | XOR(e1, e2) -> 
      node_indent ^ "XOR\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | NOT(e) -> 
      node_indent ^ "NOT\n" ^ 
      string_of_expr_tree e last_child_indent
  | EQ(e1, e2) -> 
      node_indent ^ "EQ\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | NEQ(e1, e2) -> 
      node_indent ^ "NEQ\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | LT(e1, e2) -> 
      node_indent ^ "LT\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | GT(e1, e2) -> 
      node_indent ^ "GT\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | LEQ(e1, e2) -> 
      node_indent ^ "LEQ\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | GEQ(e1, e2) -> 
      node_indent ^ "GEQ\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | DOT(e1, e2) -> 
      node_indent ^ "DOT\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | MAG(e) -> 
      node_indent ^ "MAG\n" ^ 
      string_of_expr_tree e last_child_indent
  | ABS(e) -> 
      node_indent ^ "ABS\n" ^ 
      string_of_expr_tree e last_child_indent
  | DIM(e) -> 
      node_indent ^ "DIM\n" ^ 
      string_of_expr_tree e last_child_indent
  | ANGLE(e1, e2) -> 
      node_indent ^ "ANGLE\n" ^ 
      string_of_expr_tree e1 child_indent ^ "\n" ^
      string_of_expr_tree e2 last_child_indent
  | TRANS(e) -> 
      node_indent ^ "TRANS\n" ^ 
      string_of_expr_tree e last_child_indent
  | DET(e) -> 
      node_indent ^ "DET\n" ^ 
      string_of_expr_tree e last_child_indent
  | VectorLit(dim, elements) -> 
      node_indent ^ "VectorLit (dim=" ^ string_of_int dim ^ ")\n" ^
      string_list_expr_tree elements (child_indent ^ "│  ") (last_child_indent ^ "   ")
  | MatrixLit(rows, cols, elements) -> 
      node_indent ^ "MatrixLit (" ^ string_of_int rows ^ "x" ^ string_of_int cols ^ ")\n" ^
      string_matrix_expr_tree elements (child_indent ^ "│  ") (last_child_indent ^ "   ")
  | Index(e, idx1, idx2_opt) ->
      let idx_str = match idx2_opt with
        | None -> 
            node_indent ^ "Index\n" ^ 
            string_of_expr_tree e child_indent ^ "\n" ^
            last_indent ^ "idx: " ^ string_of_expr idx1
        | Some idx2 -> 
            node_indent ^ "Index\n" ^ 
            string_of_expr_tree e child_indent ^ "\n" ^
            node_indent ^ "row: " ^ string_of_expr idx1 ^ "\n" ^
            last_indent ^ "col: " ^ string_of_expr idx2
      in
      idx_str
  | Input(None) -> node_indent ^ "Input"
  | Input(Some s) -> node_indent ^ "Input: \"" ^ s ^ "\""
  | Print(e) -> 
      node_indent ^ "Print\n" ^ 
      string_of_expr_tree e last_child_indent
  | Assign(id, e) -> 
      node_indent ^ "Assign\n" ^ 
      node_indent ^ "│   var: " ^ id ^ "\n" ^
      string_of_expr_tree e (last_child_indent ^ "    ")

and string_list_expr_tree exprs indent last_indent =
  match exprs with
  | [] -> ""
  | [e] -> string_of_expr_tree e last_indent
  | e::es -> string_of_expr_tree e indent ^ "\n" ^ string_list_expr_tree es indent last_indent

and string_matrix_expr_tree rows indent last_indent =
  match rows with
  | [] -> ""
  | [row] -> last_indent ^ "Row\n" ^ string_list_expr_tree row (last_indent ^ "    ") (last_indent ^ "    ")
  | row::rows -> 
      indent ^ "Row\n" ^ 
      string_list_expr_tree row (indent ^ "    ") (indent ^ "    ") ^ "\n" ^
      string_matrix_expr_tree rows indent last_indent

let rec string_of_stmt_tree stmt indent =
  let node_indent = indent ^ "├── " in
  let last_indent = indent ^ "└── " in
  let child_indent = indent ^ "│   " in
  let last_child_indent = indent ^ "    " in
  
  match stmt with
  | ExprStmt(e) -> 
      node_indent ^ "ExprStmt\n" ^ 
      string_of_expr_tree e last_child_indent
  | DeclStmt(id, typ, None) -> 
      node_indent ^ "DeclStmt\n" ^ 
      node_indent ^ "│   var: " ^ id ^ "\n" ^
      last_indent ^ "type: " ^ string_of_type typ
  | DeclStmt(id, typ, Some e) -> 
      node_indent ^ "DeclStmt\n" ^ 
      node_indent ^ "│   var: " ^ id ^ "\n" ^
      node_indent ^ "│   type: " ^ string_of_type typ ^ "\n" ^
      string_of_expr_tree e last_child_indent
  | AssignStmt(id, e) -> 
      node_indent ^ "AssignStmt\n" ^ 
      node_indent ^ "│   var: " ^ id ^ "\n" ^
      string_of_expr_tree e last_child_indent
  | IfStmt(cond, then_block, None) -> 
      node_indent ^ "IfStmt\n" ^ 
      node_indent ^ "│   condition:\n" ^ 
      string_of_expr_tree cond (child_indent ^ "│   ") ^ "\n" ^
      last_indent ^ "then:\n" ^ 
      string_of_block_tree then_block last_child_indent
  | IfStmt(cond, then_block, Some else_block) -> 
      node_indent ^ "IfStmt\n" ^ 
      node_indent ^ "│   condition:\n" ^ 
      string_of_expr_tree cond (child_indent ^ "│   ") ^ "\n" ^
      node_indent ^ "│   then:\n" ^ 
      string_of_block_tree then_block (child_indent ^ "│   ") ^ "\n" ^
      last_indent ^ "else:\n" ^ 
      string_of_block_tree else_block last_child_indent
  | WhileStmt(cond, block) -> 
      node_indent ^ "WhileStmt\n" ^ 
      node_indent ^ "│   condition:\n" ^ 
      string_of_expr_tree cond (child_indent ^ "│   ") ^ "\n" ^
      last_indent ^ "body:\n" ^ 
      string_of_block_tree block last_child_indent
  | DoWhileStmt(block, cond) -> 
      node_indent ^ "DoWhileStmt\n" ^ 
      node_indent ^ "│   body:\n" ^ 
      string_of_block_tree block (child_indent ^ "│   ") ^ "\n" ^
      last_indent ^ "condition:\n" ^ 
      string_of_expr_tree cond (last_child_indent ^ "│   ")
  | ForStmt(init, cond, update, block) -> 
      node_indent ^ "ForStmt\n" ^ 
      node_indent ^ "│   init:\n" ^ 
      string_of_stmt_tree init (child_indent ^ "│   ") ^ "\n" ^
      node_indent ^ "│   condition:\n" ^ 
      string_of_expr_tree cond (child_indent ^ "│   ") ^ "\n" ^
      node_indent ^ "│   update:\n" ^ 
      string_of_stmt_tree update (child_indent ^ "│   ") ^ "\n" ^
      last_indent ^ "body:\n" ^ 
      string_of_block_tree block last_child_indent
  | Block(stmts) -> 
      node_indent ^ "Block\n" ^ 
      string_of_block_tree stmts last_child_indent

and string_of_block_tree stmts indent =
  match stmts with
  | [] -> indent ^ "Empty block"
  | [s] -> string_of_stmt_tree s indent
  | s::ss -> string_of_stmt_tree s indent ^ "\n" ^ string_of_block_tree ss indent

let string_of_program_tree = function
  | Program(stmts) -> 
      "Program\n" ^ string_of_block_tree stmts ""

(* Main driver *)
let () =
  try
    (* Check for command line arguments *)
    let filename = 
      if Array.length Sys.argv > 1 then
        Sys.argv.(1)
      else
        begin
          Printf.eprintf "Usage: %s <input_file>\n" Sys.argv.(0);
          exit 1
        end
    in
    
    (* Read input from file *)
    let input_channel = open_in filename in
    let input = ref "" in
    try
      while true do
        input := !input ^ (input_line input_channel) ^ "\n"
      done
    with End_of_file -> 
      close_in input_channel;
      
    Printf.printf "Processing file: %s\n" filename;
    
    (* Create lexing buffer from input *)
    let lexbuf = Lexing.from_string !input in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                           pos_fname = filename;
                           pos_lnum = 1;
                         };
    
    (* First, collect and print all tokens *)
    print_endline "Lexical tokens:";
    let tokens = collect_tokens lexbuf in
    List.iter (fun token -> print_endline (token_to_string token)) tokens;
    print_endline "";
    
    (* Reset lexbuf for parsing *)
    let lexbuf = Lexing.from_string !input in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                           pos_fname = filename;
                           pos_lnum = 1;
                         };
    
    (* Parse the input and print AST *)
    try
      print_endline "Parsing input...";
      let ast = Parser.program Lexer.token lexbuf in
      
      print_endline "\nAbstract Syntax Tree:";
      print_endline (string_of_program_tree ast)
    with
    | Parsing.Parse_error ->
        prerr_endline ("Syntax error at " ^ print_position lexbuf)
    
  with
  | Sys_error msg -> prerr_endline ("System error: " ^ msg)
  | Failure msg -> prerr_endline ("Error: " ^ msg)
  | e -> prerr_endline ("Unexpected error: " ^ Printexc.to_string e)
