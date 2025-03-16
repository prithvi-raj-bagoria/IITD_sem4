(* main.ml - Driver program for Matrix/Vector DSL *)
open Printf
open Lexing
open Parser
open Ast
open Tokens
(* Open Typechecker last to ensure its definitions take precedence *)
open Typechecker


(* Helper function to display position information *)
let position_string pos =
  sprintf "Line %d, characters %d-%d" 
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    (pos.pos_cnum - pos.pos_bol + 1)

(* Function to convert type to string *)
let string_of_type = function
  | BoolType -> "BoolType"
  | IntType -> "IntType"
  | FloatType -> "FloatType"
  (* StringType and ErrorType are not defined in dtype, removing these cases *)
  | VectorType dim -> 
      (match dim with 
       | None -> "VectorType(None)" 
       | Some d -> sprintf "VectorType(Some(%d))" d)
  | MatrixType (rows, cols) -> 
      sprintf "MatrixType(%s, %s)"
        (match rows with None -> "None" | Some r -> sprintf "Some(%d)" r)
        (match cols with None -> "None" | Some c -> sprintf "Some(%d)" c)

(* Function to convert AST to string for display *)
let rec string_of_expr = function
  | BoolLit b -> sprintf "BoolLit(%b)" b
  | IntLit i -> sprintf "IntLit(%d)" i
  | FloatLit f -> sprintf "FloatLit(%f)" f
  | StringLit s -> sprintf "StringLit(\"%s\")" s
  | VectorLit (dim, elems) -> 
      sprintf "VectorLit(%s, [%s])" 
        (match dim with None -> "None" | Some d -> string_of_int d)
        (String.concat ", " (List.map string_of_expr elems))
  | MatrixLit (rows, cols, row_lists) ->
      sprintf "MatrixLit(%s, %s, [%s])"
        (match rows with None -> "None" | Some r -> string_of_int r)
        (match cols with None -> "None" | Some c -> string_of_int c)
        (String.concat ", " 
          (List.map 
            (fun row -> "[" ^ String.concat ", " (List.map string_of_expr row) ^ "]")
            row_lists))
  | Var name -> sprintf "Var(%s)" name
  | PLUS (e1, e2) -> sprintf "PLUS(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | MINUS (e1, e2) -> sprintf "MINUS(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | TIMES (e1, e2) -> sprintf "TIMES(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | DIV (e1, e2) -> sprintf "DIV(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | MOD (e1, e2) -> sprintf "MOD(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | NEG e -> sprintf "NEG(%s)" (string_of_expr e)
  | EQ (e1, e2) -> sprintf "EQ(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | NEQ (e1, e2) -> sprintf "NEQ(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | LT (e1, e2) -> sprintf "LT(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | GT (e1, e2) -> sprintf "GT(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | LEQ (e1, e2) -> sprintf "LEQ(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | GEQ (e1, e2) -> sprintf "GEQ(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | AND (e1, e2) -> sprintf "AND(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | OR (e1, e2) -> sprintf "OR(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | XOR (e1, e2) -> sprintf "XOR(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | NOT e -> sprintf "NOT(%s)" (string_of_expr e)
  | DOT (e1, e2) -> sprintf "DOT(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | MAG e -> sprintf "MAG(%s)" (string_of_expr e)
  | DIM e -> sprintf "DIM(%s)" (string_of_expr e)
  | ANGLE (e1, e2) -> sprintf "ANGLE(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | TRANS e -> sprintf "TRANS(%s)" (string_of_expr e)
  | DET e -> sprintf "DET(%s)" (string_of_expr e)
  | ABS e -> sprintf "ABS(%s)" (string_of_expr e)
  | Assign (id, e) -> sprintf "Assign(%s, %s)" id (string_of_expr e)
  | Input None -> "Input(None)"
  | Input (Some e) -> sprintf "Input(Some(%s))" (string_of_expr e)
  | Print e -> sprintf "Print(%s)" (string_of_expr e)
  | Index (e1, e2, e3_opt) -> 
      sprintf "Index(%s, %s, %s)" 
        (string_of_expr e1) 
        (string_of_expr e2) 
        (match e3_opt with None -> "None" | Some e3 -> "Some(" ^ string_of_expr e3 ^ ")")

let rec string_of_stmt indent = function
  | ExprStmt e -> indent ^ "ExprStmt(" ^ string_of_expr e ^ ")"
  | DeclStmt (id, typ, init_opt) ->
      sprintf "%sDeclStmt(%s, %s, %s)" indent id
        (string_of_type typ)
        (match init_opt with
         | None -> "None"
         | Some e -> "Some(" ^ string_of_expr e ^ ")")
  | AssignStmt (id, e) -> 
      sprintf "%sAssignStmt(%s, %s)" indent id (string_of_expr e)
  | IfStmt (cond, then_stmt, else_opt) ->
      sprintf "%sIfStmt(\n%s,\n%s,\n%s\n%s)" 
        indent
        (indent ^ "  " ^ string_of_expr cond)
        (string_of_stmt (indent ^ "  ") then_stmt)
        (match else_opt with
         | None -> indent ^ "  None"
         | Some s -> string_of_stmt (indent ^ "  ") s)
        indent
  | ForStmt (init, cond, incr, body) ->
      sprintf "%sForStmt(\n%s,\n%s,\n%s,\n%s\n%s)"
        indent
        (indent ^ "  " ^ string_of_expr init)
        (indent ^ "  " ^ string_of_expr cond)
        (indent ^ "  " ^ string_of_expr incr)
        (string_of_stmt (indent ^ "  ") body)
        indent
  | WhileStmt (cond, body) ->
      sprintf "%sWhileStmt(\n%s,\n%s\n%s)"
        indent
        (indent ^ "  " ^ string_of_expr cond)
        (string_of_stmt (indent ^ "  ") body)
        indent
  | DoWhileStmt (body, cond) ->
      sprintf "%sDoWhileStmt(\n%s,\n%s\n%s)"
        indent
        (string_of_stmt (indent ^ "  ") body)
        (indent ^ "  " ^ string_of_expr cond)
        indent
  | Block stmts ->
      sprintf "%sBlock[\n%s\n%s]"
        indent
        (String.concat ",\n" (List.map (string_of_stmt (indent ^ "  ")) stmts))
        indent

let string_of_program (Program stmts) =
  sprintf "Program[\n%s\n]"
    (String.concat ",\n" (List.map (string_of_stmt "  ") stmts))

(* Token conversion function to bridge between Tokens.token and Parser.token *)
let convert_token token =
  match token with
  | Tokens.INT_POS_LITERAL i -> Parser.INT_LITERAL i
  | Tokens.FLOAT_LITERAL f -> Parser.FLOAT_LITERAL f
  | Tokens.STRING_LITERAL s -> Parser.STRING_LITERAL s
  | Tokens.ID s -> Parser.ID s
  | Tokens.INPUT _ -> Parser.INPUT
  | Tokens.PRINT -> Parser.PRINT
  | Tokens.BOOLEAN -> Parser.BOOL
  | Tokens.INT_POS -> Parser.INT
  | Tokens.FLOAT -> Parser.FLOAT
  | Tokens.VECTOR -> Parser.VECTOR
  | Tokens.MATRIX -> Parser.MATRIX
  | Tokens.IF -> Parser.IF
  | Tokens.ELSE -> Parser.ELSE
  | Tokens.FOR -> Parser.FOR
  | Tokens.WHILE -> Parser.WHILE
  | Tokens.DO -> Parser.DO
  | Tokens.BOOL_LITERAL true -> Parser.TRUE
  | Tokens.BOOL_LITERAL false -> Parser.FALSE
  | Tokens.AND -> Parser.AND
  | Tokens.OR -> Parser.OR
  | Tokens.NOT -> Parser.NOT
  | Tokens.XOR -> Parser.XOR
  | Tokens.DOT -> Parser.DOT
  | Tokens.MAG -> Parser.MAG
  | Tokens.DIM -> Parser.DIM
  | Tokens.ANGLE -> Parser.ANGLE
  | Tokens.TRANS -> Parser.TRANS
  | Tokens.DET -> Parser.DET
  | Tokens.ABS -> Parser.ABS
  | Tokens.PLUS -> Parser.PLUS
  | Tokens.MINUS -> Parser.MINUS
  | Tokens.MUL -> Parser.MUL
  | Tokens.DIV -> Parser.DIV
  | Tokens.MOD -> Parser.MOD
  | Tokens.EQ -> Parser.EQ
  | Tokens.NEQ -> Parser.NEQ
  | Tokens.LT -> Parser.LT
  | Tokens.GT -> Parser.GT
  | Tokens.LEQ -> Parser.LEQ
  | Tokens.GEQ -> Parser.GEQ
  | Tokens.ASSIGN -> Parser.ASSIGN
  | Tokens.SEMICOLON -> Parser.SEMICOLON
  | Tokens.COMMA -> Parser.COMMA
  | Tokens.LPAREN -> Parser.LPAREN
  | Tokens.RPAREN -> Parser.RPAREN
  | Tokens.LBRACE -> Parser.LBRACE
  | Tokens.RBRACE -> Parser.RBRACE
  | Tokens.LBRACKET -> Parser.LBRACKET
  | Tokens.RBRACKET -> Parser.RBRACKET
  | Tokens.EOF -> Parser.EOF

(* Wrapper function for the lexer *)
let lexer_wrapper lexbuf =
  let token = Lexer.token lexbuf in
  convert_token token

(* Main function *)
let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    (* Set position tracking for errors *)
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
    
    try
      (* Parse the input, using the wrapper to convert tokens *)
      let ast = Parser.program lexer_wrapper lexbuf in
      
      (* Type check the program *)
      Typechecker.typecheck_program ast;
      
      (* Print ONLY the AST - no token output *)
      printf "%s\n" (string_of_program ast);
      
    with
    | Lexer.LexError msg ->
        fprintf stderr "Lexical error at %s: %s\n" 
          (position_string lexbuf.lex_curr_p) msg
    | Typechecker.Type_error msg ->
        fprintf stderr "Type error: %s\n" msg
    | _ as e -> (* Catch all other exceptions, including Parser.Error *)
        match Printexc.to_string e with
        | "Parser.Error" -> 
            fprintf stderr "Syntax error at %s\n" 
              (position_string lexbuf.lex_curr_p)
        | msg ->
            fprintf stderr "Unexpected error: %s\n" msg
  with
  | Sys_error msg ->
      fprintf stderr "System error: %s\n" msg
