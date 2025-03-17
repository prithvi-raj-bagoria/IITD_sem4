%{
  open Ast
  open Lexing
  
  (* Helper function to get the current position in the source code *)
  let get_pos () =
    let pos = Parsing.symbol_start_pos () in
    "line " ^ string_of_int pos.pos_lnum ^ 
    ", character " ^ string_of_int (pos.pos_cnum - pos.pos_bol)
  
  (* Helper function for vector dimension checking *)
  let check_vector_dim dim elements =
    match dim with
    | d when d <> List.length elements ->
        raise (Failure ("Vector dimension mismatch at " ^ get_pos () ^ ": expected " ^ 
               string_of_int d ^ " elements, got " ^ string_of_int (List.length elements)))
    | _ -> ()
  
  (* Extract elements from vector literals *)
  let extract_vector_elements = function
    | Ast.VectorLit(_, elements) -> elements
    | _ -> failwith ("Expected vector literal in matrix row at " ^ get_pos ())
    
  (* Report type mismatch errors *)
  let type_error expected got =
    "Type mismatch at " ^ get_pos () ^ ": expected " ^ expected ^ ", got " ^ got
%}

/* Token declarations */
%token <bool> BOOL_LITERAL
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL ID INPUT
%token PRINT
%token BOOL INT FLOAT VECTOR MATRIX
%token AND OR NOT XOR
%token ABS PLUS MINUS MUL DIV MOD
%token EQ NEQ LT GT LEQ GEQ 
%token DOT MAG DIM ANGLE TRANS DET 
%token ASSIGN IF THEN ELSE FOR WHILE DO
%token SEMICOLON COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EOF

/* Precedence rules */
%nonassoc SEMICOLON
%nonassoc ELSE
%nonassoc THEN
%right ASSIGN
%left OR
%left AND 
%left XOR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left MUL DIV MOD
%right NOT
%nonassoc UMINUS
%left DOT
%nonassoc MAG DIM TRANS DET ABS ANGLE
%nonassoc LBRACKET

%start program
%type <Ast.program> program

%%

program:
  | stmt_list EOF { Program($1) }

stmt_list:
  | /* empty */ { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | expr SEMICOLON { ExprStmt($1) }
  | type_spec ID SEMICOLON { DeclStmt($2, $1, None) }
  | type_spec ID ASSIGN expr SEMICOLON { DeclStmt($2, $1, Some $4) }
  | ID ASSIGN expr SEMICOLON { AssignStmt($1, $3) }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE { IfStmt($3, $6, None) }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE { IfStmt($3, $6, Some $10) }
  | FOR LPAREN for_init SEMICOLON expr SEMICOLON for_update RPAREN LBRACE stmt_list RBRACE { ForStmt($3, $5, $7, $10) }
  | WHILE LPAREN while_cond RPAREN LBRACE stmt_list RBRACE { WhileStmt($3, $6) }
  | DO LBRACE stmt_list RBRACE WHILE LPAREN while_cond RPAREN SEMICOLON { DoWhileStmt($3, $7) }
  | LBRACE stmt_list RBRACE { Block($2) }

/* Special rules for for loop initialization and update, which can be different from regular statements */
for_init:
  | /* empty */ { ExprStmt(IntLit(0)) } /* default empty initialization */
  | expr { ExprStmt($1) }
  | type_spec ID ASSIGN expr { DeclStmt($2, $1, Some $4) }
  | ID ASSIGN expr { AssignStmt($1, $3) }
  ;

for_update:
  | /* empty */ { ExprStmt(IntLit(1)) } /* default empty update */
  | expr { ExprStmt($1) }
  | ID ASSIGN expr { AssignStmt($1, $3) }
  ;

/* Special rule for while loop condition to provide better error messages */
while_cond:
  | expr { $1 }
  | /* empty */ { 
      let pos = Parsing.symbol_start_pos () in
      raise (Failure ("Missing condition in while statement at line " ^ 
             string_of_int pos.pos_lnum ^ ", character " ^ 
             string_of_int (pos.pos_cnum - pos.pos_bol)))
    }
  ;

type_spec:
  | BOOL { BoolType }
  | INT { IntType }
  | FLOAT { FloatType }
  | VECTOR { VectorType(3) } /* Default to dimension 3 */
  | VECTOR INT_LITERAL { VectorType($2) } /* Explicit dimension */
  | MATRIX { MatrixType(0, 0) } /* Default unspecified dimensions */
  | MATRIX INT_LITERAL COMMA INT_LITERAL { MatrixType($2,$4) }

expr:
  | simple_expr { $1 }
  | expr PLUS expr { PLUS($1, $3) }
  | expr MINUS expr { MINUS($1, $3) }
  | expr MUL expr { TIMES($1, $3) }
  | expr DIV expr { DIV($1, $3) }
  | expr MOD expr { MOD($1, $3) }
  | MINUS expr %prec UMINUS { NEG($2) }
  | expr AND expr { AND($1, $3) }
  | expr OR expr { OR($1, $3) }
  | expr XOR expr { XOR($1, $3) }
  | NOT expr { NOT($2) }
  | expr EQ expr { EQ($1, $3) }
  | expr NEQ expr { NEQ($1, $3) }
  | expr LT expr { LT($1, $3) }
  | expr GT expr { GT($1, $3) }
  | expr LEQ expr { LEQ($1, $3) }
  | expr GEQ expr { GEQ($1, $3) }
  | expr DOT expr { DOT($1, $3) }
  | ABS LPAREN expr RPAREN { ABS($3) }
  | MAG expr { MAG($2) }
  | DIM expr { DIM($2) }
  | ANGLE LPAREN expr COMMA expr RPAREN { ANGLE($3, $5) }
  | TRANS expr { TRANS($2) }
  | DET expr { DET($2) }
  | array_access { $1 }

array_access:
  /* Restrict array access to IDs only. */
  | ID LBRACKET expr RBRACKET { Index(Var($1), $3, None) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET { Index(Var($1), $3, Some($6)) }

simple_expr:
  | INT_LITERAL LBRACKET int_list RBRACKET { VectorLit($1, $3) }
  | INT_LITERAL LBRACKET float_list RBRACKET { VectorLit($1, $3) }
  | INT_LITERAL COMMA INT_LITERAL LBRACKET row_list RBRACKET { MatrixLit($1, $3, $5) }
  | INT_LITERAL { IntLit($1) }
  | BOOL_LITERAL { BoolLit($1) }
  | FLOAT_LITERAL { FloatLit($1) }
  | STRING_LITERAL { StringLit($1) }
  | ID { Var($1) }
  | INPUT { Input(None) }
  | INPUT LPAREN STRING_LITERAL RPAREN { Input(Some $3) }
  | PRINT LPAREN expr RPAREN { Print($3) }
  | LPAREN expr RPAREN { $2 }

int_list:
  | INT_LITERAL  { [IntLit($1)] }
  | INT_LITERAL COMMA int_list  { IntLit($1) :: $3 }

float_list:
  | FLOAT_LITERAL  { [FloatLit($1)] }
  | FLOAT_LITERAL COMMA float_list  { FloatLit($1) :: $3 }
  
row:
  | LBRACKET int_list RBRACKET { $2 }
  | LBRACKET float_list RBRACKET { $2 }

row_list:
  | row { [$1] }
  | row COMMA row_list { $1 :: $3 }

%%
