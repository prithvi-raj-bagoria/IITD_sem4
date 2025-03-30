%{
  open Ast
  open Lexing
  
  (* Helper function to get the current position in the source code *)
  let get_pos () =
    let pos = Parsing.symbol_start_pos () in
    "line " ^ string_of_int pos.pos_lnum ^ 
    ", character " ^ string_of_int (pos.pos_cnum - pos.pos_bol)
  
  (* Extract elements from vector literals *)
  let extract_vector_elements = function
    | Ast.VectorLit(_, elements) -> elements
    | _ -> failwith ("Expected vector literal in matrix row at " ^ get_pos ())
    
%}

/* Token declarations */
%token <bool> BOOL_LITERAL
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string>  ID INPUT
%token PRINT
%token BOOL INT FLOAT VECTOR MATRIX
%token AND OR NOT XOR
%token ABS SQRT PLUS MINUS MUL DIV MOD POWER  
%token EQ NEQ LT GT LEQ GEQ 
%token DOT MAG DIM ANGLE TRANS DET TRACE
%token ASSIGN IF ELSE FOR WHILE 
%token SEMICOLON COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EOF

/* Precedence rules */
%nonassoc LOWEST  /* Lowest precedence (empty productions) */
%nonassoc SEMICOLON ELSE
%right ASSIGN
%left OR
%left XOR
%left AND 
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left MUL DIV MOD
%nonassoc UMINUS
%right POWER  /* -2**3 = -(2**3) */
%right NOT  /* Logical NOT should be at same level as UMINUS */
%nonassoc MAG DIM TRANS DET ABS SQRT ANGLE TRACE DOT /* Function calls */
%nonassoc LBRACKET MATRIX_COMMA VECTOR_BRACKET  /* Highest precedence - array access */

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
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET ASSIGN expr SEMICOLON { 
      (* Handle matrix element assignment A[i][j] := expr using existing Assign node *)
      ExprStmt(Assign($1, $9)) 
    }
  | ID LBRACKET expr RBRACKET ASSIGN expr SEMICOLON { 
      (* Handle vector element assignment A[i] := expr using existing Assign node *)
      ExprStmt(Assign($1, $6)) 
    }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE { IfStmt($3, $6, None) }
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE { IfStmt($3, $6, Some $10) }
  | FOR LPAREN for_init SEMICOLON expr SEMICOLON for_update RPAREN LBRACE stmt_list RBRACE { ForStmt($3, $5, $7, $10) }
  | WHILE LPAREN while_cond RPAREN LBRACE stmt_list RBRACE { WhileStmt($3, $6) }
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
  | VECTOR { VectorType }
  | MATRIX { MatrixType }

expr:
  | simple_expr { $1 }
  | expr PLUS expr { PLUS($1, $3) }
  | expr MINUS expr { MINUS($1, $3) }
  | expr MUL expr { MUL($1, $3) }  
  | expr DIV expr { DIV($1, $3) }
  | expr MOD expr { MOD($1, $3) }
  | expr POWER expr { POWER($1, $3) }
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
  | SQRT LPAREN expr RPAREN { SQRT($3) }  /* Added rule for square root */
  | MAG LPAREN expr RPAREN { MAG($3) }
  | DIM LPAREN expr RPAREN { DIM($3) }
  | ANGLE LPAREN expr COMMA expr RPAREN { ANGLE($3, $5) }
  | TRANS LPAREN expr RPAREN { TRANS($3) }
  | DET LPAREN expr RPAREN { DET($3) }
  | TRACE LPAREN expr RPAREN { TRACE($3) }  /* Added missing rule for TRACE */

simple_expr:
  | INT_LITERAL { IntLit($1) } /* Simple integer literal */
  | LBRACKET vector_elements RBRACKET { VectorLit(IntLit(List.length $2), $2) } /* Vector literal */
  | LBRACKET matrix_rows RBRACKET { 
      let rows = IntLit(List.length $2) in
      let cols = if rows <> IntLit(0) then IntLit(List.length (List.hd $2)) else IntLit(0) in
      MatrixLit(rows, cols, $2) 
    } /* Matrix literal */
  | ID { Var($1) }
  | array_access { $1 }
  | BOOL_LITERAL { BoolLit($1) }
  | FLOAT_LITERAL { FloatLit($1) }
  | INPUT { Input($1) }
  | PRINT LPAREN expr RPAREN { Print($3) }
  | LPAREN expr RPAREN { $2 }

/* Define what a vector can contain */
vector_elements:
  | /* empty */ { [] }
  | non_empty_vector_elements { $1 }

non_empty_vector_elements:
  | INT_LITERAL { [IntLit($1)] }
  | FLOAT_LITERAL { [FloatLit($1)] }
  | INT_LITERAL COMMA non_empty_vector_elements { IntLit($1) :: $3 }
  | FLOAT_LITERAL COMMA non_empty_vector_elements { FloatLit($1) :: $3 }

/* Define matrix rows */
matrix_rows:
  | LBRACKET vector_elements RBRACKET %prec  VECTOR_BRACKET{ [$2] }
  | LBRACKET vector_elements RBRACKET COMMA matrix_rows %prec MATRIX_COMMA{ $2 :: $5 }

array_access:
  /* Restrict array access to IDs only. */
  | ID LBRACKET expr RBRACKET { Index(Var($1), $3, None) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET { Index(Var($1), $3, Some($6)) }
%%