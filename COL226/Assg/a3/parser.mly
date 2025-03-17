%{
  open Ast
  
  (* Helper function for vector dimension checking *)
  let check_vector_dim dim elements =
    match dim with
    | Some d when d <> List.length elements ->
        raise (Failure ("Vector dimension mismatch: expected " ^ string_of_int d))
    | _ -> ()
  
  (* Extract elements from vector literals to create a matrix row *)
  let extract_vector_elements = function
    | VectorLit(_, elements) -> elements
    | _ -> failwith "Expected vector literal in matrix row"
%}

/* Token declarations */
%token <bool> TRUE FALSE
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL ID INPUT

/* Keywords */
%token BOOL INT FLOAT VECTOR MATRIX
%token PRINT
%token IF ELSE FOR WHILE DO
%token AND OR NOT XOR
%token DOT MAG DIM ANGLE TRANS DET ABS

/* Operators and punctuation */
%token PLUS MINUS MUL DIV MOD
%token EQ NEQ LT GT LEQ GEQ
%token ASSIGN
%token SEMICOLON COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EOF

/* Operator precedence (lowest to highest) */
%nonassoc SEMICOLON
%nonassoc ELSE
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
%nonassoc MAG DIM ANGLE TRANS DET ABS

/* Entry point */
%start program
%type <Ast.program> program

%%
/* Grammar rules */

program:
  stmt_list EOF { Program($1) }
  ;

stmt_list:
  /* empty */ { [] }
| stmt stmt_list { $1 :: $2 }
  ;

stmt:
  expr SEMICOLON { ExprStmt($1) }
| type_spec ID SEMICOLON { DeclStmt($2, $1, None) }
| type_spec ID ASSIGN expr SEMICOLON { DeclStmt($2, $1, Some $4) }
| ID ASSIGN expr SEMICOLON { AssignStmt($1, $3) }
| IF LPAREN expr RPAREN block ELSE block { IfStmt($3, $5, Some $7) }
| IF LPAREN expr RPAREN block { IfStmt($3, $5, None) }
| FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN block { ForStmt($3, $5, $7, $9) }
| WHILE LPAREN expr RPAREN block { WhileStmt($3, $5) }
| DO block WHILE LPAREN expr RPAREN SEMICOLON { DoWhileStmt($2, $5) }
  ;

block:
  LBRACE stmt_list RBRACE { Block($2) }
  ;

type_spec:
  BOOL { BoolType }
| INT { IntType }
| FLOAT { FloatType }
| VECTOR { VectorType(None) }
| VECTOR INT_LITERAL { VectorType(Some $2) }
| MATRIX { MatrixType(None, None) }
| MATRIX INT_LITERAL COMMA INT_LITERAL { MatrixType(Some $2, Some $4) }
  ;

expr:
  TRUE { BoolLit(true) }
| FALSE { BoolLit(false) }
| INT_LITERAL { IntLit($1) }
| FLOAT_LITERAL { FloatLit($1) }
| STRING_LITERAL { StringLit($1) }
| ID { Var($1) }
| INPUT LPAREN RPAREN { Input(None) }
| INPUT LPAREN expr RPAREN { Input(Some $3) }
| PRINT LPAREN expr RPAREN { Print($3) }
| vector_lit { $1 }
| matrix_lit { $1 }
| LPAREN expr RPAREN { $2 }
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
| expr LBRACKET expr RBRACKET { Index($1, $3, None) }
| expr LBRACKET expr RBRACKET LBRACKET expr RBRACKET { Index($1, $3, Some $6) }
  ;

vector_lit:
  INT_LITERAL LBRACKET expr_list RBRACKET {
    let dim = $1 in
    let elements = $3 in
    check_vector_dim (Some dim) elements;
    VectorLit(Some dim, elements)
  }
| LBRACKET expr_list RBRACKET { 
    let elements = $2 in
    VectorLit(Some (List.length elements), elements) 
  }
  ;

expr_list:
  /* empty */ { [] }
| non_empty_expr_list { $1 }
  ;

non_empty_expr_list:
  expr { [$1] }
| expr COMMA non_empty_expr_list { $1 :: $3 }
  ;

matrix_lit:
  INT_LITERAL COMMA INT_LITERAL LBRACKET vector_rows RBRACKET {
    let rows = $1 in
    let cols = $3 in
    let vector_exprs = $5 in
    let row_count = List.length vector_exprs in
    
    (* Basic dimension check *)
    if rows <> row_count then
      raise (Failure ("Matrix row count mismatch: got " ^ string_of_int row_count ^ 
                      ", expected " ^ string_of_int rows));
    
    (* Extract elements from each vector to get a list of lists *)
    let elements = List.map extract_vector_elements vector_exprs in
    MatrixLit(Some rows, Some cols, elements)
  }
| LBRACKET vector_rows RBRACKET {
    let vector_exprs = $2 in
    let rows = List.length vector_exprs in
    
    (* Extract elements and determine column count *)
    let elements = List.map extract_vector_elements vector_exprs in
    let cols = match elements with
              | [] -> 0
              | first_row::_ -> List.length first_row in
    
    MatrixLit(Some rows, Some cols, elements)
  }
  ;

vector_rows:
  vector_lit { [$1] }
| vector_lit COMMA vector_rows { $1 :: $3 }
  ;

%%