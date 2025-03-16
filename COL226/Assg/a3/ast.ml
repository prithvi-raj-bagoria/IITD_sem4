(* ast.ml - Abstract Syntax Tree definitions for Matrix/Vector DSL *)

(* Type definitions *)
type dtype =
  | BoolType
  | IntType 
  | FloatType
  | VectorType of int option         (* Optional vector length *)
  | MatrixType of int option * int option  (* Optional rows, cols *)

(* Expressions *)
type expr =
  (* Literals *)
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | VectorLit of int option * expr list
  | MatrixLit of int option * int option * expr list list
  | Var of string
  
  (* Arithmetic operations *)
  | PLUS of expr * expr
  | MINUS of expr * expr
  | TIMES of expr * expr
  | DIV of expr * expr
  | MOD of expr * expr
  | NEG of expr
  
  (* Comparison operations *)
  | EQ of expr * expr
  | NEQ of expr * expr
  | LT of expr * expr
  | GT of expr * expr
  | LEQ of expr * expr
  | GEQ of expr * expr
  
  (* Boolean operations *)
  | AND of expr * expr
  | OR of expr * expr
  | XOR of expr * expr
  | NOT of expr
  
  (* Vector operations *)
  | DOT of expr * expr    (* Dot product *)
  | MAG of expr           (* Magnitude *)
  | DIM of expr           (* Dimension *)
  | ANGLE of expr * expr  (* Angle between vectors *)
  
  (* Matrix operations *)
  | TRANS of expr         (* Transpose *)
  | DET of expr           (* Determinant *)
  
  (* Other operations *)
  | ABS of expr           (* Absolute value *)
  | Assign of string * expr  (* Assignment *)
  | Input of expr option  (* Input with optional filename *)
  | Print of expr         (* Print statement *)
  | Index of expr * expr * expr option  (* Indexing (e.g., v[i] or m[i][j]) *)

(* Statements *)
type stmt =
  | ExprStmt of expr
  | DeclStmt of string * dtype * expr option  (* Declaration with optional initializer *)
  | AssignStmt of string * expr
  | IfStmt of expr * stmt * stmt option       (* if (cond) then ... else ... *)
  | ForStmt of expr * expr * expr * stmt       (* for(init; cond; incr; body) *)
  | WhileStmt of expr * stmt
  | DoWhileStmt of stmt * expr
  | Block of stmt list

(* A program is simply a list of statements *)
type program = Program of stmt list
