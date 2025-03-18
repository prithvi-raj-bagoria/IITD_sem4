(* Abstract Syntax Tree definitions *)

(* Type definitions *)
type typ = 
  | BoolType
  | IntType
  | FloatType
  | VectorType 
  | MatrixType 

(* Expression definitions *)
type expr =
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | Var of string
  | PLUS of expr * expr
  | MINUS of expr * expr
  | TIMES of expr * expr
  | DIV of expr * expr
  | MOD of expr * expr
  | NEG of expr
  | AND of expr * expr
  | OR of expr * expr
  | XOR of expr * expr
  | NOT of expr
  | EQ of expr * expr
  | NEQ of expr * expr
  | LT of expr * expr
  | GT of expr * expr
  | LEQ of expr * expr
  | GEQ of expr * expr
  | DOT of expr * expr
  | MAG of expr
  | ABS of expr
  | DIM of expr
  | ANGLE of expr * expr
  | TRANS of expr
  | DET of expr
  | VectorLit of expr * expr list
  | MatrixLit of expr * expr  * expr list list
  | Index of expr * expr * expr option  (* Modified: third argument is now an option *)
  | Input of string option
  | Print of expr
  | Assign of string * expr

(* Statement definitions *)
type stmt =
  | ExprStmt of expr
  | DeclStmt of string * typ * expr option
  | AssignStmt of string * expr
  | IfStmt of expr * block * block option
  | ForStmt of stmt * expr * stmt * block
  | WhileStmt of expr * block
  | DoWhileStmt of block * expr
  | Block of stmt list

and block = stmt list 

(* Program definition *)
type program = Program of stmt list
