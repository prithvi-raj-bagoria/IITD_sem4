(* typechecker.ml - Type checking for Matrix/Vector DSL *)
open Ast

(* Type environment: mapping variable names to their declared types *)
type env = (string * dtype) list

(* Exception raised for type errors *)
exception Type_error of string

(* Look up a variable's type in the environment *)
let rec lookup env var =
  match env with
  | [] -> raise (Type_error ("Undefined variable: " ^ var))
  | (x, t) :: rest -> if x = var then t else lookup rest var

(* Convert a dtype to a string for error messages *)
let rec string_of_type = function
  | BoolType -> "boolean"
  | IntType -> "integer"
  | FloatType -> "float"
  | VectorType None -> "vector"
  | VectorType (Some d) -> "vector[" ^ string_of_int d ^ "]"
  | MatrixType (None, None) -> "matrix"
  | MatrixType (Some r, None) -> "matrix[" ^ string_of_int r ^ ",?]"
  | MatrixType (None, Some c) -> "matrix[?, " ^ string_of_int c ^ "]"
  | MatrixType (Some r, Some c) -> "matrix[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]"

(* Check if vector dimensions are compatible *)
let check_vector_dims d1 d2 op =
  match d1, d2 with
  | None, _ | _, None -> VectorType None  (* If at least one dimension is unknown *)
  | Some n1, Some n2 ->
      if n1 = n2 then
        match op with
        | "add" | "sub" -> VectorType (Some n1)
        | "dot" | "angle" -> FloatType
        | _ -> raise (Type_error ("Unknown vector operation: " ^ op))
      else
        raise (Type_error ("Incompatible vector dimensions: " ^
                           string_of_int n1 ^ " and " ^ string_of_int n2))

(* Check if matrix dimensions are compatible *)
let check_matrix_dims r1 c1 r2 c2 op =
  match r1, c1, r2, c2 with
  | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None ->
      (match op with
       | "add" | "sub" -> MatrixType (r1, c1)
       | "mul" -> MatrixType (r1, c2)
       | "trans" -> MatrixType (c1, r1)
       | "det" -> FloatType
       | _ -> raise (Type_error ("Unknown matrix operation: " ^ op)))
  | Some m1, Some n1, Some m2, Some n2 ->
      match op with
      | "add" | "sub" when m1 = m2 && n1 = n2 -> MatrixType (Some m1, Some n1)
      | "mul" when n1 = m2 -> MatrixType (Some m1, Some n2)
      | "trans" -> MatrixType (Some n1, Some m1)
      | "det" when m1 = n1 -> FloatType
      | "det" -> raise (Type_error "Determinant requires a square matrix")
      | _ -> raise (Type_error ("Incompatible matrix dimensions for " ^ op))

(* Main type inference function for expressions *)
let rec type_of_expr env = function
  | BoolLit _ -> BoolType
  | IntLit _ -> IntType
  | FloatLit _ -> FloatType
  | StringLit _ ->
      raise (Type_error "String literals are only valid in I/O operations")
  
  | VectorLit (dim, elements) ->
      let elem_types = List.map (type_of_expr env) elements in
      (match elem_types with
       | [] -> VectorType dim
       | t :: ts ->
           if List.for_all (fun x -> x = t) ts then
             (match t with
              | IntType | FloatType -> VectorType dim
              | _ -> raise (Type_error "Vector elements must be numeric"))
           else
             raise (Type_error "Vector elements must have the same type"))
  
  | MatrixLit (rows, cols, vectors) ->
      (* 'vectors' is a list of rows (each a list of expressions).
         Flatten it so that each element is checked by type_of_expr. *)
      let flat_elems = List.flatten vectors in
      let elem_types = List.map (type_of_expr env) flat_elems in
      (match elem_types with
       | [] -> MatrixType (rows, cols)
       | t :: ts ->
           if List.for_all (fun x -> x = t) ts then
             (match t with
              | IntType | FloatType -> MatrixType (rows, cols)
              | _ -> raise (Type_error "Matrix elements must be numeric"))
           else
             raise (Type_error "Matrix elements must have the same type"))
  
  | Var name -> lookup env name
  
  | PLUS (e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | IntType, IntType -> IntType
       | FloatType, FloatType -> FloatType
       | IntType, FloatType | FloatType, IntType -> FloatType
       | VectorType d1, VectorType d2 -> check_vector_dims d1 d2 "add"
       | MatrixType (r1, c1), MatrixType (r2, c2) -> check_matrix_dims r1 c1 r2 c2 "add"
       | _ -> raise (Type_error ("Type mismatch in addition: " ^
                                  string_of_type t1 ^ " + " ^ string_of_type t2)))
  
  | MINUS (e1, e2) ->
      let t1 = type_of_expr env e1 and t2 = type_of_expr env e2 in
      (match t1, t2 with
       | IntType, IntType -> IntType
       | FloatType, FloatType -> FloatType
       | IntType, FloatType | FloatType, IntType -> FloatType
       | VectorType d1, VectorType d2 -> check_vector_dims d1 d2 "sub"
       | MatrixType (r1, c1), MatrixType (r2, c2) -> check_matrix_dims r1 c1 r2 c2 "sub"
       | _ -> raise (Type_error ("Type mismatch in subtraction: " ^
                                  string_of_type t1 ^ " - " ^ string_of_type t2)))
  
  | TIMES (e1, e2) ->
      let t1 = type_of_expr env e1 and t2 = type_of_expr env e2 in
      (match t1, t2 with
       | IntType, IntType -> IntType
       | FloatType, FloatType -> FloatType
       | IntType, FloatType | FloatType, IntType -> FloatType
       | IntType, VectorType d | FloatType, VectorType d -> VectorType d
       | VectorType d, IntType | VectorType d, FloatType -> VectorType d
       | IntType, MatrixType (r, c) | FloatType, MatrixType (r, c) -> MatrixType (r, c)
       | MatrixType (r, c), IntType | MatrixType (r, c), FloatType -> MatrixType (r, c)
       | MatrixType (r1, c1), MatrixType (r2, c2) -> check_matrix_dims r1 c1 r2 c2 "mul"
       | _ -> raise (Type_error ("Type mismatch in multiplication: " ^
                                  string_of_type t1 ^ " * " ^ string_of_type t2)))
  
  | DIV (e1, e2) ->
      let t1 = type_of_expr env e1 and t2 = type_of_expr env e2 in
      (match t1, t2 with
       | IntType, IntType -> IntType
       | FloatType, FloatType -> FloatType
       | IntType, FloatType | FloatType, IntType -> FloatType
       | VectorType d, IntType | VectorType d, FloatType -> VectorType d
       | MatrixType (r, c), IntType | MatrixType (r, c), FloatType -> MatrixType (r, c)
       | _ -> raise (Type_error ("Type mismatch in division: " ^
                                  string_of_type t1 ^ " / " ^ string_of_type t2)))
  
  | MOD (e1, e2) ->
      let t1 = type_of_expr env e1 and t2 = type_of_expr env e2 in
      (match t1, t2 with
       | IntType, IntType -> IntType
       | _ -> raise (Type_error "Modulo requires integer operands"))
  
  | EQ (e1, e2) | NEQ (e1, e2) | LT (e1, e2)
  | GT (e1, e2) | LEQ (e1, e2) | GEQ (e1, e2) ->
      let t1 = type_of_expr env e1 and t2 = type_of_expr env e2 in
      (match t1, t2 with
       | IntType, IntType | FloatType, FloatType | BoolType, BoolType -> BoolType
       | _ -> raise (Type_error "Comparison requires compatible types"))
  
  | AND (e1, e2) | OR (e1, e2) | XOR (e1, e2) ->
      let t1 = type_of_expr env e1 and t2 = type_of_expr env e2 in
      (match t1, t2 with
       | BoolType, BoolType -> BoolType
       | _ -> raise (Type_error "Logical operations require boolean operands"))
  
  | NOT e ->
      let t = type_of_expr env e in
      (match t with
       | BoolType -> BoolType
       | _ -> raise (Type_error "Logical NOT requires a boolean operand"))
  
  | NEG e ->
      let t = type_of_expr env e in
      (match t with
       | IntType -> IntType
       | FloatType -> FloatType
       | VectorType d -> VectorType d
       | MatrixType (r, c) -> MatrixType (r, c)
       | _ -> raise (Type_error "Unary negation requires a numeric type"))
  
  | ABS e ->
      let t = type_of_expr env e in
      (match t with
       | IntType -> IntType
       | FloatType -> FloatType
       | _ -> raise (Type_error "Absolute value requires a numeric operand"))
  
  | DOT (e1, e2) ->
      let t1 = type_of_expr env e1 and t2 = type_of_expr env e2 in
      (match t1, t2 with
       | VectorType d1, VectorType d2 -> check_vector_dims d1 d2 "dot"
       | _ -> raise (Type_error "Dot product requires vector operands"))
  
  | MAG e ->
      let t = type_of_expr env e in
      (match t with
       | VectorType _ -> FloatType
       | _ -> raise (Type_error "Magnitude requires a vector operand"))
  
  | DIM e ->
      let t = type_of_expr env e in
      (match t with
       | VectorType _ | MatrixType _ -> IntType
       | _ -> raise (Type_error "Dimension requires a vector or matrix operand"))
  
  | ANGLE (e1, e2) ->
      let t1 = type_of_expr env e1 and t2 = type_of_expr env e2 in
      (match t1, t2 with
       | VectorType d1, VectorType d2 -> check_vector_dims d1 d2 "angle"
       | _ -> raise (Type_error "Angle requires vector operands"))
  
  | TRANS e ->
      let t = type_of_expr env e in
      (match t with
       | MatrixType (r, c) -> MatrixType (c, r)
       | _ -> raise (Type_error "Transpose requires a matrix operand"))
  
  | DET e ->
      let t = type_of_expr env e in
      (match t with
       | MatrixType (r, c) ->
           (match r, c with
            | Some m, Some n when m = n -> FloatType
            | Some _, Some _ -> raise (Type_error "Determinant requires a square matrix")
            | _, _ -> FloatType)
       | _ -> raise (Type_error "Determinant requires a matrix operand"))
  
  | Assign (var, e) ->
      let var_type = lookup env var in
      let expr_type = type_of_expr env e in
      if var_type = expr_type then var_type
      else raise (Type_error ("Type mismatch in assignment: expected " ^
                              string_of_type var_type ^ " but got " ^
                              string_of_type expr_type))
  
  | Input None -> IntType
  | Input (Some e) -> let _ = type_of_expr env e in IntType
  | Print e -> let _ = type_of_expr env e in IntType
  | Index (_, _, _) -> IntType  (* Simplified indexing support *)

(* Type checker for statements *)
let rec typecheck_stmt env = function
  | ExprStmt e ->
      let _ = type_of_expr env e in env
  | DeclStmt (name, typ, init_opt) ->
      let env' = (name, typ) :: env in
      (match init_opt with
       | None -> env'
       | Some e ->
           let init_type = type_of_expr env e in
           if init_type = typ then env'
           else raise (Type_error ("Initializer type does not match declaration: " ^
                                    string_of_type init_type ^ " vs " ^ string_of_type typ)))
  | AssignStmt (name, e) ->
      let var_type = lookup env name in
      let expr_type = type_of_expr env e in
      if var_type = expr_type then env
      else raise (Type_error ("Assignment type mismatch: " ^
                              string_of_type var_type ^ " vs " ^ string_of_type expr_type))
  | IfStmt (cond, then_stmt, else_opt) ->
      let cond_type = type_of_expr env cond in
      if cond_type <> BoolType then raise (Type_error "If condition must be boolean")
      else
        let _ = typecheck_stmt env then_stmt in
        (match else_opt with
         | None -> env
         | Some else_stmt -> let _ = typecheck_stmt env else_stmt in env)
  | ForStmt (init, cond, incr, body) ->
      let _ = type_of_expr env init in
      let cond_type = type_of_expr env cond in
      if cond_type <> BoolType then raise (Type_error "For loop condition must be boolean")
      else
        let _ = type_of_expr env incr in
        let _ = typecheck_stmt env body in env
  | WhileStmt (cond, body) ->
      if type_of_expr env cond <> BoolType then raise (Type_error "While loop condition must be boolean")
      else let _ = typecheck_stmt env body in env
  | DoWhileStmt (body, cond) ->
      let _ = typecheck_stmt env body in
      if type_of_expr env cond <> BoolType then raise (Type_error "Do-while loop condition must be boolean")
      else env
  | Block stmts ->
      List.fold_left typecheck_stmt env stmts

(* Type check an entire program *)
let typecheck_program (Program stmts) =
  let _ = List.fold_left typecheck_stmt [] stmts in
  ()
