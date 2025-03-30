open Ast

exception TypeError of string * int * int  (* message, line, column *)

(* Type environment: mapping identifiers to types *)
type env = (string * typ) list

(* Location information for error reporting *)
type location = {
  line: int;
  column: int;
}

(* Global reference to track current location *)
let current_loc = ref { line = 1; column = 0 }  (* Start at line 1 instead of 0 *)

(* Lookup an identifier in the environment *)
let rec lookup env x =
  match env with
  | [] -> raise (TypeError ("Undefined variable: " ^ x, !current_loc.line, !current_loc.column))
  | (y,t)::rest -> if x = y then t else lookup rest x

(* Check if two types are compatible *)
let compatible t1 t2 =
  match (t1,t2) with
  | (IntType,IntType) | (FloatType,FloatType) | (BoolType,BoolType) -> true
  | (IntType,FloatType) | (FloatType,IntType) -> true
  | (VectorType, VectorType) -> true
  | (MatrixType, MatrixType) -> true
  | (VectorType, IntType) | (IntType, VectorType) -> true  (* Allow vector * int operations *)
  | (VectorType, FloatType) | (FloatType, VectorType) -> true  (* Allow vector * float operations *)
  | (MatrixType, IntType) | (IntType, MatrixType) -> true  (* Allow matrix * int operations *)
  | (MatrixType, FloatType) | (FloatType, MatrixType) -> true  (* Allow matrix * float operations *)
  | _ -> false

(* Resulting type for arithmetic operations *)
let result_type t1 t2 =
  match (t1,t2) with
  | (IntType,IntType) -> IntType
  | (IntType,FloatType) | (FloatType,IntType) | (FloatType,FloatType) -> FloatType
  | (VectorType, _) | (_, VectorType) -> VectorType
  | (MatrixType, _) | (_, MatrixType) -> MatrixType
  | _ -> t1

(* Helper function to set the current location *)
let with_loc line col f =
  let old_loc = !current_loc in
  current_loc := { line = if line = 0 then 1 else line; column = col };  (* Ensure line is never 0 *)
  try
    let result = f () in
    current_loc := old_loc;
    result
  with e ->
    current_loc := old_loc;
    raise e

(* Convert type to string for error messages - Made public *)
let string_of_type = function
  | IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | VectorType -> "vector"
  | MatrixType -> "matrix"

(* Typecheck expressions *)
let rec type_expr env = function
  | BoolLit _ -> BoolType
  | IntLit _ -> IntType
  | FloatLit _ -> FloatType
  | Var x -> lookup env x
  | PLUS(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      if compatible t1 t2 then 
        match (t1, t2) with
        | (VectorType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (IntType, FloatType) | (FloatType, IntType) | (FloatType, FloatType) -> FloatType
        | _ -> raise (TypeError ("Addition not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column))
      else raise (TypeError ("Addition type mismatch: cannot operate on " ^ 
                           string_of_type t1 ^ " and " ^ string_of_type t2, 
                           !current_loc.line, !current_loc.column))
  | MINUS(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      if compatible t1 t2 then 
        match (t1, t2) with
        | (VectorType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (IntType, FloatType) | (FloatType, IntType) | (FloatType, FloatType) -> FloatType
        | _ -> raise (TypeError ("Subtraction not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column))
      else raise (TypeError ("Subtraction type mismatch: cannot operate on " ^ 
                           string_of_type t1 ^ " and " ^ string_of_type t2, 
                           !current_loc.line, !current_loc.column))
  | MUL(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      if compatible t1 t2 then 
        match (t1, t2) with
        | (VectorType, IntType) | (IntType, VectorType) -> VectorType
        | (VectorType, FloatType) | (FloatType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (MatrixType, IntType) | (IntType, MatrixType) -> MatrixType
        | (MatrixType, FloatType) | (FloatType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (IntType, FloatType) | (FloatType, IntType) | (FloatType, FloatType) -> FloatType
        | _ -> raise (TypeError ("Multiplication not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column))
      else raise (TypeError ("Multiplication type mismatch: cannot operate on " ^ 
                           string_of_type t1 ^ " and " ^ string_of_type t2, 
                           !current_loc.line, !current_loc.column))
  | DIV(e1,e2)
  | MOD(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      if (t1 = IntType || t1 = FloatType) && (t2 = IntType || t2 = FloatType) then 
        if t1 = FloatType || t2 = FloatType then FloatType else IntType
      else raise (TypeError ("Division/Modulo operation type mismatch: cannot operate on " ^ 
                           string_of_type t1 ^ " and " ^ string_of_type t2, 
                           !current_loc.line, !current_loc.column))
  | POWER(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      if (t1 = IntType || t1 = FloatType) && (t2 = IntType || t2 = FloatType) then 
        if t1 = FloatType || t2 = FloatType then FloatType else IntType
      else raise (TypeError ("Exponentiation operation type mismatch: cannot operate on " ^ 
                           string_of_type t1 ^ " and " ^ string_of_type t2, 
                           !current_loc.line, !current_loc.column))
  | NEG e ->
      let t = type_expr env e in
      if t = IntType || t = FloatType then t
      else raise (TypeError ("Unary negation requires numeric type, got " ^ string_of_type t, 
                             !current_loc.line, !current_loc.column))
  | AND(e1,e2)
  | OR(e1,e2)
  | XOR(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      if t1 = BoolType && t2 = BoolType then BoolType
      else raise (TypeError ("Logical operation requires booleans, got " ^ 
                             string_of_type t1 ^ " and " ^ string_of_type t2, 
                             !current_loc.line, !current_loc.column))
  | NOT e ->
      let t = type_expr env e in
      if t = BoolType then BoolType
      else raise (TypeError ("NOT requires a boolean, got " ^ string_of_type t, 
                             !current_loc.line, !current_loc.column))
  | EQ(e1,e2)
  | NEQ(e1,e2)
  | LT(e1,e2)
  | GT(e1,e2)
  | LEQ(e1,e2)
  | GEQ(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      if compatible t1 t2 then BoolType
      else raise (TypeError ("Comparison requires compatible types, got " ^ 
                             string_of_type t1 ^ " and " ^ string_of_type t2, 
                             !current_loc.line, !current_loc.column))
  | DOT(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      (match (t1,t2) with
       | (VectorType, VectorType) -> FloatType
       | _ -> raise (TypeError ("Dot product requires vectors, got " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2, 
                                !current_loc.line, !current_loc.column)))
  | ABS e ->
      let t = type_expr env e in
      if t = IntType || t = FloatType then t
      else raise (TypeError ("ABS requires numeric type, got " ^ string_of_type t, 
                             !current_loc.line, !current_loc.column))
  | SQRT e ->  (* Added type checking for sqrt operation *)
      let t = type_expr env e in
      if t = IntType || t = FloatType then FloatType  (* sqrt always returns float *)
      else raise (TypeError ("SQRT requires numeric type, got " ^ string_of_type t, 
                             !current_loc.line, !current_loc.column))
  | MAG e ->
      let t = type_expr env e in
      (match t with
       | VectorType -> FloatType
       | _ -> raise (TypeError ("MAG requires a vector, got " ^ string_of_type t, 
                                !current_loc.line, !current_loc.column)))
  | DIM e ->
      let t = type_expr env e in
      (match t with
       | VectorType | MatrixType -> IntType
       | _ -> raise (TypeError ("DIM requires a vector or matrix, got " ^ string_of_type t, 
                                !current_loc.line, !current_loc.column)))
  | ANGLE(e1,e2) ->
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      (match (t1,t2) with
       | (VectorType, VectorType) -> FloatType
       | _ -> raise (TypeError ("ANGLE requires vectors, got " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2, 
                                !current_loc.line, !current_loc.column)))
  | TRANS e ->
      let t = type_expr env e in
      (match t with
       | MatrixType -> MatrixType
       | _ -> raise (TypeError ("TRANS requires a matrix, got " ^ string_of_type t, 
                                !current_loc.line, !current_loc.column)))
  | DET e ->
      let t = type_expr env e in
      if t = MatrixType then FloatType
      else raise (TypeError ("DET requires a square matrix, got " ^ string_of_type t, 
                                !current_loc.line, !current_loc.column))
  | TRACE(e) ->
      let t = type_expr env e in
      if t = MatrixType then FloatType
      else raise (TypeError ("Matrix expected in trace operation, got " ^ string_of_type t, !current_loc.line, !current_loc.column))
  | VectorLit(_, elements) ->
      let elems = List.map (type_expr env) elements in
      if elems = [] then VectorType
      else if List.for_all (fun t -> t = IntType || t = FloatType) elems then VectorType
      else raise (TypeError ("Vector elements must be numeric (int or float)", 
                           !current_loc.line, !current_loc.column))
  | MatrixLit(_, _, elements) ->
      if elements = [] then MatrixType
      else 
        let c = List.length (List.hd elements) in
        if List.for_all (fun row -> List.length row = c) elements then
          let all_numeric = List.for_all 
            (fun row -> List.for_all 
              (fun e -> 
                let t = type_expr env e in 
                t = IntType || t = FloatType
              ) row
            ) elements
          in
          if all_numeric then MatrixType
          else raise (TypeError ("Matrix elements must be numeric (int or float)", 
                               !current_loc.line, !current_loc.column))
        else raise (TypeError ("Matrix literal: column count mismatch", 
                             !current_loc.line, !current_loc.column))
  | Index(e, idx1, None) ->
      (match type_expr env e with
       | VectorType ->
           if type_expr env idx1 = IntType then FloatType
           else raise (TypeError ("Vector index must be an integer", 
                                  !current_loc.line, !current_loc.column))
       | _ -> raise (TypeError ("Single index access requires a vector", 
                                !current_loc.line, !current_loc.column)))
  | Index(e, idx1, Some idx2) ->
      (match type_expr env e with
       | MatrixType ->
           if type_expr env idx1 = IntType && type_expr env idx2 = IntType then FloatType
           else raise (TypeError ("Matrix indices must be integers", 
                                  !current_loc.line, !current_loc.column))
       | _ -> raise (TypeError ("Double index access requires a matrix", 
                                !current_loc.line, !current_loc.column)))
  | Input s_opt -> FloatType  (* Assume input returns float *)
  | Print e -> let _ = type_expr env e in IntType
  | Assign(id, e) ->
      let t_var = lookup env id in
      let t_e = type_expr env e in
      if compatible t_var t_e then t_var
      else raise (TypeError ("Assignment type mismatch: cannot assign " ^ 
                             string_of_type t_e ^ " to " ^ string_of_type t_var, 
                             !current_loc.line, !current_loc.column))

(* Typecheck statements *)
let rec type_stmt env = function
  | ExprStmt e -> let _ = type_expr env e in env
  | DeclStmt(id, typ, None) -> (id, typ)::env
  | DeclStmt(id, typ, Some e) ->
      let t_e = type_expr env e in
      if compatible typ t_e then (id, typ)::env
      else raise (TypeError ("Declaration type mismatch: cannot initialize " ^ 
                             string_of_type typ ^ " with " ^ string_of_type t_e, 
                             !current_loc.line, !current_loc.column))
  | AssignStmt(id, e) ->
      let t_var = lookup env id in
      let t_e = type_expr env e in
      if compatible t_var t_e then env
      else raise (TypeError ("Assignment type mismatch: cannot assign " ^ 
                             string_of_type t_e ^ " to " ^ string_of_type t_var, 
                             !current_loc.line, !current_loc.column))
  | IfStmt(cond, then_block, else_opt) ->
      if type_expr env cond <> BoolType then
        raise (TypeError ("If condition must be boolean", 
                          !current_loc.line, !current_loc.column))
      else
        let _ = type_block env then_block in
        (match else_opt with
         | None -> env
         | Some else_blk -> let _ = type_block env else_blk in env)
  | WhileStmt(cond, block) ->
      if type_expr env cond <> BoolType then 
        raise (TypeError ("While condition must be boolean", 
                          !current_loc.line, !current_loc.column))
      else let _ = type_block env block in env
  | ForStmt(init, cond, update, block) ->
      let env' = type_stmt env init in
      if type_expr env' cond <> BoolType then 
        raise (TypeError ("For loop condition must be boolean", 
                          !current_loc.line, !current_loc.column))
      else
        let _ = type_stmt env' update in
        let _ = type_block env' block in env
  | Block stmts -> type_block env stmts

and type_block env stmts =
  List.fold_left type_stmt env stmts

(* Top-level typechecking function *)
let typecheck prog =
  try
    let _ = type_block [] (match prog with Program stmts -> stmts) in
    print_endline "\027[32mType checking succeeded!\027[0m"
  with
  | TypeError (msg, line, col) -> 
      print_endline ("\027[31mType error at line " ^ string_of_int line ^ ", column " ^ 
                    string_of_int col ^ ": " ^ msg ^ "\027[0m");
      exit 1  (* Exit with error code to prevent further processing *)
