open Ast

exception TypeError of string * int * int  (* message, line, column *)

(* Type gammaironment: mapping identifiers to types *)
type gamma = (string * typ) list

(* Location information for error reporting *)
type location = {
  line: int;
  column: int;
}

(* Global reference to track current location *)
let current_loc = ref { line = 1; column = 0 }  (* Start at line 1 instead of 0 *)

(* Lookup an identifier in the gammaironment *)
let rec lookup gamma x =
  match gamma with
  | [] -> raise (TypeError ("Undefined variable: " ^ x, !current_loc.line, !current_loc.column))
  | (y,t)::rest -> if x = y then t else lookup rest x

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
let rec type_expr gamma = function
  | BoolLit _ -> BoolType
  | IntLit _ -> IntType
  | FloatLit _ -> FloatType
  | Var x -> lookup gamma x
  | PLUS(e1,e2) ->
     let t1 = type_expr gamma e1 in let t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (VectorType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (FloatType, FloatType) -> FloatType
        | _ -> raise (TypeError ("Addition not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column)))
  | MINUS(e1,e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (VectorType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (FloatType, FloatType) -> FloatType
        | _ -> raise (TypeError ("Subtraction not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column)))
  | MUL(e1,e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (VectorType, IntType) | (IntType, VectorType) -> VectorType
        | (VectorType, FloatType) | (FloatType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (MatrixType, IntType) | (IntType, MatrixType) -> MatrixType
        | (MatrixType, FloatType) | (FloatType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (FloatType, FloatType) -> FloatType
        | _ -> raise (TypeError ("Multiplication not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column)))
  | DIV(e1,e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
       (match (t1, t2) with
        | (IntType, IntType) -> FloatType
        | (FloatType, IntType) | (IntType, FloatType) | (FloatType, FloatType) -> FloatType
        | (VectorType, IntType) | (IntType, VectorType) -> VectorType
        | (MatrixType, IntType) | (IntType, MatrixType) -> MatrixType
        | (VectorType, FloatType) | (FloatType, VectorType) -> VectorType
        | (MatrixType, FloatType) | (FloatType, MatrixType) -> MatrixType
        | _ -> raise (TypeError ("Division not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column)))
  | MOD(e1,e2) ->
      (let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
       match (t1, t2) with
        | (IntType, IntType) -> IntType
        | _ -> raise (TypeError ("Modulo not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column)))
  | POWER(e1,e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (IntType, IntType) -> IntType
        | (FloatType, IntType) | (IntType, FloatType) | (FloatType, FloatType) -> FloatType
        | _ -> raise (TypeError ("Power operation not supported between " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column)))
  | NEG e ->
      let t = type_expr gamma e in
        (match t with
        | IntType | FloatType | VectorType | MatrixType -> t
        | _ -> raise (TypeError ("Unary negation not supported for " ^ 
                               string_of_type t, !current_loc.line, !current_loc.column)))
  | AND(e1,e2) | OR(e1,e2) | XOR(e1,e2)-> 
      (let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        match (t1, t2) with
        | (BoolType, BoolType) -> BoolType
        | _ -> raise (TypeError ("Logical operation requires booleans, got " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column)))
  | NOT e ->
      let t = type_expr gamma e in
      if t = BoolType then BoolType
      else raise (TypeError ("NOT requires a boolean, got " ^ string_of_type t, 
                             !current_loc.line, !current_loc.column))
  | EQ(e1,e2) | NEQ(e1,e2) | LT(e1,e2) | GT(e1,e2) | LEQ(e1,e2) | GEQ(e1,e2)-> 
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (IntType, IntType) -> BoolType
        | _ -> raise (TypeError ("Equality check requires compatible types, got " ^ 
                               string_of_type t1 ^ " and " ^ string_of_type t2, 
                               !current_loc.line, !current_loc.column)))
  | DOT(e1,e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
      (match (t1,t2) with
       | (VectorType, VectorType) -> FloatType
       | _ -> raise (TypeError ("Dot product requires vectors, got " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2, 
                                !current_loc.line, !current_loc.column)))
  | ABS e ->
      let t = type_expr gamma e in
      if t = IntType || t = FloatType then t
      else raise (TypeError ("ABS requires numeric type, got " ^ string_of_type t, 
                             !current_loc.line, !current_loc.column))
  | SQRT e ->  (* Added type checking for sqrt operation *)
      let t = type_expr gamma e in
      if t = IntType || t = FloatType then FloatType  (* sqrt always returns float *)
      else raise (TypeError ("SQRT requires numeric type, got " ^ string_of_type t, 
                             !current_loc.line, !current_loc.column))
  | MAG e ->
      let t = type_expr gamma e in
      (match t with
       | VectorType -> FloatType
       | _ -> raise (TypeError ("MAG requires a vector, got " ^ string_of_type t, 
                                !current_loc.line, !current_loc.column)))
  | DIM e ->
      let t = type_expr gamma e in
      (match t with
       | VectorType | MatrixType -> IntType
       | _ -> raise (TypeError ("DIM requires a vector or matrix, got " ^ string_of_type t, 
                                !current_loc.line, !current_loc.column)))
  | ANGLE(e1,e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
      (match (t1,t2) with
       | (VectorType, VectorType) -> FloatType
       | _ -> raise (TypeError ("ANGLE requires vectors, got " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2, 
                                !current_loc.line, !current_loc.column)))
  | TRANS e ->
      let t = type_expr gamma e in
      (match t with
       | MatrixType -> MatrixType
       | _ -> raise (TypeError ("TRANS requires a matrix, got " ^ string_of_type t, 
                                !current_loc.line, !current_loc.column)))
  | DET e ->
      let t = type_expr gamma e in
      if t = MatrixType then FloatType
      else raise (TypeError ("DET requires a square matrix, got " ^ string_of_type t, 
                                !current_loc.line, !current_loc.column))
  | TRACE(e) ->
      let t = type_expr gamma e in
      if t = MatrixType then FloatType
      else raise (TypeError ("Matrix expected in trace operation, got " ^ string_of_type t, !current_loc.line, !current_loc.column))
  | VectorLit(_, elements) ->
      let elems = List.map (type_expr gamma) elements in
      if elems = [] then VectorType
      else if List.for_all ( fun t -> t = IntType) elems then VectorType
      else if List.for_all ( fun t -> t = FloatType) elems then VectorType
      else raise (TypeError ("Vector elements must be numeric (all int or all float)", 
                           !current_loc.line, !current_loc.column))
  | MatrixLit(_, _, elements) ->
      if elements = [] then MatrixType
      else 
        let c = List.length (List.hd elements) in
        if List.for_all (fun row -> List.length row = c) elements then
          let all_int = List.for_all 
            (fun row -> List.for_all 
              (fun e -> (type_expr gamma e ) = IntType) row
            ) elements
          and all_float = List.for_all 
            (fun row -> List.for_all 
              (fun e -> (type_expr gamma e) = FloatType) row
            ) elements 
          in
          if all_int || all_float then MatrixType
          else raise (TypeError ("Matrix elements must be numeric (int or float)", 
                               !current_loc.line, !current_loc.column))
        else raise (TypeError ("Matrix literal: column count mismatch", 
                             !current_loc.line, !current_loc.column))
  | Index(e, idx1, None) ->
      (match type_expr gamma e with
       | VectorType ->
           if type_expr gamma idx1 = IntType then 
             (* Return element type based on vector content *)
             (match e with
              | VectorLit(_, elems) when elems <> [] ->
                  (* For vector literals, check the element type *)
                  let elem_type = type_expr gamma (List.hd elems) in
                  elem_type
              | _ ->
                  (* Default to FloatType for vectors from other sources *)
                  FloatType)
           else raise (TypeError ("Vector index must be an integer", 
                                  !current_loc.line, !current_loc.column))
       | _ -> raise (TypeError ("Single index access requires a vector", 
                                !current_loc.line, !current_loc.column)))
                                
  | Index(e, idx1, Some idx2) ->
      (match type_expr gamma e with
       | MatrixType ->
           if type_expr gamma idx1 = IntType && type_expr gamma idx2 = IntType then
             (* Return element type based on matrix content *)
             (match e with
              | MatrixLit(_, _, rows) when rows <> [] && List.hd rows <> [] ->
                  (* For matrix literals, check the element type *)
                  let elem_type = type_expr gamma (List.hd (List.hd rows)) in
                  elem_type
              | _ ->
                  (* Default to FloatType for matrices from other sources *)
                  FloatType)
           else raise (TypeError ("Matrix indices must be integers", 
                                  !current_loc.line, !current_loc.column))
       | _ -> raise (TypeError ("Double index access requires a matrix", 
                                !current_loc.line, !current_loc.column)))
  | Input s_opt -> FloatType  (* Assume input returns float *)
  | Print e -> let _ = type_expr gamma e in IntType

(* Typecheck statements *)
let rec type_stmt gamma stmt =
  match stmt with
  | ExprStmt e -> let _ = type_expr gamma e in gamma
  | DeclStmt(id, typ, None) -> 
      if List.exists (fun (x,_) -> x = id) gamma then
        raise (TypeError ("Variable " ^ id ^ " already declared", 
                          !current_loc.line, !current_loc.column))
      else
        let t = match typ with
          | IntType | FloatType | BoolType -> typ
          | VectorType -> VectorType
          | MatrixType -> MatrixType
        in (id, t) :: gamma  (* Add variable to gammaironment *)
  | DeclStmt(id, typ, Some e) ->
      if List.exists (fun (x,_) -> x = id) gamma then
        raise (TypeError ("Variable " ^ id ^ " already declared", 
                          !current_loc.line, !current_loc.column))
    else
      let t_e = type_expr gamma e in
      ( match (t_e,typ) with 
        | (IntType, IntType) | (FloatType, FloatType) | (BoolType, BoolType) ->  (id,typ) :: gamma
        | ( VectorType, VectorType) | (MatrixType, MatrixType) -> (id,typ) :: gamma
        | _ -> raise (TypeError ("Declaration type mismatch: cannot assign " ^ 
                               string_of_type t_e ^ " to " ^ string_of_type typ, 
                               !current_loc.line, !current_loc.column)))
  | AssignStmt(id, e) ->
      let t_var = lookup gamma id in
      let t_e = type_expr gamma e in
      ( match t_var,t_e with
        | IntType,IntType | FloatType,FloatType | BoolType,BoolType -> gamma
        | VectorType,VectorType | MatrixType,MatrixType -> gamma
        | _ -> raise (TypeError ("Assignment requires a variable of numeric type", 
                                 !current_loc.line, !current_loc.column)))
  | ArrayAssignStmt(id, idx1, idx2_opt, e) ->
    let t_var = lookup gamma id in
    let t_idx1 = type_expr gamma idx1 in
    let t_e = type_expr gamma e in

    (match t_var, t_idx1, idx2_opt with
    | VectorType, IntType, None ->
        (* Vector assignment with single integer index *)
        if t_e = FloatType || t_e = IntType then gamma  (* Keep checking for both types *)
        else raise (TypeError ("Vector element assignment type mismatch: cannot assign " ^ 
                              string_of_type t_e ^ " to vector element", 
                              !current_loc.line, !current_loc.column))
                              
    | MatrixType, IntType, Some idx2 ->
        (* Matrix assignment with two indices *)
        let t_idx2 = type_expr gamma idx2 in
        if t_idx2 <> IntType then
          raise (TypeError ("Matrix column index must be an integer", 
                           !current_loc.line, !current_loc.column))
        else if t_e = FloatType || t_e = IntType then gamma
        else raise (TypeError ("Matrix element assignment type mismatch: cannot assign " ^ 
                              string_of_type t_e ^ " to matrix element", 
                              !current_loc.line, !current_loc.column))
                              
    | VectorType, _, _ ->
        raise (TypeError ("Vector index must be an integer", 
                          !current_loc.line, !current_loc.column))
                          
    | MatrixType, IntType, None ->
        raise (TypeError ("Matrix assignment requires two indices", 
                          !current_loc.line, !current_loc.column))
                          
    | MatrixType, _, _ ->
        raise (TypeError ("Matrix indices must be integers", 
                          !current_loc.line, !current_loc.column))
                          
    | _, _, _ ->
        raise (TypeError ("Array assignment requires a vector or matrix, got " ^ 
                          string_of_type t_var, 
                          !current_loc.line, !current_loc.column)))
  | IfStmt(cond, then_block, else_opt) ->
      if type_expr gamma cond <> BoolType then
        raise (TypeError ("If condition must be boolean", 
                          !current_loc.line, !current_loc.column))
      else
        let _ = type_block gamma then_block in
        (match else_opt with
         | None -> gamma
         | Some else_blk -> let _ = type_block gamma else_blk in gamma)
  | WhileStmt(cond, block) ->
      if type_expr gamma cond <> BoolType then 
        raise (TypeError ("While condition must be boolean", 
                          !current_loc.line, !current_loc.column))
      else let _ = type_block gamma block in gamma
  | ForStmt(init, cond, update, block) ->
      let gamma' = type_stmt gamma init in
      if type_expr gamma' cond <> BoolType then 
        raise (TypeError ("For loop condition must be boolean", 
                          !current_loc.line, !current_loc.column))
      else
        let _ = type_stmt gamma' update in
        let _ = type_block gamma' block in gamma
  | Block stmts -> type_block gamma stmts

and type_block gamma stmts =
  List.fold_left type_stmt gamma stmts

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
