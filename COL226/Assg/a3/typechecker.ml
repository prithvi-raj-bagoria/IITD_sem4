open Ast

(* Simplified exception that doesn't track line/column *)
exception TypeError of string

(* Create a module for string maps *)
module StringMap = Map.Make(String)

(* Type environment: mapping identifiers to types *)
type gamma = typ StringMap.t 

(* Helper function for concise ast representation in error messages *)
let rec short_string_of_expr = function
  | BoolLit b -> string_of_bool b
  | IntLit i -> string_of_int i
  | FloatLit f -> string_of_float f
  | Var x -> x
  | PLUS(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " + " ^ short_string_of_expr e2 ^ ")"
  | MINUS(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " - " ^ short_string_of_expr e2 ^ ")"
  | MUL(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " * " ^ short_string_of_expr e2 ^ ")"
  | DIV(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " / " ^ short_string_of_expr e2 ^ ")"
  | MOD(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " % " ^ short_string_of_expr e2 ^ ")"
  | POWER(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " ** " ^ short_string_of_expr e2 ^ ")"
  | NEG e -> "(-" ^ short_string_of_expr e ^ ")"
  | AND(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " && " ^ short_string_of_expr e2 ^ ")"
  | OR(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " || " ^ short_string_of_expr e2 ^ ")"
  | XOR(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " ^ " ^ short_string_of_expr e2 ^ ")"
  | NOT e -> "!" ^ short_string_of_expr e
  | EQ(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " == " ^ short_string_of_expr e2 ^ ")"
  | NEQ(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " != " ^ short_string_of_expr e2 ^ ")"
  | LT(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " < " ^ short_string_of_expr e2 ^ ")"
  | GT(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " > " ^ short_string_of_expr e2 ^ ")"
  | LEQ(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " <= " ^ short_string_of_expr e2 ^ ")"
  | GEQ(e1, e2) -> "(" ^ short_string_of_expr e1 ^ " >= " ^ short_string_of_expr e2 ^ ")"
  | DOT(e1, e2) -> "dot(" ^ short_string_of_expr e1 ^ ", " ^ short_string_of_expr e2 ^ ")"
  | MAG e -> "mag(" ^ short_string_of_expr e ^ ")"
  | ABS e -> "abs(" ^ short_string_of_expr e ^ ")"
  | SQRT e -> "sqrt(" ^ short_string_of_expr e ^ ")"
  | DIM e -> "dim(" ^ short_string_of_expr e ^ ")"
  | ANGLE(e1, e2) -> "angle(" ^ short_string_of_expr e1 ^ ", " ^ short_string_of_expr e2 ^ ")"
  | TRANS e -> "trans(" ^ short_string_of_expr e ^ ")"
  | DET e -> "det(" ^ short_string_of_expr e ^ ")"
  | TRACE e -> "trace(" ^ short_string_of_expr e ^ ")"
  | VectorLit(_, _) -> "[...vector...]"
  | MatrixLit(_, _, _) -> "[...matrix...]"
  | Index(e, i1, None) -> short_string_of_expr e ^ "[" ^ short_string_of_expr i1 ^ "]"
  | Index(e, i1, Some i2) -> short_string_of_expr e ^ "[" ^ short_string_of_expr i1 ^ "][" ^ short_string_of_expr i2 ^ "]"
  | Input s -> "input(\"" ^ s ^ "\")"
  | Print e -> "print(" ^ short_string_of_expr e ^ ")"

(* Improved error reporting with expression context *)
let type_error msg expr = 
  let error_msg = "Type error: " ^ msg ^ "\nIn expression: " ^ short_string_of_expr expr in
  raise (TypeError error_msg)

(* Convert type to string for error messages *)
let string_of_type = function
  | IntType -> "int" | FloatType -> "float" | BoolType -> "bool"| VectorType -> "vector" | MatrixType -> "matrix"

(* Lookup an identifier in the environment *)
let lookup gamma x =
  try StringMap.find x gamma
  with Not_found -> raise (TypeError ("Undefined variable: " ^ x))

(* Helper function to check if a variable is already declared *)
let is_declared gamma id = StringMap.mem id gamma

(* Typecheck expressions *)
let rec type_expr gamma expr = match expr with
  | BoolLit _ -> BoolType
  | IntLit _ -> IntType
  | FloatLit _ -> FloatType
  | Var x -> lookup gamma x
  | PLUS(e1, e2) ->
     let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (VectorType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (FloatType, FloatType) | (IntType,FloatType) | (FloatType,IntType)-> FloatType
        | _ -> type_error ("Addition not supported between " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | MINUS(e1, e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (VectorType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (FloatType, FloatType) | (IntType,FloatType) | (FloatType,IntType) -> FloatType
        | _ -> type_error ("Subtraction not supported between " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | MUL(e1, e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (VectorType, IntType) | (IntType, VectorType) -> VectorType
        | (VectorType, FloatType) | (FloatType, VectorType) -> VectorType
        | (MatrixType, MatrixType) -> MatrixType
        | (MatrixType, IntType) | (IntType, MatrixType) -> MatrixType
        | (MatrixType, FloatType) | (FloatType, MatrixType) -> MatrixType
        | (IntType, IntType) -> IntType
        | (FloatType, FloatType) | (IntType,FloatType) | (FloatType,IntType)-> FloatType
        | _ -> type_error ("Multiplication not supported between " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | DIV(e1, e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
       (match (t1, t2) with
        | (IntType, IntType) -> FloatType
        | (FloatType, IntType) | (IntType, FloatType) | (FloatType, FloatType) -> FloatType
        | (VectorType, IntType) | (IntType, VectorType) -> VectorType
        | (MatrixType, IntType) | (IntType, MatrixType) -> MatrixType
        | (VectorType, FloatType) | (FloatType, VectorType) -> VectorType
        | (MatrixType, FloatType) | (FloatType, MatrixType) -> MatrixType
        | _ -> type_error ("Division not supported between " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | MOD(e1, e2) ->
      (let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
       match (t1, t2) with
        | (IntType, IntType) -> IntType
        | _ -> type_error ("Modulo not supported between " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | POWER(e1, e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (IntType, IntType) -> IntType
        | (FloatType, IntType) | (IntType, FloatType) | (FloatType, FloatType) -> FloatType
        | _ -> type_error ("Power operation not supported between " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | NEG e ->
      let t = type_expr gamma e in
        (match t with
        | BoolType -> BoolType
        | _ -> type_error ("Boolean negation not supported for " ^ 
                          string_of_type t) expr)
  | AND(e1, e2) | OR(e1, e2) | XOR(e1, e2)-> 
      (let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        match (t1, t2) with
        | (BoolType, BoolType) -> BoolType
        | _ -> type_error ("Logical operation requires booleans, got " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | NOT e ->
      let t = type_expr gamma e in
      if t = BoolType then BoolType
      else type_error ("NOT requires a boolean, got " ^ string_of_type t) expr
  | EQ(e1, e2) | NEQ(e1, e2) | LT(e1, e2) | GT(e1, e2) | LEQ(e1, e2) | GEQ(e1, e2)-> 
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
        (match (t1, t2) with
        | (IntType, IntType) -> BoolType
        | (FloatType, FloatType) | (IntType,FloatType) | (FloatType,IntType)-> BoolType
        | _ -> type_error ("Equality check requires compatible types, got " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | DOT(e1, e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
      (match (t1,t2) with
       | (VectorType, VectorType) -> FloatType
       | _ -> type_error ("Dot product requires vectors, got " ^ 
                         string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | ABS e ->
      let t = type_expr gamma e in
      if t = IntType || t = FloatType then t
      else type_error ("ABS requires numeric type, got " ^ string_of_type t) expr
  | SQRT e ->  (* Added type checking for sqrt operation *)
      let t = type_expr gamma e in
      if t = IntType || t = FloatType then FloatType  (* sqrt always returns float *)
      else type_error ("SQRT requires numeric type, got " ^ string_of_type t) expr
  | MAG e ->
      let t = type_expr gamma e in
      (match t with
       | VectorType -> FloatType
       | _ -> type_error ("MAG requires a vector, got " ^ string_of_type t) expr)
  | DIM e ->
      let t = type_expr gamma e in
      (match t with
       | VectorType | MatrixType -> IntType
       | _ -> type_error ("DIM requires a vector or matrix, got " ^ string_of_type t) expr)
  | ANGLE(e1,e2) ->
      let t1 = type_expr gamma e1 and t2 = type_expr gamma e2 in
      (match (t1,t2) with
       | (VectorType, VectorType) -> FloatType
       | _ -> type_error ("ANGLE requires vectors, got " ^ 
                         string_of_type t1 ^ " and " ^ string_of_type t2) expr)
  | TRANS e ->
      let t = type_expr gamma e in
      (match t with
       | MatrixType -> MatrixType
       | _ -> type_error ("TRANS requires a matrix, got " ^ string_of_type t) expr)
  | DET e ->
      let t = type_expr gamma e in
      if t = MatrixType then FloatType
      else type_error ("DET requires a square matrix, got " ^ string_of_type t) expr
  | TRACE(e) ->
      let t = type_expr gamma e in
      if t = MatrixType then FloatType
      else type_error ("Matrix expected in trace operation, got " ^ string_of_type t) expr
  | VectorLit(_, elements) ->
      let elems = List.map (type_expr gamma) elements in
      if elems = [] then VectorType
      else if List.for_all ( fun t -> t = IntType || t = FloatType) elems then VectorType
      else type_error ("Vector elements must be numeric (int or float)") expr
  | MatrixLit(_, _, elements) ->
      if elements = [] then MatrixType
      else 
        let c = List.length (List.hd elements) in
        if List.for_all (fun row -> List.length row = c) elements then
          let all_numeric = List.for_all 
            (fun row -> List.for_all 
              (fun e -> let t = (type_expr gamma e ) in t = IntType || t = FloatType) row
            ) elements
          in
          if all_numeric then MatrixType
          else type_error ("Matrix elements must be numeric (int or float)") expr
        else type_error ("Matrix literal: column count mismatch") expr
  | Index(e, idx1, None) ->
      (match type_expr gamma e with
       | VectorType ->
           if type_expr gamma idx1 = IntType then FloatType 
           else type_error ("Vector index must be an integer") expr
       | _ -> type_error ("Single index access requires a vector") expr)
  | Index(e, idx1, Some idx2) ->
      (match type_expr gamma e with
       | MatrixType ->
           if type_expr gamma idx1 = IntType && type_expr gamma idx2 = IntType then FloatType
           else type_error ("Matrix indices must be integers") expr
       | _ -> type_error ("Double index access requires a matrix") expr)
  | Input s_opt -> FloatType  (* Assume input returns float *)
  | Print e ->  type_expr gamma e

(* Typecheck statements *)
let rec type_stmt gamma stmt = match stmt with
  | ExprStmt e -> let _ = type_expr gamma e in gamma
  | DeclStmt(id, typ, None) -> 
      if is_declared gamma id then
        raise (TypeError ("Variable '" ^ id ^ "' already declared"))
      else
        StringMap.add id typ gamma
  | DeclStmt(id, typ, Some e) ->
      if is_declared gamma id then
        raise (TypeError ("Variable '" ^ id ^ "' already declared"))
      else
        let t_e = type_expr gamma e in
        if t_e = typ then
          StringMap.add id typ gamma
        else
          type_error ("Type mismatch in variable declaration: expected " ^ 
                      string_of_type typ ^ ", got " ^ string_of_type t_e) e
  | AssignStmt(id, e) ->
      let t_var = lookup gamma id in
      let t_e = type_expr gamma e in
      if t_var = t_e then gamma
      else type_error ("Assignment type mismatch: expected " ^ 
                      string_of_type t_var ^ ", got " ^ string_of_type t_e) e
  | ArrayAssignStmt(id, idx1, idx2_opt, e) ->
    let t_var = lookup gamma id in
    let t_idx1 = type_expr gamma idx1 in
    let t_e = type_expr gamma e in

    (match t_var, t_idx1, idx2_opt with
    | VectorType, IntType, None ->
        (* Vector assignment with single integer index *)
        if t_e = FloatType || t_e = IntType then gamma  (* Keep checking for both types *)
        else type_error ("Vector element assignment type mismatch: cannot assign " ^ 
                        string_of_type t_e ^ " to vector element") e
    | MatrixType, IntType, Some idx2 ->
        (* Matrix assignment with two indices *)
        let t_idx2 = type_expr gamma idx2 in
        if t_idx2 <> IntType then
          type_error ("Matrix column index must be an integer") e
        else if t_e = FloatType || t_e = IntType then gamma
        else type_error ("Matrix element assignment type mismatch: cannot assign " ^ 
                        string_of_type t_e ^ " to matrix element") e
    | VectorType, _, _ ->
        type_error ("Vector index must be an integer") e
    | MatrixType, IntType, None ->
        type_error ("Matrix assignment requires two indices") e
    | MatrixType, _, _ ->
        type_error ("Matrix indices must be integers") e
    | _, _, _ ->
        type_error ("Array assignment requires a vector or matrix, got " ^ 
                    string_of_type t_var) e)
  | IfStmt(cond, then_block, else_opt) ->
      if type_expr gamma cond <> BoolType then
        type_error ("If condition must be boolean") cond
      else
        let _ = type_block gamma then_block in
        (match else_opt with
         | None -> gamma
         | Some else_blk -> let _ = type_block gamma else_blk in gamma)
  | WhileStmt(cond, block) ->
      if type_expr gamma cond <> BoolType then 
        type_error ("While condition must be boolean") cond
      else let _ = type_block gamma block in gamma
  | ForStmt(init, cond, update, block) ->
      let gamma' = type_stmt gamma init in
      if type_expr gamma' cond <> BoolType then 
        type_error ("For loop condition must be boolean") cond
      else
        let _ = type_stmt gamma' update in
        let _ = type_block gamma' block in gamma
  | Block stmts -> type_block gamma stmts

and type_block gamma stmts =
  List.fold_left type_stmt gamma stmts

(* Top-level typechecking function *)
let typecheck prog =
  try
    let _ = type_block StringMap.empty (match prog with Program stmts -> stmts) in
    print_endline "\027[32mType checking succeeded!\027[0m"
  with
  | TypeError msg -> 
      print_endline ("\027[31mType Error: " ^ msg ^ "\027[0m");
      exit 1
