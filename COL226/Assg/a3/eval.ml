(* eval.ml - Interpreter for matrix/vector manipulation language *)
open Ast

(* Runtime value representation *)
type value =
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
  | VectorVal of value list
  | MatrixVal of value list list
  | UnitVal (* For statements that don't return values *)

(* Create a module for string maps - same as in typechecker.ml *)
module StringMap = Map.Make(String)

(* Environment mapping variable names to values using Map *)
type env = value StringMap.t

let empty_env : env = StringMap.empty

(* Custom runtime error with expression context *)
exception RuntimeError of string * expr option

(* Helper function for concise value representation in error messages *)
let rec short_string_of_value = function
  | IntVal n -> string_of_int n
  | FloatVal f -> string_of_float f
  | BoolVal b -> string_of_bool b
  | VectorVal vs -> "[" ^ String.concat ", " (List.map short_string_of_value vs) ^ "]"
  | MatrixVal rows -> 
      "[" ^ String.concat "; " (List.map (fun r -> 
        "[" ^ String.concat ", " (List.map short_string_of_value r) ^ "]") rows) ^ "]"
  | UnitVal -> "()"

(* Reuse the short_string_of_expr function from typechecker *)
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

(* Runtime error helper *)
let runtime_error msg expr_opt =
  let error_msg = match expr_opt with
    | None -> "Runtime error: " ^ msg
    | Some e -> "Runtime error: " ^ msg ^ "\nIn expression: " ^ short_string_of_expr e
  in
  raise (RuntimeError (error_msg, expr_opt))

(* Environment operations using Map *)
let lookup env x =
  try StringMap.find x env
  with Not_found -> runtime_error ("Undefined variable: " ^ x) None

let extend env x v = StringMap.add x v env

let update env x v = StringMap.add x v env

(* Full string conversion for values - used for printing/output *)
let rec string_of_value = function
  | IntVal n -> string_of_int n
  | FloatVal f -> string_of_float f
  | BoolVal b -> string_of_bool b
  | VectorVal vs -> 
      "[" ^ String.concat ", " (List.map string_of_value vs) ^ "]"
  | MatrixVal rows ->
      let row_strs = List.map (fun r -> 
        "[" ^ String.concat ", " (List.map string_of_value r) ^ "]"
      ) rows in
      "[" ^ String.concat "\n " row_strs ^ "]"
  | UnitVal -> "()"

(* Vector and matrix operations - with improved error reporting *)
let vector_op op v1 v2 e =
  match (v1, v2) with
  | (VectorVal vs1, VectorVal vs2) ->
      if List.length vs1 <> List.length vs2 then
        runtime_error "Vector operation requires equal dimensions" (Some e)
      else
        VectorVal (List.map2 (fun a b ->
          match a, b with
          | IntVal x, IntVal y -> op (`Int (x, y))
          | FloatVal x, FloatVal y -> op (`Float (x, y))
          | IntVal x, FloatVal y -> op (`Float ((float_of_int x), y))
          | FloatVal x, IntVal y -> op (`Float (x, (float_of_int y)))
          | _, _ -> runtime_error "Vector elements must be numbers" (Some e)
        ) vs1 vs2)
  | _ -> runtime_error "Vector operation requires vectors" (Some e)

let matrix_op op m1 m2 e =
  match (m1, m2) with
  | (MatrixVal rows1, MatrixVal rows2) ->
      if List.length rows1 <> List.length rows2 ||
         (rows1 <> [] && List.length (List.hd rows1) <> List.length (List.hd rows2)) then
        runtime_error "Matrix operation requires equal dimensions" (Some e)
      else
        MatrixVal (List.map2 (fun r1 r2 ->
          List.map2 (fun a b ->
            match a, b with
            | IntVal x, IntVal y -> op (`Int (x, y))
            | FloatVal x, FloatVal y -> op (`Float (x, y))
            | IntVal x, FloatVal y -> op (`Float ((float_of_int x), y))
            | FloatVal x, IntVal y -> op (`Float (x, (float_of_int y)))
            | _, _ -> runtime_error "Matrix elements must be numbers" (Some e)
          ) r1 r2
        ) rows1 rows2)
  | _ -> runtime_error "Matrix operation requires matrices" (Some e)

let vector_scalar_multiply v scalar = 
  match v with
  | VectorVal vs ->
      VectorVal (List.map (fun x ->
        match x, scalar with
        | IntVal n, IntVal s -> IntVal (n * s)
        | IntVal n, FloatVal s -> FloatVal ((float_of_int n) *. s)
        | FloatVal f, IntVal s -> FloatVal (f *. (float_of_int s))
        | FloatVal f, FloatVal s -> FloatVal (f *. s)
        | _, _ -> runtime_error "Vector elements and scalar must be numbers" None
      ) vs)
  | _ -> runtime_error "Vector-scalar multiplication requires a vector" None

let matrix_scalar_multiply m scalar = 
  match m with
  | MatrixVal rows ->
      MatrixVal (List.map (fun row ->
        List.map (fun x ->
          match x, scalar with
          | IntVal n, IntVal s -> IntVal (n * s)
          | IntVal n, FloatVal s -> FloatVal ((float_of_int n) *. s)
          | FloatVal f, IntVal s -> FloatVal (f *. (float_of_int s))
          | FloatVal f, FloatVal s -> FloatVal (f *. s)
          | _, _ -> runtime_error "Matrix elements and scalar must be numbers" None
        ) row
      ) rows)
  | _ -> runtime_error "Matrix-scalar multiplication requires a matrix" None

let vector_scalar_divide v scalar =
  match scalar with
  | IntVal 0 -> runtime_error "Division by zero" None
  | FloatVal 0.0 -> runtime_error "Division by zero" None
  | _ ->
    match v with
    | VectorVal vs ->
        VectorVal (List.map (fun x ->
          match x, scalar with
          | IntVal n, IntVal s -> IntVal (n / s)
          | IntVal n, FloatVal s -> FloatVal ((float_of_int n) /. s)
          | FloatVal f, IntVal s -> FloatVal (f /. (float_of_int s))
          | FloatVal f, FloatVal s -> FloatVal (f /. s)
          | _, _ -> runtime_error "Vector elements and scalar must be numbers" None
        ) vs)
    | _ -> runtime_error "Vector-scalar division requires a vector" None

let matrix_scalar_divide m scalar =
  match scalar with
  | IntVal 0 -> runtime_error "Division by zero" None
  | FloatVal 0.0 -> runtime_error "Division by zero" None
  | _ ->
    match m with
    | MatrixVal rows ->
        MatrixVal (List.map (fun row ->
          List.map (fun x ->
            match x, scalar with
            | IntVal n, IntVal s -> IntVal (n / s)
            | IntVal n, FloatVal s -> FloatVal ((float_of_int n) /. s)
            | FloatVal f, IntVal s -> FloatVal (f /. (float_of_int s))
            | FloatVal f, FloatVal s -> FloatVal (f /. s)
            | _, _ -> runtime_error "Matrix elements and scalar must be numbers" None
          ) row
        ) rows)
    | _ -> runtime_error "Matrix-scalar division requires a matrix" None

let vector_add v1 v2 =
  match v1, v2 with
  | VectorVal vs1, VectorVal vs2 ->
      if List.length vs1 <> List.length vs2 then
        runtime_error "Vector addition requires equal dimensions" None
      else
        VectorVal (List.map2 (fun a b ->
          match a, b with
          | IntVal x, IntVal y -> IntVal (x + y)
          | FloatVal x, FloatVal y -> FloatVal (x +. y)
          | IntVal x, FloatVal y -> FloatVal ((float_of_int x) +. y)
          | FloatVal x, IntVal y -> FloatVal (x +. (float_of_int y))
          | _, _ -> runtime_error "Vector elements must be numbers" None
        ) vs1 vs2)
  | _, _ -> runtime_error "Vector addition requires vectors" None

let matrix_add m1 m2 =
  match m1, m2 with
  | MatrixVal rows1, MatrixVal rows2 ->
      if List.length rows1 <> List.length rows2 then 
        runtime_error "Matrix addition requires equal dimensions" None
      else if List.length rows1 > 0 && 
              (List.length (List.hd rows1) <> List.length (List.hd rows2)) then
        runtime_error "Matrix addition requires equal dimensions" None
      else
        MatrixVal (List.map2 (fun r1 r2 ->
          List.map2 (fun a b ->
            match a, b with
            | IntVal x, IntVal y -> IntVal (x + y)
            | FloatVal x, FloatVal y -> FloatVal (x +. y)
            | IntVal x, FloatVal y -> FloatVal ((float_of_int x) +. y)
            | FloatVal x, IntVal y -> FloatVal (x +. (float_of_int y))
            | _, _ -> runtime_error "Matrix elements must be numbers" None
          ) r1 r2
        ) rows1 rows2)
  | _, _ -> runtime_error "Matrix addition requires matrices" None

let transpose matrix =
  match matrix with
  | MatrixVal rows ->
      if rows = [] then MatrixVal []
      else
        let cols = List.length (List.hd rows) in
        let transposed = List.init cols (fun j ->
          List.map (fun row ->
            try List.nth row j
            with _ -> runtime_error "Irregular matrix found during transpose" None
          ) rows
        ) in
        MatrixVal transposed
  | _ -> runtime_error "Transpose requires a matrix" None

let matrix_multiply m1 m2 =
  match m1, m2 with
  | MatrixVal rows1, MatrixVal rows2 ->
      if rows1 = [] || rows2 = [] then
        runtime_error "Cannot multiply empty matrices" None
      else
        (* Get transpose of m2 for easier column access *)
        let cols2 = match transpose (MatrixVal rows2) with
                    | MatrixVal cols -> cols
                    | _ -> runtime_error "Impossible: Transpose returned non-matrix" None in
        
        (* Check compatible dimensions *)
        if List.length (List.hd rows1) <> List.length rows2 then
          runtime_error "Matrix multiplication dimension mismatch" None
        else
          let result = List.map (fun row1 ->
            List.map (fun col2 ->
              (* Compute dot product of row1 and col2 *)
              let products = List.map2 (fun a b ->
                match a, b with
                | IntVal x, IntVal y -> IntVal (x * y)
                | FloatVal x, FloatVal y -> FloatVal (x *. y)
                | IntVal x, FloatVal y -> FloatVal ((float_of_int x) *. y)
                | FloatVal x, IntVal y -> FloatVal (x *. (float_of_int y))
                | _, _ -> runtime_error "Matrix elements must be numbers" None
              ) row1 col2 in
              
              (* Sum the products *)
              List.fold_left (fun acc elem ->
                match acc, elem with
                | IntVal x, IntVal y -> IntVal (x + y)
                | FloatVal x, FloatVal y -> FloatVal (x +. y)
                | IntVal x, FloatVal y -> FloatVal ((float_of_int x) +. y)
                | FloatVal x, IntVal y -> FloatVal (x +. (float_of_int y))
                | _, _ -> runtime_error "Matrix elements must be numbers" None
              ) (IntVal 0) products
            ) cols2
          ) rows1 in
          MatrixVal result
  | _, _ -> runtime_error "Matrix multiplication requires matrices" None

let compute_determinant matrix =
  match matrix with
  | MatrixVal rows ->
      if rows = [] then
        IntVal 0
      else if List.length rows <> List.length (List.hd rows) then
        runtime_error "Determinant requires a square matrix" None
      else
        let size = List.length rows in
        if size = 1 then
          (* 1x1 matrix - determinant is the value *)
          List.hd (List.hd rows)
        else if size = 2 then
          (* 2x2 matrix - a*d - b*c *)
          let extract i j = 
            match List.nth (List.nth rows i) j with
            | IntVal n -> float_of_int n
            | FloatVal f -> f
            | _ -> runtime_error "Matrix elements must be numbers" None in
          let a = extract 0 0 and b = extract 0 1 
          and c = extract 1 0 and d = extract 1 1 in
          FloatVal ((a *. d) -. (b *. c))
        else
          (* Recursive definition using cofactor expansion *)
          let rec minor i j mat =
            (* Create matrix with row i and column j removed *)
            let filtered_rows = 
              List.mapi (fun idx row -> 
                if idx <> i then 
                  Some (List.mapi (fun jdx elem -> 
                    if jdx <> j then Some elem else None) row
                  |> List.filter_map (fun x -> x))
                else None
              ) mat
              |> List.filter_map (fun x -> x) in
            MatrixVal filtered_rows in
          
          let rec det_helper mat size =
            if size = 1 then List.hd (List.hd mat)
            else if size = 2 then
              let a = List.nth (List.nth mat 0) 0 in
              let b = List.nth (List.nth mat 0) 1 in
              let c = List.nth (List.nth mat 1) 0 in
              let d = List.nth (List.nth mat 1) 1 in
              match a, b, c, d with
                | IntVal a, IntVal b, IntVal c, IntVal d -> 
                  IntVal (a * d - b * c)
                | _ ->
                  (* Convert all values to float and calculate determinant *)
                  let float_of_val v = match v with
                  | IntVal n -> float_of_int n
                  | FloatVal f -> f
                  | _ -> runtime_error "Non-numeric value in determinant" None 
                  in
                  let fa = float_of_val a and fb = float_of_val b 
                  and fc = float_of_val c and fd = float_of_val d in
                  FloatVal (fa *. fd -. fb *. fc)
            else
              (* Expand along first row *)
              let sum = ref (IntVal 0) in
              for j = 0 to size - 1 do
                let element = List.nth (List.nth mat 0) j in
                let minor_mat = match minor 0 j mat with
                               | MatrixVal m -> m
                               | _ -> runtime_error "Impossible" None in
                let minor_det = det_helper minor_mat (size - 1) in
                let sign = if j mod 2 = 0 then 1 else -1 in
                
                (* Multiply element by cofactor and add to sum *)
                let term = match element, minor_det with
                  | IntVal e, IntVal md -> IntVal (sign * e * md)
                  | FloatVal f, IntVal md -> FloatVal ((float_of_int sign) *. f *. (float_of_int md))
                  | IntVal e, FloatVal f -> FloatVal ((float_of_int sign) *. (float_of_int e) *. f)
                  | FloatVal e, FloatVal f -> FloatVal ((float_of_int sign) *. e *. f)
                  | _, _ -> runtime_error "Non-numeric value in determinant" None 
                in
                
                (* Add to running sum *)
                sum := match !sum, term with
                  | IntVal s, IntVal t -> IntVal (s + t)
                  | FloatVal s, FloatVal t -> FloatVal (s +. t)
                  | IntVal s, FloatVal t -> FloatVal ((float_of_int s) +. t)
                  | FloatVal s, IntVal t -> FloatVal (s +. (float_of_int t))
                  | _, _ -> runtime_error "Non-numeric sum value in determinant" None
                done;
              !sum in
          
          det_helper rows size
  | _ -> runtime_error "Determinant requires a matrix" None

(* Read matrix from file *)
let read_matrix_from_file filename =
  try
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        let values = String.split_on_char ' ' line 
                    |> List.filter (fun s -> s <> "") 
                    |> List.map (fun s -> 
                         try IntVal (int_of_string s)
                         with Failure _ -> FloatVal (float_of_string s)) in
        read_lines (values :: acc)
      with End_of_file -> 
        close_in ic;
        List.rev acc in
    let matrix = read_lines [] in
    if matrix = [] then
      runtime_error "Empty file or no valid numbers found" None
    else if List.exists (fun row -> List.length row <> List.length (List.hd matrix)) matrix then
      runtime_error "Inconsistent row lengths in matrix file" None
    else
      MatrixVal matrix
  with
  | Sys_error msg -> runtime_error msg None
  | Failure msg -> runtime_error msg None

(* Main evaluation functions *)
let rec eval_expr expr env =
  match expr with
  | IntLit n -> IntVal n
  | FloatLit f -> FloatVal f
  | BoolLit b -> BoolVal b
  | Var name -> lookup env name
  
  | PLUS (e1, e2) -> 
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | (IntVal n1, IntVal n2) -> IntVal (n1 + n2)
      | (FloatVal f1, FloatVal f2) -> FloatVal (f1 +. f2)
      | (IntVal n, FloatVal f) -> FloatVal ((float_of_int n) +. f)
      | (FloatVal f, IntVal n) -> FloatVal (f +. (float_of_int n))
      | (VectorVal _, VectorVal _) -> vector_op (function 
                                         | `Int (a, b) -> IntVal (a + b)
                                         | `Float (a, b) -> FloatVal (a +. b)) v1 v2 expr
      | (MatrixVal _, MatrixVal _) -> matrix_op (function 
                                         | `Int (a, b) -> IntVal (a + b)
                                         | `Float (a, b) -> FloatVal (a +. b)) v1 v2 expr
      | _ -> runtime_error "Addition type mismatch" (Some expr))
  
  | MINUS (e1, e2) -> 
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | (IntVal n1, IntVal n2) -> IntVal (n1 - n2)
      | (FloatVal f1, FloatVal f2) -> FloatVal (f1 -. f2)
      | (IntVal n, FloatVal f) -> FloatVal ((float_of_int n) -. f)
      | (FloatVal f, IntVal n) -> FloatVal (f -. (float_of_int n))
      | (VectorVal _, VectorVal _) -> vector_op (function 
                                         | `Int (a, b) -> IntVal (a - b)
                                         | `Float (a, b) -> FloatVal (a -. b)) v1 v2 expr
      | (MatrixVal _, MatrixVal _) -> matrix_op (function 
                                         | `Int (a, b) -> IntVal (a - b)
                                         | `Float (a, b) -> FloatVal (a -. b)) v1 v2 expr
      | _ -> runtime_error "Subtraction type mismatch" (Some expr))
  
  | MUL (e1, e2) -> 
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | (IntVal n1, IntVal n2) -> IntVal (n1 * n2)
      | (FloatVal f1, FloatVal f2) -> FloatVal (f1 *. f2)
      | (IntVal n, FloatVal f) -> FloatVal ((float_of_int n) *. f)
      | (FloatVal f, IntVal n) -> FloatVal (f *. (float_of_int n))
      | (MatrixVal _, MatrixVal _) -> matrix_multiply v1 v2
      | (VectorVal _, IntVal _) -> vector_scalar_multiply v1 v2
      | (VectorVal _, FloatVal _) -> vector_scalar_multiply v1 v2
      | (IntVal _, VectorVal _) -> vector_scalar_multiply v2 v1
      | (FloatVal _, VectorVal _) -> vector_scalar_multiply v2 v1
      | (MatrixVal _, IntVal _) -> matrix_scalar_multiply v1 v2
      | (MatrixVal _, FloatVal _) -> matrix_scalar_multiply v1 v2
      | (IntVal _, MatrixVal _) -> matrix_scalar_multiply v2 v1
      | (FloatVal _, MatrixVal _) -> matrix_scalar_multiply v2 v1
      | (VectorVal _, VectorVal _) -> eval_expr (DOT (e1, e2)) env  (* Vector * Vector = dot product *)
      | _ -> runtime_error "Multiplication type mismatch" (Some expr))
  
  | DIV (e1, e2) -> 
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | (IntVal _, IntVal 0) | (IntVal 0 , IntVal _)-> runtime_error "Division by zero" (Some expr)
      | (FloatVal _, FloatVal 0.0) | (FloatVal 0.0 , FloatVal _) -> runtime_error "Division by zero" (Some expr)
      | (IntVal n1, IntVal n2) -> IntVal (n1 / n2)
      | (FloatVal f1, FloatVal f2) -> FloatVal (f1 /. f2)
      | (IntVal n, FloatVal f) -> FloatVal ((float_of_int n) /. f)
      | (FloatVal f, IntVal n) -> FloatVal (f /. (float_of_int n))
      | (VectorVal _, IntVal _) -> vector_scalar_divide v1 v2
      | (VectorVal _, FloatVal _) -> vector_scalar_divide v1 v2
      | (MatrixVal _, IntVal _) -> matrix_scalar_divide v1 v2
      | (MatrixVal _, FloatVal _) -> matrix_scalar_divide v1 v2
      | _ -> runtime_error "Division type mismatch" (Some expr))
  
  | MOD (e1, e2) -> 
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | (IntVal _, IntVal 0) -> runtime_error "Modulo by zero" (Some expr)
      | (IntVal n1, IntVal n2) -> IntVal (n1 mod n2)
      | _ -> runtime_error "Modulo requires integers" (Some expr))
  
  | NEG e1 -> 
      let v = eval_expr e1 env in
      (match v with
      | BoolVal b -> BoolVal (not b)
      | _ -> runtime_error "Negation requires a boolean" (Some expr))
  
  | AND (e1, e2) -> 
      let v1 = eval_expr e1 env in
      (match v1 with
      | BoolVal false -> BoolVal false  (* Short-circuit evaluation *)
      | BoolVal true -> 
          let v2 = eval_expr e2 env in
          (match v2 with
           | BoolVal b -> BoolVal b
           | _ -> runtime_error "AND requires boolean operands" (Some expr))
      | _ -> runtime_error "AND requires boolean operands" (Some expr))
  
  | OR (e1, e2) -> 
      let v1 = eval_expr e1 env in
      (match v1 with
      | BoolVal true -> BoolVal true  (* Short-circuit evaluation *)
      | BoolVal false -> 
          let v2 = eval_expr e2 env in
          (match v2 with
           | BoolVal b -> BoolVal b
           | _ -> runtime_error "OR requires boolean operands" (Some expr))
      | _ -> runtime_error "OR requires boolean operands" (Some expr))
  
  | NOT e1 -> 
      let v = eval_expr e1 env in
      (match v with
      | BoolVal b -> BoolVal (not b)
      | _ -> runtime_error "NOT requires a boolean operand" (Some expr))
  
  | VectorLit( _ ,elems) ->
      let eval_elems = List.map (fun e -> eval_expr e env) elems in
      VectorVal eval_elems
  
  | MatrixLit (_ ,_,rows) ->
      let eval_rows = List.map (fun row -> 
        List.map (fun e -> eval_expr e env) row
      ) rows in
      (* Check that all rows have the same length *)
      if eval_rows <> [] && 
         List.exists (fun row -> List.length row <> List.length (List.hd eval_rows)) eval_rows then
        runtime_error "All rows in a matrix must have the same length" (Some expr)
      else
        MatrixVal eval_rows
  
  | TRANS e ->
      let v = eval_expr e env in
      transpose v
  
  | DET e ->
      let v = eval_expr e env in 
      compute_determinant v
  
  | Input opt ->
      (match opt with
      | filename ->  read_matrix_from_file filename)
  
  | Print e ->
      let v = eval_expr e env in
      print_endline (string_of_value v);
      v
      
  | POWER (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | IntVal n1, IntVal n2 -> 
          if n2 >= 0 then IntVal (int_of_float ((float_of_int n1) ** float_of_int n2))
          else FloatVal ((float_of_int n1) ** float_of_int n2)
      | FloatVal f1, IntVal n2 -> FloatVal (f1 ** float_of_int n2)
      | IntVal n1, FloatVal f2 -> FloatVal ((float_of_int n1) ** f2)
      | FloatVal f1, FloatVal f2 -> FloatVal (f1 ** f2)
      | _ -> runtime_error "Power operation type mismatch" (Some expr))
  
  | XOR (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | BoolVal b1, BoolVal b2 -> BoolVal ((b1 || b2) && not (b1 && b2))
      | _ -> runtime_error "XOR requires boolean operands" (Some expr))
  
  | EQ (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
      | FloatVal f1, FloatVal f2 -> BoolVal (f1 = f2)
      | IntVal n, FloatVal f -> BoolVal ((float_of_int n) = f)
      | FloatVal f, IntVal n -> BoolVal (f = (float_of_int n))
      | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
      | _ -> runtime_error "Equality comparison type mismatch" (Some expr))
  
  | NEQ (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 <> n2)
      | FloatVal f1, FloatVal f2 -> BoolVal (f1 <> f2)
      | IntVal n, FloatVal f -> BoolVal ((float_of_int n) <> f)
      | FloatVal f, IntVal n -> BoolVal (f <> (float_of_int n))
      | BoolVal b1, BoolVal b2 -> BoolVal (b1 <> b2)
      | _ -> runtime_error "Inequality comparison type mismatch" (Some expr))
  
  | LT (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 < n2)
      | FloatVal f1, FloatVal f2 -> BoolVal (f1 < f2)
      | IntVal n, FloatVal f -> BoolVal ((float_of_int n) < f)
      | FloatVal f, IntVal n -> BoolVal (f < (float_of_int n))
      | _ -> runtime_error "Less than comparison type mismatch" (Some expr))
  
  | GT (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 > n2)
      | FloatVal f1, FloatVal f2 -> BoolVal (f1 > f2)
      | IntVal n, FloatVal f -> BoolVal ((float_of_int n) > f)
      | FloatVal f, IntVal n -> BoolVal (f > (float_of_int n))
      | _ -> runtime_error "Greater than comparison type mismatch" (Some expr))
   
  | LEQ (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 <= n2)
      | FloatVal f1, FloatVal f2 -> BoolVal (f1 <= f2)
      | IntVal n, FloatVal f -> BoolVal ((float_of_int n) <= f)
      | FloatVal f, IntVal n -> BoolVal (f <= (float_of_int n))
      | _ -> runtime_error "Less than or equal comparison type mismatch" (Some expr))
  
  | GEQ (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | IntVal n1, IntVal n2 -> BoolVal (n1 >= n2)
      | FloatVal f1, FloatVal f2 -> BoolVal (f1 >= f2)
      | IntVal n, FloatVal f -> BoolVal ((float_of_int n) >= f)
      | FloatVal f, IntVal n -> BoolVal (f >= (float_of_int n))
      | _ -> runtime_error "Greater than or equal comparison type mismatch" (Some expr))
  
  | DOT (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | VectorVal vec1, VectorVal vec2 ->
          if List.length vec1 <> List.length vec2 then
            runtime_error "Dot product requires vectors of equal length" (Some expr)
          else
            let products = List.map2 (fun a b ->
              match a, b with
              | IntVal n1, IntVal n2 -> IntVal (n1 * n2)
              | FloatVal f1, FloatVal f2 -> FloatVal (f1 *. f2)
              | IntVal n, FloatVal f -> FloatVal ((float_of_int n) *. f)
              | FloatVal f, IntVal n -> FloatVal (f *. (float_of_int n))
              | _, _ -> runtime_error "Vector elements must be numbers" (Some expr)
            ) vec1 vec2 in
            List.fold_left (fun acc elem ->
              match acc, elem with
              | IntVal n1, IntVal n2 -> IntVal (n1 + n2)
              | FloatVal f1, FloatVal f2 -> FloatVal (f1 +. f2)
              | IntVal n, FloatVal f -> FloatVal ((float_of_int n) +. f)
              | FloatVal f, IntVal n -> FloatVal (f +. (float_of_int n))
              | _, _ -> runtime_error "Dot product summation error" (Some expr)
            ) (IntVal 0) products
      | _ -> runtime_error "Dot product requires two vectors" (Some expr))
  
  | MAG e ->
      let v = eval_expr e env in
      (match v with
      | VectorVal vec ->
          let squares = List.map (fun elem ->
            match elem with
            | IntVal n -> float_of_int n *. float_of_int n
            | FloatVal f -> f *. f
            | _ -> runtime_error "Vector elements must be numbers" (Some expr)
          ) vec in
          let sum = List.fold_left (+.) 0.0 squares in
          FloatVal (sqrt sum)
      | _ -> runtime_error "Magnitude requires a vector" (Some expr))
  
  | ABS e ->
      let v = eval_expr e env in
      (match v with
      | IntVal n -> IntVal (abs n)
      | FloatVal f -> FloatVal (abs_float f)
      | _ -> runtime_error "Absolute value requires a number" (Some expr))
  
  | SQRT e ->
      let v = eval_expr e env in
      (match v with
      | IntVal n -> 
          if n < 0 then runtime_error "Cannot take square root of negative number" (Some expr)
          else FloatVal (sqrt (float_of_int n))
      | FloatVal f -> 
          if f < 0.0 then runtime_error "Cannot take square root of negative number" (Some expr)
          else FloatVal (sqrt f)
      | _ -> runtime_error "Square root requires a number" (Some expr))
  
  | DIM e ->
      let v = eval_expr e env in
      (match v with
      | VectorVal vec -> IntVal (List.length vec)
      | MatrixVal mat -> 
          if mat = [] then IntVal 0
          else IntVal (List.length mat * List.length (List.hd mat))
      | _ -> runtime_error "Dimension operation requires a vector or matrix" (Some expr))
  
  | ANGLE (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
      | VectorVal vec1, VectorVal vec2 ->
          if List.length vec1 <> List.length vec2 then
            runtime_error "Angle between vectors requires equal dimensions" (Some expr)
          else
            (* Calculate dot product *)
            let dot_val = match eval_expr (DOT (e1, e2)) env with
                          | FloatVal f -> f
                          | IntVal i -> float_of_int i
                          | _ -> runtime_error "Dot product calculation error" (Some expr) in
            (* Calculate magnitudes *)
            let mag1_val = match eval_expr (MAG e1) env with
                           | FloatVal f -> f
                           | _ -> runtime_error "Magnitude calculation error" (Some expr) in
            let mag2_val = match eval_expr (MAG e2) env with
                           | FloatVal f -> f
                           | _ -> runtime_error "Magnitude calculation error" (Some expr) in
            (* Calculate angle: arccos(dot / (mag1 * mag2)) *)
            FloatVal (acos (dot_val /. (mag1_val *. mag2_val)))
      | _ -> runtime_error "Angle calculation requires two vectors" (Some expr))
  
  | TRACE e ->
      let v = eval_expr e env in
      (match v with
      | MatrixVal mat ->
          if mat = [] then IntVal 0
          else if List.length mat <> List.length (List.hd mat) then
            runtime_error "Trace requires a square matrix" (Some expr)
          else
            (* Sum of diagonal elements *)
            let diagonal = List.mapi (fun i row -> 
              try List.nth row i
              with _ -> runtime_error "Irregular matrix in trace calculation" (Some expr)
            ) mat in
            List.fold_left (fun acc elem ->
              match acc, elem with
              | IntVal n1, IntVal n2 -> IntVal (n1 + n2)
              | FloatVal f1, FloatVal f2 -> FloatVal (f1 +. f2)
              | IntVal n, FloatVal f -> FloatVal ((float_of_int n) +. f)
              | FloatVal f, IntVal n -> FloatVal (f +. (float_of_int n))
              | _, _ -> runtime_error "Matrix elements must be numbers" (Some expr)
            ) (IntVal 0) diagonal
      | _ -> runtime_error "Trace requires a matrix" (Some expr))
  
  | Index (e, idx1, idx2_opt) ->
      let v = eval_expr e env in
      let i1 = match eval_expr idx1 env with
               | IntVal n -> n
               | _ -> runtime_error "Index must be an integer" (Some expr) in
      (match idx2_opt, v with
      | None, VectorVal vec ->
          if i1 < 0 || i1 >= List.length vec then
            runtime_error ("Vector index out of bounds: " ^ string_of_int i1) (Some expr)
          else
            List.nth vec i1
      | Some idx2, MatrixVal mat ->
          let i2 = match eval_expr idx2 env with
                   | IntVal n -> n
                   | _ -> runtime_error "Index must be an integer" (Some expr) in
          if i1 < 0 || i1 >= List.length mat then
            runtime_error ("Matrix row index out of bounds: " ^ string_of_int i1) (Some expr)
          else
            let row = List.nth mat i1 in
            if i2 < 0 || i2 >= List.length row then
              runtime_error ("Matrix column index out of bounds: " ^ string_of_int i2) (Some expr)
            else
              List.nth row i2
      | None, _ -> runtime_error "Single index requires a vector" (Some expr)
      | Some _, _ -> runtime_error "Double index requires a matrix" (Some expr))
  

let rec eval_stmt stmt env =
  match stmt with
  | ExprStmt e -> let _ = eval_expr e env in env
      
  | DeclStmt (id, _, None) -> extend env id UnitVal
      
  | DeclStmt (id, _, Some e) ->
      let v = eval_expr e env in extend env id v
      
  | AssignStmt (id, e) ->
      let v = eval_expr e env in update env id v

  | ArrayAssignStmt (id, idx1, idx2_opt, e) ->
    (* First evaluate the indices and the value *)
    let i1 = match eval_expr idx1 env with
             | IntVal n -> n
             | _ -> runtime_error "Index must be an integer" (Some idx1) in
    let i2_opt = match idx2_opt with
                | None -> None
                | Some idx2 -> match eval_expr idx2 env with
                               | IntVal n -> Some n
                               | _ -> runtime_error "Index must be an integer" (Some idx2) in
    let v = eval_expr e env in
    
    (* Get the array value from the environment *)
    let arr = lookup env id in
    
    (* Update the array value based on the indices *)
    let updated_arr = match arr, i2_opt with
      | VectorVal vec, None ->
          if i1 < 0 || i1 >= List.length vec then
            runtime_error ("Vector index out of bounds: " ^ string_of_int i1) (Some idx1)
          else
            VectorVal (List.mapi (fun i el -> if i = i1 then v else el) vec)
      | MatrixVal mat, Some i2 ->
          if i1 < 0 || i1 >= List.length mat then
            runtime_error ("Matrix row index out of bounds: " ^ string_of_int i1) (Some idx1)
          else
            let row = List.nth mat i1 in
            if i2 < 0 || i2 >= List.length row then
              runtime_error ("Matrix column index out of bounds: " ^ string_of_int i2) 
              (match idx2_opt with Some idx2 -> Some idx2 | None -> None)
            else
              MatrixVal (List.mapi (fun i r -> 
                if i = i1 then 
                  List.mapi (fun j el -> if j = i2 then v else el) r
                else r
              ) mat)
      | _, None -> runtime_error "Single index requires a vector" (Some (Var id))
      | _, Some _ -> runtime_error "Double index requires a matrix" (Some (Var id))
    in
    
    (* Update the environment with the modified array *)
    update env id updated_arr

  | IfStmt (cond, then_block, else_opt) ->
      let v = eval_expr cond env in
      (match v with
      | BoolVal true -> eval_block then_block env
      | BoolVal false -> 
          (match else_opt with
           | Some else_block -> eval_block else_block env
           | None -> env)
      | _ -> runtime_error "Condition in if statement must be boolean" (Some cond))
      
  | WhileStmt (cond, body) ->
      let rec loop env' =
        let v = eval_expr cond env' in
        match v with
        | BoolVal true -> 
            let new_env = eval_block body env' in
            loop new_env
        | BoolVal false -> env'
        | _ -> runtime_error "Condition in while loop must be boolean" (Some cond)
      in
      loop env
      
  | ForStmt (init, cond, update, body) ->
      (* First evaluate initialization statement *)
      let env_init = eval_stmt init env in
      
      (* Define a recursive loop function that:
         1. Checks condition
         2. Evaluates body
         3. Performs update
         4. Repeats until condition is false *)
      let rec loop env_loop =
        let v = eval_expr cond env_loop in
        match v with
        | BoolVal true ->
            let env_body = eval_block body env_loop in
            let env_update = eval_stmt update env_body in
            loop env_update
        | BoolVal false -> env_loop
        | _ -> runtime_error "For loop condition must be boolean" (Some cond)
      in
      
      loop env_init
      
  | Block stmts -> eval_block stmts env

(* Helper function for block evaluation *)
and eval_block stmts env =
  List.fold_left (fun env stmt -> eval_stmt stmt env) env stmts

let eval_program = function
  | Program stmts -> 
      try
        let _ = List.fold_left (fun env stmt -> eval_stmt stmt env) empty_env stmts in
        UnitVal
      with
      | RuntimeError (msg, _) -> 
          print_endline ("\027[31m" ^ msg ^ "\027[0m");
          exit 1
