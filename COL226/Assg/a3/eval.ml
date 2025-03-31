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

(* Environment mapping variable names to values *)
type env = (string * value) list

let empty_env : env = []

(* Lookup a variable in the environment *)
let rec lookup (rho : env) (x : string) : value =
  match rho with
  | [] -> failwith ("Runtime Error: Unbound variable '" ^ x ^ "'")
  | (y, v) :: rest -> if x = y then v else lookup rest x

(* Extend the environment with a new binding *)
let extend (rho : env) (x : string) (v : value) : env =
  (x, v) :: rho

(* Update an existing variable in the environment *)
let rec update (rho : env) (x : string) (v : value) : env =
  match rho with
  | [] -> [(x, v)]  (* Add binding if it doesn't exist *)
  | (y, _) :: rest when x = y -> (x, v) :: rest
  | pair :: rest -> pair :: update rest x v

(* Convert a runtime value to a string *)
let rec string_of_value (v : value) : string =
  match v with
  | IntVal n -> string_of_int n
  | FloatVal f -> string_of_float f
  | BoolVal b -> string_of_bool b
  | VectorVal vs ->
      "[" ^ (String.concat ", " (List.map string_of_value vs)) ^ "]"
  | MatrixVal rows ->
      let row_strs = List.map (fun row ->
        "[" ^ (String.concat ", " (List.map string_of_value row)) ^ "]"
      ) rows in
      "[" ^ (String.concat "\n " row_strs) ^ "]"
  | UnitVal -> "()"

(* Vector and matrix operations *)
let vector_scalar_multiply v scalar = 
  match v with
  | VectorVal vs ->
      VectorVal (List.map (fun x ->
        match x, scalar with
        | IntVal n, IntVal s -> IntVal (n * s)
        | IntVal n, FloatVal s -> FloatVal ((float_of_int n) *. s)
        | FloatVal f, IntVal s -> FloatVal (f *. (float_of_int s))
        | FloatVal f, FloatVal s -> FloatVal (f *. s)
        | _, _ -> failwith "Runtime error: Vector elements must be numbers"
      ) vs)
  | _ -> failwith "Runtime error: Vector-scalar multiplication requires a vector"

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
          | _, _ -> failwith "Runtime error: Matrix elements must be numbers"
        ) row
      ) rows)
  | _ -> failwith "Runtime error: Matrix-scalar multiplication requires a matrix"

let vector_scalar_divide v scalar =
  match scalar with
  | IntVal 0 -> failwith "Runtime error: Division by zero"
  | FloatVal 0.0 -> failwith "Runtime error: Division by zero"
  | _ ->
    match v with
    | VectorVal vs ->
        VectorVal (List.map (fun x ->
          match x, scalar with
          | IntVal n, IntVal s -> IntVal (n / s)
          | IntVal n, FloatVal s -> FloatVal ((float_of_int n) /. s)
          | FloatVal f, IntVal s -> FloatVal (f /. (float_of_int s))
          | FloatVal f, FloatVal s -> FloatVal (f /. s)
          | _, _ -> failwith "Runtime error: Vector elements must be numbers"
        ) vs)
    | _ -> failwith "Runtime error: Vector-scalar division requires a vector"

let matrix_scalar_divide m scalar =
  match scalar with
  | IntVal 0 -> failwith "Runtime error: Division by zero"
  | FloatVal 0.0 -> failwith "Runtime error: Division by zero"
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
            | _, _ -> failwith "Runtime error: Matrix elements must be numbers"
          ) row
        ) rows)
    | _ -> failwith "Runtime error: Matrix-scalar division requires a matrix"

let vector_add v1 v2 =
  match v1, v2 with
  | VectorVal vs1, VectorVal vs2 ->
      if List.length vs1 <> List.length vs2 then
        failwith "Runtime error: Vector addition requires equal dimensions"
      else
        VectorVal (List.map2 (fun a b ->
          match a, b with
          | IntVal x, IntVal y -> IntVal (x + y)
          | FloatVal x, FloatVal y -> FloatVal (x +. y)
          | IntVal x, FloatVal y -> FloatVal ((float_of_int x) +. y)
          | FloatVal x, IntVal y -> FloatVal (x +. (float_of_int y))
          | _, _ -> failwith "Runtime error: Vector elements must be numbers"
        ) vs1 vs2)
  | _, _ -> failwith "Runtime error: Vector addition requires vectors"

let matrix_add m1 m2 =
  match m1, m2 with
  | MatrixVal rows1, MatrixVal rows2 ->
      if List.length rows1 <> List.length rows2 then 
        failwith "Runtime error: Matrix addition requires equal dimensions"
      else if List.length rows1 > 0 && 
              (List.length (List.hd rows1) <> List.length (List.hd rows2)) then
        failwith "Runtime error: Matrix addition requires equal dimensions"
      else
        MatrixVal (List.map2 (fun r1 r2 ->
          List.map2 (fun a b ->
            match a, b with
            | IntVal x, IntVal y -> IntVal (x + y)
            | FloatVal x, FloatVal y -> FloatVal (x +. y)
            | IntVal x, FloatVal y -> FloatVal ((float_of_int x) +. y)
            | FloatVal x, IntVal y -> FloatVal (x +. (float_of_int y))
            | _, _ -> failwith "Runtime error: Matrix elements must be numbers"
          ) r1 r2
        ) rows1 rows2)
  | _, _ -> failwith "Runtime error: Matrix addition requires matrices"

let transpose matrix =
  match matrix with
  | MatrixVal rows ->
      if rows = [] then MatrixVal []
      else
        let cols = List.length (List.hd rows) in
        let transposed = List.init cols (fun j ->
          List.map (fun row ->
            try List.nth row j
            with _ -> failwith "Runtime error: Irregular matrix found during transpose"
          ) rows
        ) in
        MatrixVal transposed
  | _ -> failwith "Runtime error: Transpose requires a matrix"

let matrix_multiply m1 m2 =
  match m1, m2 with
  | MatrixVal rows1, MatrixVal rows2 ->
      if rows1 = [] || rows2 = [] then
        failwith "Runtime error: Cannot multiply empty matrices"
      else
        (* Get transpose of m2 for easier column access *)
        let cols2 = match transpose (MatrixVal rows2) with
                    | MatrixVal cols -> cols
                    | _ -> failwith "Impossible: Transpose returned non-matrix" in
        
        (* Check compatible dimensions *)
        if List.length (List.hd rows1) <> List.length rows2 then
          failwith "Runtime error: Matrix multiplication dimension mismatch"
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
                | _, _ -> failwith "Runtime error: Matrix elements must be numbers"
              ) row1 col2 in
              
              (* Sum the products *)
              List.fold_left (fun acc elem ->
                match acc, elem with
                | IntVal x, IntVal y -> IntVal (x + y)
                | FloatVal x, FloatVal y -> FloatVal (x +. y)
                | IntVal x, FloatVal y -> FloatVal ((float_of_int x) +. y)
                | FloatVal x, IntVal y -> FloatVal (x +. (float_of_int y))
                | _, _ -> failwith "Runtime error: Matrix elements must be numbers"
              ) (IntVal 0) products
            ) cols2
          ) rows1 in
          MatrixVal result
  | _, _ -> failwith "Runtime error: Matrix multiplication requires matrices"

let compute_determinant matrix =
  match matrix with
  | MatrixVal rows ->
      if rows = [] then
        IntVal 0
      else if List.length rows <> List.length (List.hd rows) then
        failwith "Runtime error: Determinant requires a square matrix"
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
            | _ -> failwith "Runtime error: Matrix elements must be numbers" in
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
              | IntVal a', IntVal b', IntVal c', IntVal d' ->
                  IntVal ((a' * d') - (b' * c'))
              | _ -> failwith "Runtime error: Mixed types in determinant"
            else
              (* Expand along first row *)
              let sum = ref (IntVal 0) in
              for j = 0 to size - 1 do
                let element = List.nth (List.nth mat 0) j in
                let minor_mat = match minor 0 j mat with
                               | MatrixVal m -> m
                               | _ -> failwith "Impossible" in
                let minor_det = det_helper minor_mat (size - 1) in
                let sign = if j mod 2 = 0 then 1 else -1 in
                
                (* Multiply and add to sum *)
                match !sum, element, minor_det with
                | IntVal s, IntVal e, IntVal md ->
                    sum := IntVal (s + sign * e * md)
                | _ -> failwith "Runtime error: Mixed types in determinant"
              done;
              !sum in
          
          det_helper rows size
  | _ -> failwith "Runtime error: Determinant requires a matrix"

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
      failwith "Runtime error: Empty file or no valid numbers found"
    else if List.exists (fun row -> List.length row <> List.length (List.hd matrix)) matrix then
      failwith "Runtime error: Inconsistent row lengths in matrix file"
    else
      MatrixVal matrix
  with
  | Sys_error msg -> failwith ("Runtime error: " ^ msg)
  | Failure msg -> failwith ("Runtime error: " ^ msg)

(* Main evaluation functions *)
let rec eval_expr (e : expr) (rho : env) : value =
  match e with
  | IntLit n -> IntVal n
  | FloatLit f -> FloatVal f
  | BoolLit b -> BoolVal b
  | Var name -> lookup rho name
  | PLUS (e1, e2) -> 
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> IntVal (n1 + n2)
       | FloatVal f1, FloatVal f2 -> FloatVal (f1 +. f2)
       | IntVal n, FloatVal f -> FloatVal ((float_of_int n) +. f)
       | FloatVal f, IntVal n -> FloatVal (f +. (float_of_int n))
       | VectorVal _, VectorVal _ -> vector_add v1 v2
       | MatrixVal _, MatrixVal _ -> matrix_add v1 v2
       | _, _ -> failwith "Runtime error: Addition type mismatch")
  | MINUS (e1, e2) -> 
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> IntVal (n1 - n2)
       | FloatVal f1, FloatVal f2 -> FloatVal (f1 -. f2)
       | IntVal n, FloatVal f -> FloatVal ((float_of_int n) -. f)
       | FloatVal f, IntVal n -> FloatVal (f -. (float_of_int n))
       | VectorVal _, VectorVal _ -> vector_add v1 (vector_scalar_multiply v2 (IntVal (-1)))
       | MatrixVal _, MatrixVal _ -> matrix_add v1 (matrix_scalar_multiply v2 (IntVal (-1)))
       | _, _ -> failwith "Runtime error: Subtraction type mismatch")
  | MUL (e1, e2) -> 
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> IntVal (n1 * n2)
       | FloatVal f1, FloatVal f2 -> FloatVal (f1 *. f2)
       | IntVal n, FloatVal f -> FloatVal ((float_of_int n) *. f)
       | FloatVal f, IntVal n -> FloatVal (f *. (float_of_int n))
       | MatrixVal _, MatrixVal _ -> matrix_multiply v1 v2
       | VectorVal _, IntVal _ -> vector_scalar_multiply v1 v2
       | VectorVal _, FloatVal _ -> vector_scalar_multiply v1 v2
       | IntVal _, VectorVal _ -> vector_scalar_multiply v2 v1
       | FloatVal _, VectorVal _ -> vector_scalar_multiply v2 v1
       | MatrixVal _, IntVal _ -> matrix_scalar_multiply v1 v2
       | MatrixVal _, FloatVal _ -> matrix_scalar_multiply v1 v2
       | IntVal _, MatrixVal _ -> matrix_scalar_multiply v2 v1
       | FloatVal _, MatrixVal _ -> matrix_scalar_multiply v2 v1
       | VectorVal _, VectorVal _ -> eval_expr (DOT (e1, e2)) rho  (* Vector * Vector = dot product *)
       | _, _ -> failwith "Runtime error: Multiplication type mismatch")
  | DIV (e1, e2) -> 
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal _, IntVal 0 -> failwith "Runtime error: Division by zero"
       | FloatVal _, FloatVal 0.0 -> failwith "Runtime error: Division by zero"
       | IntVal n1, IntVal n2 -> IntVal (n1 / n2)
       | FloatVal f1, FloatVal f2 -> FloatVal (f1 /. f2)
       | IntVal n, FloatVal f -> FloatVal ((float_of_int n) /. f)
       | FloatVal f, IntVal n -> FloatVal (f /. (float_of_int n))
       | VectorVal _, IntVal _ -> vector_scalar_divide v1 v2
       | VectorVal _, FloatVal _ -> vector_scalar_divide v1 v2
       | MatrixVal _, IntVal _ -> matrix_scalar_divide v1 v2
       | MatrixVal _, FloatVal _ -> matrix_scalar_divide v1 v2
       | _, _ -> failwith "Runtime error: Division type mismatch")
  | MOD (e1, e2) -> 
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal _, IntVal 0 -> failwith "Runtime error: Modulo by zero"
       | IntVal n1, IntVal n2 -> IntVal (n1 mod n2)
       | _, _ -> failwith "Runtime error: Modulo requires integers")
  | NEG e1 -> 
      let v = eval_expr e1 rho in
      (match v with
       | IntVal n -> IntVal (-n)
       | FloatVal f -> FloatVal (-.f)
       | _ -> failwith "Runtime error: Negation requires a number")
  | AND (e1, e2) -> 
      let v1 = eval_expr e1 rho in
      (match v1 with
       | BoolVal false -> BoolVal false  (* Short-circuit evaluation *)
       | BoolVal true -> 
           let v2 = eval_expr e2 rho in
           (match v2 with
            | BoolVal b -> BoolVal b
            | _ -> failwith "Runtime error: AND requires boolean operands")
       | _ -> failwith "Runtime error: AND requires boolean operands")
  | OR (e1, e2) -> 
      let v1 = eval_expr e1 rho in
      (match v1 with
       | BoolVal true -> BoolVal true  (* Short-circuit evaluation *)
       | BoolVal false -> 
           let v2 = eval_expr e2 rho in
           (match v2 with
            | BoolVal b -> BoolVal b
            | _ -> failwith "Runtime error: OR requires boolean operands")
       | _ -> failwith "Runtime error: OR requires boolean operands")
  | NOT e1 -> 
      let v = eval_expr e1 rho in
      (match v with
       | BoolVal b -> BoolVal (not b)
       | _ -> failwith "Runtime error: NOT requires a boolean operand")
  | VectorLit( _ ,elems) ->
      let eval_elems = List.map (fun e -> eval_expr e rho) elems in
      VectorVal eval_elems
  | MatrixLit (_ ,_,rows) ->
      let eval_rows = List.map (fun row -> 
        List.map (fun e -> eval_expr e rho) row
      ) rows in
      (* Check that all rows have the same length *)
      if eval_rows <> [] && 
         List.exists (fun row -> List.length row <> List.length (List.hd eval_rows)) eval_rows then
        failwith "Runtime error: All rows in a matrix must have the same length"
      else
        MatrixVal eval_rows
  | TRANS e ->
      let v = eval_expr e rho in
      transpose v
  | DET e ->
      let v = eval_expr e rho in
      compute_determinant v
  | Input opt ->
      (match opt with
      | filename ->  read_matrix_from_file filename)
  
  | Print e ->
      let v = eval_expr e rho in
      print_endline (string_of_value v);
      v
      
  | POWER (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> 
           if n2 >= 0 then IntVal (int_of_float ((float_of_int n1) ** float_of_int n2))
           else FloatVal ((float_of_int n1) ** float_of_int n2)
       | FloatVal f1, IntVal n2 -> FloatVal (f1 ** float_of_int n2)
       | IntVal n1, FloatVal f2 -> FloatVal ((float_of_int n1) ** f2)
       | FloatVal f1, FloatVal f2 -> FloatVal (f1 ** f2)
       | _, _ -> failwith "Runtime error: Power operation type mismatch")
  
  | XOR (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | BoolVal b1, BoolVal b2 -> BoolVal ((b1 || b2) && not (b1 && b2))
       | _, _ -> failwith "Runtime error: XOR requires boolean operands")
  
  | EQ (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
       | FloatVal f1, FloatVal f2 -> BoolVal (f1 = f2)
       | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
       | _, _ -> failwith "Runtime error: Equality comparison type mismatch")
  
  | NEQ (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> BoolVal (n1 <> n2)
       | FloatVal f1, FloatVal f2 -> BoolVal (f1 <> f2)
       | BoolVal b1, BoolVal b2 -> BoolVal (b1 <> b2)
       | _, _ -> failwith "Runtime error: Inequality comparison type mismatch")
  
  | LT (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> BoolVal (n1 < n2)
       | FloatVal f1, FloatVal f2 -> BoolVal (f1 < f2)
       | _, _ -> failwith "Runtime error: Less than comparison type mismatch")
  
  | GT (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> BoolVal (n1 > n2)
       | FloatVal f1, FloatVal f2 -> BoolVal (f1 > f2)
       | _, _ -> failwith "Runtime error: Greater than comparison type mismatch")
   
  | LEQ (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> BoolVal (n1 <= n2)
       | FloatVal f1, FloatVal f2 -> BoolVal (f1 <= f2)
       | _, _ -> failwith "Runtime error: Less than or equal comparison type mismatch")
  
  | GEQ (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | IntVal n1, IntVal n2 -> BoolVal (n1 >= n2)
       | FloatVal f1, FloatVal f2 -> BoolVal (f1 >= f2)
       | _, _ -> failwith "Runtime error: Greater than or equal comparison type mismatch")
  
  | DOT (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | VectorVal vec1, VectorVal vec2 ->
           if List.length vec1 <> List.length vec2 then
             failwith "Runtime error: Dot product requires vectors of equal length"
           else
             let products = List.map2 (fun a b ->
               match a, b with
               | IntVal n1, IntVal n2 -> IntVal (n1 * n2)
               | FloatVal f1, FloatVal f2 -> FloatVal (f1 *. f2)
               | IntVal n, FloatVal f -> FloatVal ((float_of_int n) *. f)
               | FloatVal f, IntVal n -> FloatVal (f *. (float_of_int n))
               | _, _ -> failwith "Runtime error: Vector elements must be numbers"
             ) vec1 vec2 in
             List.fold_left (fun acc elem ->
               match acc, elem with
               | IntVal n1, IntVal n2 -> IntVal (n1 + n2)
               | FloatVal f1, FloatVal f2 -> FloatVal (f1 +. f2)
               | IntVal n, FloatVal f -> FloatVal ((float_of_int n) +. f)
               | FloatVal f, IntVal n -> FloatVal (f +. (float_of_int n))
               | _, _ -> failwith "Runtime error: Dot product summation error"
             ) (IntVal 0) products
       | _, _ -> failwith "Runtime error: Dot product requires two vectors")
  
  | MAG e ->
      let v = eval_expr e rho in
      (match v with
       | VectorVal vec ->
           let squares = List.map (fun elem ->
             match elem with
             | IntVal n -> float_of_int n *. float_of_int n
             | FloatVal f -> f *. f
             | _ -> failwith "Runtime error: Vector elements must be numbers"
           ) vec in
           let sum = List.fold_left (+.) 0.0 squares in
           FloatVal (sqrt sum)
       | _ -> failwith "Runtime error: Magnitude requires a vector")
  
  | ABS e ->
      let v = eval_expr e rho in
      (match v with
       | IntVal n -> IntVal (abs n)
       | FloatVal f -> FloatVal (abs_float f)
       | _ -> failwith "Runtime error: Absolute value requires a number")
  
  | SQRT e ->
      let v = eval_expr e rho in
      (match v with
       | IntVal n -> 
           if n < 0 then failwith "Runtime error: Cannot take square root of negative number"
           else FloatVal (sqrt (float_of_int n))
       | FloatVal f -> 
           if f < 0.0 then failwith "Runtime error: Cannot take square root of negative number"
           else FloatVal (sqrt f)
       | _ -> failwith "Runtime error: Square root requires a number")
  
  | DIM e ->
      let v = eval_expr e rho in
      (match v with
       | VectorVal vec -> IntVal (List.length vec)
       | MatrixVal mat -> 
           if mat = [] then IntVal 0
           else IntVal (List.length mat * List.length (List.hd mat))
       | _ -> failwith "Runtime error: Dimension operation requires a vector or matrix")
  
  | ANGLE (e1, e2) ->
      let v1 = eval_expr e1 rho in
      let v2 = eval_expr e2 rho in
      (match v1, v2 with
       | VectorVal vec1, VectorVal vec2 ->
           if List.length vec1 <> List.length vec2 then
             failwith "Runtime error: Angle between vectors requires equal dimensions"
           else
             (* Calculate dot product *)
             let dot_val = match eval_expr (DOT (e1, e2)) rho with
                           | FloatVal f -> f
                           | IntVal i -> float_of_int i
                           | _ -> failwith "Runtime error: Dot product calculation error" in
             (* Calculate magnitudes *)
             let mag1_val = match eval_expr (MAG e1) rho with
                            | FloatVal f -> f
                            | _ -> failwith "Runtime error: Magnitude calculation error" in
             let mag2_val = match eval_expr (MAG e2) rho with
                            | FloatVal f -> f
                            | _ -> failwith "Runtime error: Magnitude calculation error" in
             (* Calculate angle: arccos(dot / (mag1 * mag2)) *)
             FloatVal (acos (dot_val /. (mag1_val *. mag2_val)))
       | _, _ -> failwith "Runtime error: Angle calculation requires two vectors")
  
  | TRACE e ->
      let v = eval_expr e rho in
      (match v with
       | MatrixVal mat ->
           if mat = [] then IntVal 0
           else if List.length mat <> List.length (List.hd mat) then
             failwith "Runtime error: Trace requires a square matrix"
           else
             (* Sum of diagonal elements *)
             let diagonal = List.mapi (fun i row -> 
               try List.nth row i
               with _ -> failwith "Runtime error: Irregular matrix in trace calculation"
             ) mat in
             List.fold_left (fun acc elem ->
               match acc, elem with
               | IntVal n1, IntVal n2 -> IntVal (n1 + n2)
               | FloatVal f1, FloatVal f2 -> FloatVal (f1 +. f2)
               | IntVal n, FloatVal f -> FloatVal ((float_of_int n) +. f)
               | FloatVal f, IntVal n -> FloatVal (f +. (float_of_int n))
               | _, _ -> failwith "Runtime error: Matrix elements must be numbers"
             ) (IntVal 0) diagonal
       | _ -> failwith "Runtime error: Trace requires a matrix")
  
  | Index (e, idx1, idx2_opt) ->
      let v = eval_expr e rho in
      let i1 = match eval_expr idx1 rho with
               | IntVal n -> n
               | _ -> failwith "Runtime error: Index must be an integer" in
      match idx2_opt, v with
      | None, VectorVal vec ->
          if i1 < 0 || i1 >= List.length vec then
            failwith ("Runtime error: Vector index out of bounds: " ^ string_of_int i1)
          else
            List.nth vec i1
      | Some idx2, MatrixVal mat ->
          let i2 = match eval_expr idx2 rho with
                   | IntVal n -> n
                   | _ -> failwith "Runtime error: Index must be an integer" in
          if i1 < 0 || i1 >= List.length mat then
            failwith ("Runtime error: Matrix row index out of bounds: " ^ string_of_int i1)
          else
            let row = List.nth mat i1 in
            if i2 < 0 || i2 >= List.length row then
              failwith ("Runtime error: Matrix column index out of bounds: " ^ string_of_int i2)
            else
              List.nth row i2
      | None, _ -> failwith "Runtime error: Single index requires a vector"
      | Some _, _ -> failwith "Runtime error: Double index requires a matrix"
  

let rec eval_stmt (s : stmt) (rho : env) =
  match s with
  | ExprStmt e ->
      (* For expression statements, we need to handle Assign specially *)
      (match e with
       | Assign(id, e') ->
           let v = eval_expr e' rho in
           update rho id v  (* Update the environment for assign statements *)
       | _ -> 
           let _ = eval_expr e rho in
           rho)
      
  | DeclStmt (id, _, None) -> 
      (* Variable declaration without initialization - use UnitVal as default *)
      extend rho id UnitVal
      
  | DeclStmt (id, _, Some e) ->
      (* Variable declaration with initialization *)
      let v = eval_expr e rho in
      extend rho id v
      
  | AssignStmt (id, e) ->
      let v = eval_expr e rho in
      update rho id v
      
  | IfStmt (cond, then_block, else_opt) ->
      let v = eval_expr cond rho in
      (match v with
       | BoolVal true -> eval_block then_block rho
       | BoolVal false -> 
           (match else_opt with
            | Some else_block -> eval_block else_block rho
            | None -> rho)
       | _ -> failwith "Runtime error: Condition in if statement must be boolean")
      
  | WhileStmt (cond, body) ->
      let rec loop rho' =
        let v = eval_expr cond rho' in
        match v with
        | BoolVal true -> 
            let new_rho = eval_block body rho' in
            loop new_rho
        | BoolVal false -> rho'
        | _ -> failwith "Runtime error: Condition in while loop must be boolean"
      in
      loop rho
      
  | ForStmt (init, cond, update, body) ->
      (* First evaluate initialization statement *)
      let rho_init = eval_stmt init rho in
      
      (* Define a recursive loop function that:
         1. Checks condition
         2. Evaluates body
         3. Performs update
         4. Repeats until condition is false *)
      let rec loop rho_loop =
        let v = eval_expr cond rho_loop in
        match v with
        | BoolVal true ->
            let rho_body = eval_block body rho_loop in
            let rho_update = eval_stmt update rho_body in
            loop rho_update
        | BoolVal false -> rho_loop
        | _ -> failwith "Runtime error: For loop condition must be boolean"
      in
      
      loop rho_init
      
  | Block stmts -> eval_block stmts rho

(* Helper function for block evaluation *)
and eval_block (stmts : stmt list) (rho : env) : env =
  List.fold_left (fun env stmt -> eval_stmt stmt env) rho stmts

let eval_program (p : program) : value =
  match p with
  | Program stmts -> 
      (* Fold over each statement in the program, threading the environment through *)
      let _final_env = List.fold_left 
        (fun env stmt -> eval_stmt stmt env) 
        empty_env 
        stmts 
      in
      UnitVal
