open A2;;
(* 
--------------- 
TYPE CHECKING TESTS 
--------------- 
*)

(*----- 
  BASIC TYPE CHECKING 
-------*)

(* Test 1: Test if the type of boolean True (T) is Bool *)
let%test "type_of_bool_T" = (type_of T = Bool)

(* Test 2: Test if the type of boolean False (F) is Bool *)
let%test "type_of_bool_F" = (type_of F = Bool)

(* Test 3: Test if the type of a scalar constant (ConstS 3.14) is Scalar *)
let%test "type_of_scalar" = (type_of (ConstS 3.14) = Scalar)

(* Test 4: Test if the type of a vector constant (ConstV [1.0; 2.0]) is Vector with length 2 *)
let%test "type_of_vector" = (type_of (ConstV [1.0; 2.0]) = Vector 2)

(* Test 5: Empty vector should raise Wrong exception *)
let%test "type_of_empty_vector" =
  (try
     let _ = type_of (ConstV []) in false
   with
   | Wrong _ -> true
   | _ -> false)

(*----- 
  ADD OPERATION TYPE CHECKING 
-------*)

(* Test 1: Test if the type of adding two booleans is Bool *)
let%test "type_of_add_bools" = (type_of (Add(T, F)) = Bool)

(* Test 2: Test if the type of adding two scalars is Scalar *)
let%test "type_of_add_scalars" = (type_of (Add(ConstS 1.0, ConstS 2.0)) = Scalar)

(* Test 3: Test if the type of adding two vectors of same dimensions is Vector with that dimension *)
let%test "type_of_add_vectors" = (type_of (Add(ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* Test 4: Adding empty vectors should raise Wrong exception *)
let%test "type_of_add_empty_vectors" =
  (try
     let _ = type_of (Add(ConstV [], ConstV [])) in false
   with
   | Wrong _ -> true
   | _ -> false)

(* Test 5: Adding empty vector and non-empty vector should raise Wrong exception *)
let%test "type_of_add_empty_and_nonempty_vectors" =
  (try
     let _ = type_of (Add(ConstV [], ConstV [1.0; 2.0])) in false
   with
   | Wrong _ -> true
   | _ -> false)

(* Test 6: Adding vectors of different dimensions should raise Wrong exception *)
let%test "type_of_add_vectors_dimension_mismatch" =
  (try
     let _ = type_of (Add(ConstV [1.0], ConstV [1.0; 2.0])) in false
   with
   | Wrong _ -> true
   | _ -> false)

(* Test 7: Adding boolean and scalar should raise Wrong exception *)
let%test "type_of_add_bool_scalar" =
  (try
     let _ = type_of (Add(T, ConstS 1.0)) in false
   with
   | Wrong _ -> true
   | _ -> false)

(*----- 
  INV OPERATION TYPE CHECKING 
-------*)

(* Test 1: Basic Inv type checking on booleans *)
let%test "type_of_inv_bool" = (type_of (Inv(T)) = Bool)

(* Test 2: Inv on a scalar should be Scalar *)
let%test "type_of_inv_scalar" = (type_of (Inv(ConstS 3.14)) = Scalar)

(* Test 3: Inv on a vector should remain a vector with the same dimension *)
let%test "type_of_inv_vector" = (type_of (Inv(ConstV [1.0; 2.0])) = Vector 2)

(* Test 4: Inv with an empty vector should raise Wrong exception *)
let%test "type_of_inv_empty_vector" =
  (try
     let _ = type_of (Inv(ConstV [])) in false
   with
   | Wrong _ -> true
   | _ -> false)

(* Test 5: Double Inv on a boolean should preserve the type *)
let%test "type_of_double_inv_bool" = (type_of (Inv(Inv(T))) = Bool)

(* Test 6: Double Inv on a scalar should preserve the type *)
let%test "type_of_double_inv_scalar" = (type_of (Inv(Inv(ConstS 1.0))) = Scalar)

(*----- 
  SCALPROD OPERATION TYPE CHECKING 
-------*)

(* Test 1: ScalProd on booleans should return Bool *)
let%test "type_of_scalprod_bool" = (type_of (ScalProd(T, T)) = Bool)

(* Test 2: ScalProd of two scalars should be Scalar *)
let%test "type_of_scalprod_scalars" = (type_of (ScalProd(ConstS 2.0, ConstS 3.0)) = Scalar)

(* Test 3: ScalProd between a scalar and a vector (scalar first) should yield a vector with the same dimensions *)
let%test "type_of_scalprod_scalar_vector" = (type_of (ScalProd(ConstS 2.0, ConstV [1.0; 2.0])) = Vector 2)

(* Test 4: ScalProd between a vector and a scalar (vector first) should yield a vector with the same dimensions *)
let%test "type_of_scalprod_vector_scalar" = (type_of (ScalProd(ConstV [1.0; 2.0], ConstS 2.0)) = Vector 2)

(* Test 5: ScalProd with an empty vector should raise Wrong exception *)
let%test "type_of_scalprod_empty_vector" =
  (try
     let _ = type_of (ScalProd(ConstS 2.0, ConstV [])) in false
   with
   | Wrong _ -> true
   | _ -> false)

(*----- 
  DOTPROD OPERATION TYPE CHECKING 
  -------*)

(* Test 1: Dot product of two vectors with same dimensions, expected type Scalar *)
let%test "type_of_dotprod_vectors" =
  (type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Scalar)

(* Test 2: Dot product of vectors with different dimensions should raise Wrong exception *)
let%test "type_of_dotprod_different_dimensions" =
  (try 
     let _ = type_of (DotProd (ConstV [1.0], ConstV [1.0; 2.0])) in false
   with
   | Wrong _ -> true)

(* Test 3: Dot product of empty vectors should raise Wrong exception *)
let%test "type_of_dotprod_empty_vectors" =
  (try 
     let _ = type_of (DotProd (ConstV [], ConstV [])) in false
   with
   | Wrong _ -> true)


(*----- 
  MAG OPERATION TYPE CHECKING 
  -------*)

(* Test 1: Magnitude of a scalar; expected type Scalar *)
let%test "type_of_mag_scalar" =
  (type_of (Mag (ConstS 3.14)) = Scalar)

(* Test 2: Magnitude of a vector; expected type Scalar *)
let%test "type_of_mag_vector" =
  (type_of (Mag (ConstV [3.0; 4.0])) = Scalar)

(* Test 3: Magnitude of an empty vector should raise Wrong exception *)
let%test "type_of_mag_empty_vector" =
  (try 
     let _ = type_of (Mag (ConstV [])) in false
   with
   | Wrong _ -> true)

(* Test 4: Magnitude applied to a boolean should raise Wrong exception *)
let%test "type_of_mag_bool" =
  (try 
     let _ = type_of (Mag T) in false
   with
   | Wrong _ -> true)


(*----- 
  ANGLE OPERATION TYPE CHECKING 
  -------*)

(* Test 1: Angle between two vectors; expected type Scalar *)
let%test "type_of_angle_vectors" =
  (type_of (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) = Scalar)

(* Test 2: Angle between vectors with different dimensions should raise Wrong exception *)
let%test "type_of_angle_different_dimensions" =
  (try 
     let _ = type_of (Angle (ConstV [1.0], ConstV [1.0; 0.0])) in false
   with
   | Wrong _ -> true)

(* Test 3: Angle between empty vectors should raise Wrong exception *)
let%test "type_of_angle_empty_vectors" =
  (try 
     let _ = type_of (Angle (ConstV [], ConstV [])) in false
   with
   | Wrong _ -> true)


(*----- 
  ISZERO OPERATION TYPE CHECKING 
  -------*)

(* Test 1: IsZero on a boolean value; expected type Bool *)
let%test "type_of_iszero_bool" =
  (type_of (IsZero F) = Bool)

(* Test 2: IsZero on a scalar zero; expected type Bool *)
let%test "type_of_iszero_scalar" =
  (type_of (IsZero (ConstS 0.0)) = Bool)

(* Test 3: IsZero on a zero vector; expected type Bool *)
let%test "type_of_iszero_vector" =
  (type_of (IsZero (ConstV [0.0; 0.0])) = Bool)

(* Test 4: IsZero on an empty vector should raise Wrong exception *)
let%test "type_of_iszero_empty_vector" =
  (try 
     let _ = type_of (IsZero (ConstV [])) in false
   with
   | Wrong _ -> true)


(*----- 
  COND OPERATION TYPE CHECKING 
  -------*)

(* Test 1: Conditional operation with boolean outputs; expected type Bool *)
let%test "type_of_cond_bool" =
  (type_of (Cond (T, T, F)) = Bool)

(* Test 2: Conditional operation with scalar outputs; expected type Scalar *)
let%test "type_of_cond_scalar" =
  (type_of (Cond (T, ConstS 1.0, ConstS 2.0)) = Scalar)

(* Test 3: Conditional operation with vector outputs; expected type (Vector 2) *)
let%test "type_of_cond_vector" =
  (type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* Test 4: Conditional operation with a non-boolean guard should raise Wrong exception *)
let%test "type_of_cond_non_bool_guard" =
  (try 
     let _ = type_of (Cond (ConstS 1.0, T, F)) in false
   with
   | Wrong _ -> true)

(* Test 5: Conditional operation with different types in branches should raise Wrong exception *)
let%test "type_of_cond_different_types" =
  (try 
     let _ = type_of (Cond (T, ConstS 1.0, T)) in false
   with
   | Wrong _ -> true)

(* 
--------------- 
EVAL TESTING 
--------------- 
*)

(*----- 
  BASIC EVALUATION 
  -------*)

(* Test 1: Evaluate T should yield B true *)
let%test "eval_T" =
  (eval T = B true)

(* Test 2: Evaluate F should yield B false *)
let%test "eval_F" =
  (eval F = B false)

(* Test 3: Evaluate a positive scalar should yield S 3.14 *)
let%test "eval_scalar_positive" =
  (eval (ConstS 3.14) = S 3.14)

(* Test 4: Evaluate a zero scalar should yield S 0.0 *)
let%test "eval_scalar_zero" =
  (eval (ConstS 0.0) = S 0.0)

(* Test 5: Evaluate a very large scalar should yield S 1e30 *)
let%test "eval_scalar_very_large" =
  (eval (ConstS 1e30) = S 1e30)

(* Test 6: Evaluate a very small scalar should yield S 1e-30 *)
let%test "eval_scalar_very_small" =
  (eval (ConstS 1e-30) = S 1e-30)

(* Test 7: Evaluate a single element vector should yield V [1.0] *)
let%test "eval_vector_single" =
  (eval (ConstV [1.0]) = V [1.0])

(* Test 8: Evaluate a multiple element vector should yield V [1.0; 2.0; 3.0] *)
let%test "eval_vector_multiple" =
  (eval (ConstV [1.0; 2.0; 3.0]) = V [1.0; 2.0; 3.0])

(* Test 9: Evaluate a negative vector should yield V [-1.0; -2.0] *)
let%test "eval_vector_negative" =
  (eval (ConstV [-1.0; -2.0]) = V [-1.0; -2.0])

(* Test 10: Evaluate a zero vector should yield V [0.0; 0.0] *)
let%test "eval_vector_zeros" =
  (eval (ConstV [0.0; 0.0]) = V [0.0; 0.0])

(* Test 11: Evaluating an empty vector should raise Wrong exception *)
let%test "eval_empty_vector" =
  (try let _ = eval (ConstV []) in false
   with Wrong _ -> true)

(* Test 12: Evaluate a large dimension vector of 1000 elements all 1.0 *)
let%test "eval_vector_large_dimension" =
  let n = 1000 in
  let large_vec = Vectors.create n 1.0 in
  (eval (ConstV large_vec) = V large_vec)


(*----- 
  ADD OPERATION EVALUATION 
  -------*)

(* Test 1: Boolean addition (disjunction) should yield B true *)
let%test "eval_add_booleans" =
  (eval (Add (T, F)) = B true)

(* Test 2: Scalar addition of 2.0 and 3.0 should yield S 5.0 *)
let%test "eval_add_scalars" =
  (match eval (Add (ConstS 2.0, ConstS 3.0)) with
   | S x -> x = 5.0
   | _ -> false)

(* Test 3: Adding two empty vectors should raise Wrong exception *)
let%test "eval_add_empty_vectors" =
  (try let _ = eval (Add (ConstV [], ConstV [])) in false
   with Wrong _ -> true)

(* Test 4: Adding an empty vector and a non-empty vector (empty first) should raise Wrong exception *)
let%test "eval_add_empty_and_nonempty_vectors" =
  (try let _ = eval (Add (ConstV [], ConstV [1.0; 2.0])) in false
   with Wrong _ -> true)

(* Test 5: Adding vectors with mismatched dimensions should raise Wrong exception *)
let%test "eval_add_mismatched_vector_dimensions" =
  (try let _ = eval (Add (ConstV [1.0; 2.0], ConstV [1.0])) in false
   with Wrong _ -> true)


(*----- 
  INV OPERATION EVALUATION 
  -------*)

(* Test 1: Inversion of Bool T should yield B false *)
let%test "eval_inv_bool" =
  (eval (Inv T) = B false)

(* Test 2: Additive inversion of scalar 3.0 should yield S (-3.0) *)
let%test "eval_inv_scalar" =
  (match eval (Inv (ConstS 3.0)) with
   | S x -> x = -.3.0
   | _ -> false)

(* Test 3: Inversion of vector [1.0; -2.0] should yield V [-1.0; 2.0] *)
let%test "eval_inv_vector" =
  (eval (Inv (ConstV [1.0; -2.0])) = V [-1.0; 2.0])

(* Test 4: Double inversion of scalar 3.0 should yield S 3.0 *)
let%test "eval_inv_double_scalar" =
  (eval (Inv (Inv (ConstS 3.0))) = S 3.0)

(* Test 5: Inversion of an empty vector should raise Wrong exception *)
let%test "eval_inv_empty_vector" =
  (try let _ = eval (Inv (ConstV [])) in false
   with Wrong _ -> true)


(*----- 
  SCALPROD OPERATION EVALUATION 
  -------*)

(* Test 1: Scalar product of two Booleans (T and F) should yield B false *)
let%test "eval_scalprod_booleans" =
  (eval (ScalProd (T, F)) = B false)

(* Test 2: Scalar product of two Scalars 2.0 and 3.0 should yield S 6.0 *)
let%test "eval_scalprod_scalars" =
  (match eval (ScalProd (ConstS 2.0, ConstS 3.0)) with
   | S x -> x = 6.0
   | _ -> false)

(* Test 3: Scalar product of scalar 2.0 and vector [1.0; 2.0] should yield V [2.0; 4.0] *)
let%test "eval_scalprod_scalar_vector" =
  (eval (ScalProd (ConstS 2.0, ConstV [1.0; 2.0])) = V [2.0; 4.0])

(* Test 4: Scalar product of vector [1.0; 2.0] and scalar 2.0 should yield V [2.0; 4.0] *)
let%test "eval_scalprod_vector_scalar" =
  (eval (ScalProd (ConstV [1.0; 2.0], ConstS 2.0)) = V [2.0; 4.0])

(* Test 5: Scalar product of a scalar and an empty vector should raise Wrong exception *)
let%test "eval_scalprod_empty_vector" =
  (try let _ = eval (ScalProd (ConstS 2.0, ConstV [])) in false
   with Wrong _ -> true)


(*----- 
  DOTPROD OPERATION EVALUATION 
  -------*)

(* Test 1: Dot product of vectors [1.0; 2.0] and [3.0; 4.0] should yield S 11.0 *)
let%test "eval_dotprod_vectors" =
  (match eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])) with
   | S x -> x = 11.0
   | _ -> false)

(* Test 2: Dot product of vectors with different dimensions should raise Wrong exception *)
let%test "eval_dotprod_different_dimensions" =
  (try let _ = eval (DotProd (ConstV [1.0], ConstV [1.0; 2.0])) in false
   with Wrong _ -> true)

(* Test 3: Dot product of empty vectors should raise Wrong exception *)
let%test "eval_dotprod_empty_vectors" =
  (try let _ = eval (DotProd (ConstV [], ConstV [])) in false
   with Wrong _ -> true)

(* Test 4: Dot product with non-vector types should raise Wrong exception *)
let%test "eval_dotprod_non_vector" =
  (try let _ = eval (DotProd (T, T)) in false
   with Wrong _ -> true)

(* Test 5: Dot product of two orthogonal vectors [1.0; 0.0] and [0.0; 1.0] should yield S 0.0 *)
let%test "eval_dotprod_orthogonal_vectors" =
  (match eval (DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) with
   | S x -> x = 0.0
   | _ -> false)


(*----- 
  MAG OPERATION EVALUATION 
  -------*)

(* Test 1: Magnitude of scalar -3.0 should yield S 3.0 *)
let%test "eval_mag_scalar" =
  (match eval (Mag (ConstS (-.3.0))) with
   | S x -> x = 3.0
   | _ -> false)

(* Test 2: Magnitude of vector [3.0; 4.0] should yield S 5.0 *)
let%test "eval_mag_vector" =
  (match eval (Mag (ConstV [3.0; 4.0])) with
   | S x -> x = 5.0
   | _ -> false)

(* Test 3: Magnitude of an empty vector should raise Wrong exception *)
let%test "eval_mag_empty_vector" =
  (try let _ = eval (Mag (ConstV [])) in false
   with Wrong _ -> true)

(* Test 4: Magnitude of zero vector [0.0; 0.0] should yield S value nearly 0.0 *)
let%test "eval_mag_zero_vector" =
  (match eval (Mag (ConstV [0.0; 0.0])) with
   | S x -> abs_float x < epsilon
   | _ -> false)

(* Test 5: Magnitude of a boolean should raise Wrong exception *)
let%test "eval_mag_bool" =
  (try let _ = eval (Mag T) in false
   with Wrong _ -> true)


(*----- 
  ANGLE OPERATION EVALUATION 
  -------*)

(* Test 1: Angle between orthogonal vectors [1.0; 0.0] and [0.0; 1.0] should yield S (pi/2) *)
let%test "eval_angle_orthogonal" =
  (match eval (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) with
   | S x -> abs_float (x -. (Float.pi /. 2.0)) < epsilon
   | _ -> false)

(* Test 2: Angle between parallel vectors [2.0; 0.0] and [4.0; 0.0] should yield S 0.0 *)
let%test "eval_angle_parallel" =
  (match eval (Angle (ConstV [2.0; 0.0], ConstV [4.0; 0.0])) with
   | S x -> abs_float x < epsilon
   | _ -> false)

(* Test 3: Angle between empty vectors should raise Wrong exception *)
let%test "eval_angle_empty_vectors" =
  (try let _ = eval (Angle (ConstV [], ConstV [])) in false
   with Wrong _ -> true)

(* Test 4: Angle with zero vector (e.g., [0.0; 0.0] and [1.0; 0.0]) should raise Wrong exception *)
let%test "eval_angle_zero_vector" =
  (try let _ = eval (Angle (ConstV [0.0; 0.0], ConstV [1.0; 0.0])) in false
   with Wrong _ -> true)

(* Test 5: Angle between vectors with different dimensions should raise Wrong exception *)
let%test "eval_angle_different_dimensions" =
  (try let _ = eval (Angle (ConstV [1.0], ConstV [1.0; 0.0])) in false
   with Wrong _ -> true)


(*----- 
  ISZERO OPERATION EVALUATION 
  -------*)

(* Test 1: IsZero of boolean F should yield B true *)
let%test "eval_iszero_bool_f" =
  (eval (IsZero F) = B true)

(* Test 2: IsZero of a scalar near zero (epsilon/2) should yield B true *)
let%test "eval_iszero_scalar_near_zero" =
  (match eval (IsZero (ConstS (epsilon /. 2.0))) with
   | B x -> x = true
   | _ -> false)

(* Test 3: IsZero of zero vector [0.0; 0.0] should yield B true *)
let%test "eval_iszero_zero_vector" =
  (match eval (IsZero (ConstV [0.0; 0.0])) with
   | B x -> x = true
   | _ -> false)

(* Test 4: IsZero of an empty vector should raise Wrong exception *)
let%test "eval_iszero_empty_vector" =
  (try let _ = eval (IsZero (ConstV [])) in false
   with Wrong _ -> true)

(* Test 5: IsZero of non-zero scalar 1.0 should yield B false *)
let%test "eval_iszero_nonzero" =
  (match eval (IsZero (ConstS 1.0)) with
   | B x -> x = false
   | _ -> false)


(*----- 
  COND OPERATION EVALUATION 
  -------*)

(* Test 1: Condition with true guard and scalars should yield S 1.0 *)
let%test "eval_cond_true_scalar" =
  (match eval (Cond (T, ConstS 1.0, ConstS 2.0)) with
   | S x -> x = 1.0
   | _ -> false)

(* Test 2: Condition with false guard and vectors should yield V [3.0; 4.0] *)
let%test "eval_cond_false_vector" =
  (eval (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [3.0; 4.0])

(* Test 3: Condition with non-boolean guard should raise Wrong exception *)
let%test "eval_cond_non_bool_guard" =
  (try let _ = eval (Cond (ConstS 1.0, T, F)) in false
   with Wrong _ -> true)

(* Test 4: Condition with mismatched branch types should raise Wrong exception *)
let%test "eval_cond_type_mismatch" =
  (try let _ = eval (Cond (T, ConstS 1.0, T)) in false
   with Wrong _ -> true)

(* Test 5: Nested condition: outer true uses inner condition yielding S 2.0, overall should yield S 2.0 *)
let%test "eval_cond_nested" =
  (match eval (Cond (T, 
                      Cond (F, ConstS 1.0, ConstS 2.0),
                      ConstS 3.0)) with
   | S x -> x = 2.0
   | _ -> false)