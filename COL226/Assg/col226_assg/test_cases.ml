open Vectors

let () =
  let pass_count = ref 0 in
  let total_tests = ref 0 in

  let assert_test condition =
    total_tests := !total_tests + 1;
    if condition then
      pass_count := !pass_count + 1
    else
      Printf.printf "Test case %d failed!\n" !total_tests
  in

  Printf.printf "Testing create...\n";
  let v1 = Vectors.create 3 1.5 in
  let v2 = Vectors.create 5 0. in
  let v3 = Vectors.create 1 (-2.3) in
  assert_test (v1 = [1.5; 1.5; 1.5]);
  assert_test (v2 = [0.; 0.; 0.; 0.; 0.]);
  assert_test (v3 = [-2.3]);

  let test_dimension_error n x =
    total_tests := !total_tests + 1;
    try
      let _ = Vectors.create n x in
      Printf.printf "FAILED: create(%d, %f) should raise DimensionError\n" n x
    with
    | Vectors.DimensionError  -> 
        pass_count := !pass_count + 1;
        Printf.printf "PASSED: create(%d, %f) raised DimensionError\n" n x
    | _ -> 
        Printf.printf "FAILED: create(%d, %f) raised the wrong exception\n" n x
  in
  test_dimension_error 0 1.0;
  test_dimension_error (-2) 3.5;

  Printf.printf "Testing dim...\n";
  assert_test (Vectors.dim v1 = 3);
  assert_test (Vectors.dim v2 = 5);
  assert_test (Vectors.dim v3 = 1);
  Printf.printf "dim passed!\n";

  Printf.printf "Testing is_zero...\n";
  assert_test (Vectors.is_zero [0.; 0.; 0.]);
  assert_test (not (Vectors.is_zero [0.; 1.; 0.]));
  assert_test (not (Vectors.is_zero [1.0]));
  Printf.printf "is_zero passed!\n";

  Printf.printf "Testing unit...\n";
  assert_test (Vectors.unit 3 2 = [0.; 1.; 0.]);
  assert_test (Vectors.unit 4 1 = [1.; 0.; 0.; 0.]);
  assert_test (Vectors.unit 5 5 = [0.; 0.; 0.; 0.; 1.]);

  Printf.printf "unit passed for valid inputs!\n";
  let test_unit_error n j =
    total_tests := !total_tests + 1;
    try
      let _ = Vectors.unit n j in
      Printf.printf "FAILED: unit(%d, %d) should raise DimensionError\n" n j
    with
    | Vectors.DimensionError  -> 
        pass_count := !pass_count + 1;
        Printf.printf "PASSED: unit(%d, %d) raised DimensionError\n" n j
    | _ -> 
        Printf.printf "FAILED: unit(%d, %d) raised the wrong exception\n" n j
  in

  test_unit_error 3 0;   (* j < 1 *)
  test_unit_error 3 4;   (* j > n *)
  test_unit_error 0 1;   (* n < 1 *)
  test_unit_error (-2) 1; (* n < 1 *)
  Printf.printf "unit passed!\n";

  Printf.printf "Testing addv...\n";
  assert_test (Vectors.addv [1.; 2.; 3.] [4.; 5.; 6.] = [5.; 7.; 9.]);
  assert_test (Vectors.addv [0.; 0.] [0.; 0.] = [0.; 0.]);
  Printf.printf "addv passed!\n";

  let test_addv_error v1 v2 =
    total_tests := !total_tests + 1;
    try
      let _ = Vectors.addv v1 v2 in
      Printf.printf "FAILED: addv %s + %s should raise DimensionError\n"
        (String.concat ", " (List.map string_of_float v1))
        (String.concat ", " (List.map string_of_float v2))
    with
    | Vectors.DimensionError  -> 
        pass_count := !pass_count + 1;
        Printf.printf "PASSED: addv with mismatched dimensions raised DimensionError\n"
    | _ -> 
        Printf.printf "FAILED: addv raised the wrong exception\n"
  in
  test_addv_error [1.; 2.] [1.; 2.; 3.];

  Printf.printf "Testing dot_prod...\n";
  assert_test (Vectors.dot_prod [1.; 2.; 3.] [4.; 5.; 6.] = 32.);
  assert_test (Vectors.dot_prod [1.; 0.; -1.] [0.; 1.; 0.] = 0.);
  Printf.printf "dot_prod passed!\n";

  (* Test dot_prod exception cases *)
  let test_dot_prod_error v1 v2 =
    total_tests := !total_tests + 1;
    try
      let _ = Vectors.dot_prod v1 v2 in
      Printf.printf  "FAILED: dot_prod %s . %s should raise DimensionError\n"
        (String.concat ", " (List.map string_of_float v1))
        (String.concat ", " (List.map string_of_float v2))
    with
    | Vectors.DimensionError  -> 
        pass_count := !pass_count + 1;
        Printf.printf  "PASSED: dot_prod with mismatched dimensions raised DimensionError\n"
    | _ -> 
        Printf.printf  "FAILED: dot_prod raised the wrong exception\n"
  in
  test_dot_prod_error [1.; 2.] [1.; 2.; 3.];

  Printf.printf "Testing inv...\n";
  assert_test (Vectors.inv [1.; -2.; 3.] = [-1.; 2.; -3.]);
  assert_test (Vectors.inv [0.; 0.; 0.] = [0.; 0.; 0.]);
  Printf.printf "inv passed!\n";

  Printf.printf "Testing length...\n";
  assert_test (Vectors.length [3.; 4.] = 5.);
  assert_test (Vectors.length [0.; 0.; 0.] = 0.);
  Printf.printf "length passed!\n";

  Printf.printf "Testing angle...\n";
  assert_test (abs_float (Vectors.angle [1.; 0.] [0.; 1.] -. (Float.pi /. 2.)) < 1e-6);
  assert_test (abs_float (Vectors.angle [1.; 1.] [1.; 1.] -. 0.) < 1e-6);
  assert_test (abs_float (Vectors.angle [1.; 0.; 0.] [0.; 1.; 0.] -. (Float.pi /. 2.)) < 1e-6);
  assert_test (abs_float (Vectors.angle [1.; 2.; 3.] [-1.; -2.; -3.] -. Float.pi) < 1e-6);
  assert_test (abs_float (Vectors.angle [1.; 2.; 2.] [2.; 1.; 2.] -. 0.4758822496) < 1e-6);
  Printf.printf "angle passed!\n";

  let test_angle_error v1 v2 =
    total_tests := !total_tests + 1;
    try
      let _ = Vectors.angle v1 v2 in
      Printf.printf "FAILED: angle %s , %s should raise DimensionError\n"
        (String.concat ", " (List.map string_of_float v1))
        (String.concat ", " (List.map string_of_float v2))
    with
    | Vectors.DimensionError  -> 
        pass_count := !pass_count + 1;
        Printf.printf  "PASSED: angle with mismatched dimensions raised DimensionError\n"
    | _ -> 
        Printf.printf "FAILED: angle raised the wrong exception\n"
  in
  test_angle_error [1.; 2.] [1.; 2.; 3.];

  Printf.printf "Testing add+dotprod...\n";

  let v1 = [1.; 2.; 3.] in
  let v2 = [4.; -1.; 0.] in

  let v3 = Vectors.addv v1 v2 in
  assert_test (v3 = [5.; 1.; 3.]);

  let dp1 = Vectors.dot_prod v3 v1 in
  assert_test (dp1 = (5. *. 1.) +. (1. *. 2.) +. (3. *. 3.));

  let dp2 = Vectors.dot_prod v1 v2 in
  let scaled_v1 = List.map (fun x -> x *. dp2) v1 in
  assert_test (scaled_v1 = [2.; 4.; 6.]);

  Printf.printf "add+dotprod tests passed!\n";
  Printf.printf "All tests passed: %d out of %d\n" !pass_count !total_tests;
