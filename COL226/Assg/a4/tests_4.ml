(* ---------- TEST FRAMEWORK ---------- *)

let test_case name f =
  try
    f ();
    Printf.printf "Test '%s' passed!\n" name
  with
  | Assert_failure _ ->
      Printf.printf "Test '%s' FAILED (assertion failure)!\n" name
  | e ->
      Printf.printf "Test '%s' FAILED with exception: %s\n" name (Printexc.to_string e)

(* ---------- KRIVINE MACHINE TESTS ---------- *)

open Krivine  (* Assuming your krivine.ml exposes Krivine module *)

let test_krivine_var_lookup () =
  let env = [ ("x", Closure (V "x", []))] in
  let cl = Closure (V "x", env) in
  let final_state = krivine (cl, []) in (* Call krivine with tuple *)
  let final_closure, _ = final_state in (* Extract final closure *)
  assert (unload final_closure = V "x")

let test_krivine_identity_function () =
  let id = Lam ("x", V "x") in
  let env = [ ("y", Closure (V "y", []))] in
  let cl = Closure (App (id, V "y"), env) in
  let final_state = krivine (cl, []) in (* Call krivine with tuple *)
  let final_closure, _ = final_state in (* Extract final closure *)
  assert (unload final_closure = V "y")

let test_krivine_nested_lambda () =
  let term = App (Lam ("x", Lam ("y", V "x")), V "z") in
  let env = [ ("z", Closure (V "z", []))] in
  let cl = Closure (term, env) in
  let final_state = krivine (cl, []) in (* Call krivine with tuple *)
  let final_closure, _ = final_state in (* Extract final closure *)
  match unload final_closure with
  | Lam ("y", V "z") -> ()
  | _ -> assert false

let test_krivine_application_of_functions () =
  let f = Lam ("x", App (V "x", V "x")) in
  let arg = Lam ("y", V "y") in
  let env = [] in
  let term = App (f, arg) in
  let cl = Closure (term, env) in
  let final_state = krivine (cl, []) in (* Call krivine with tuple *)
  let final_closure, _ = final_state in (* Extract final closure *)
  match unload final_closure with
  | Lam ("y", V "y") -> ()
  | _ -> assert false

let test_krivine_free_variable () =
  let term = Lam ("x", V "y") in
  let env = [ ("z", Closure (V "z", []))] in
  let cl = Closure (App (term, V "z"), env) in
  let final_state = krivine (cl, []) in (* Call krivine with tuple *)
  let final_closure, _ = final_state in (* Extract final closure *)
  match unload final_closure with
  | V "y" -> ()
  | _ -> assert false

(* ---------- SECD MACHINE TESTS ---------- *)

open Secd  (* Assuming your secd.ml exposes SECD module *)

let test_secd_var_lookup () =
  let env = [ ("x", Clos ("x", [], [], V "x")) ] in
  let s, e, c, d = ([], env, [LOOKUP "x"], []) in
  let result = secd_machine (s, e, c, d) in
  match result with
  | Clos ("x", [], _, _) -> ()
  | _ -> assert false

let test_secd_simple_function_application () =
  let lam = MkCLOS ("x", [LOOKUP "x"; RET], Lam ("x", V "x")) in
  let prog = [lam; lam; APP] in
  let result = secd_machine ([], [], prog, []) in
  match result with
  | Clos ("x", [LOOKUP "x"; RET], _, _) -> ()
  | _ -> assert false

let test_secd_nested_functions () =
  let inner = MkCLOS ("y", [LOOKUP "y"; RET], Lam ("y", V "y")) in
  let outer = MkCLOS ("x", [inner; RET], Lam ("x", Lam ("y", V "y"))) in
  let prog = [outer; outer; APP] in
  let result = secd_machine ([], [], prog, []) in
  match result with
  | Clos ("y", [LOOKUP "y"; RET], _, _) -> ()
  | _ -> assert false

let test_secd_application_ret () =
  let lam = MkCLOS ("x", [LOOKUP "x"; RET], Lam ("x", V "x")) in
  let prog = [lam; lam; APP] in
  let result = secd_machine ([], [], prog, []) in
  match result with
  | Clos ("x", [LOOKUP "x"; RET], _, _) -> ()
  | _ -> assert false

let test_secd_closure_environment () =
  let lam = MkCLOS ("x", [LOOKUP "x"; RET], Lam ("x", V "x")) in
  let env = [ ("y", Clos ("y", [], [], V "y")) ] in
  let s, e, c, d = ([], env, [lam], []) in
  let result = secd_machine (s, e, c, d) in
  match result with
  | Clos ("x", [LOOKUP "x"; RET], closure_env, _) ->
      let rec contains_y = function
        | ("y", _) :: _ -> true
        | _ :: rest    -> contains_y rest
        | []           -> false
      in
      assert (contains_y closure_env)
  | _ -> assert false

(* ---------- RUN ALL TESTS ---------- *)

let () =
  (* Krivine Tests *)
  test_case "Krivine Var Lookup" test_krivine_var_lookup;
  test_case "Krivine Identity Function" test_krivine_identity_function;
  test_case "Krivine Nested Lambda" test_krivine_nested_lambda;
  test_case "Krivine Application of Functions" test_krivine_application_of_functions;
  test_case "Krivine Free Variable" test_krivine_free_variable;

  (* SECD Tests *)
  test_case "SECD Var Lookup" test_secd_var_lookup;
  test_case "SECD Simple Function Application" test_secd_simple_function_application;
  test_case "SECD Nested Functions" test_secd_nested_functions;
  test_case "SECD Application RET" test_secd_application_ret;
  test_case "SECD Closure Environment" test_secd_closure_environment
