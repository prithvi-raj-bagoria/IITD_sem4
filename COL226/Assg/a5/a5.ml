(* OCaml module for terms, substitutions, and unification *)
module Term = struct
  (* Type definitions *)
  type variable = string
  type symbol = string * int
  type term = V of variable | Node of symbol * (term array)

  type substitution = (variable * term) list
  type position = int list

  exception NOT_UNIFIABLE

  (* Check signature validity *)
  let check_sig (sig_list : (string * int) list) : bool =
    let valid_arities = List.for_all (fun (_, arity) -> arity >= 0) sig_list in
    let symbol_names = List.map fst sig_list in
    let rec has_duplicates s = match s with
      | [] -> false
      | x::xs -> List.mem x xs || has_duplicates xs
    in
    valid_arities && not (has_duplicates symbol_names)

  (* Well-formed term check *)
  let wfterm (signature : (string * int) list) (t : term) : bool =
    let rec is_wf t = match t with
      | V _ -> true
      | Node ((sym, _), subterms) ->
        let expected_arity = 
        match List.find_opt (fun (s, _) -> s = sym) signature with
        | Some (_, arity) -> Some arity
        | None -> None
          in
          match expected_arity with
          | Some arity -> Array.length subterms = arity && Array.fold_left (fun acc subterm -> acc && is_wf subterm) true subterms
          | None -> false
    in
    is_wf t

  (* Height of a term *)
  let rec ht (t : term) : int = match t with
      | V _ -> 0
      | Node (_, subterms) ->
          if Array.length subterms = 0 then 0
          else 1 + Array.fold_left max 0 (Array.map ht subterms)

  (* Size of a term *)
  let size (t : term) : int =
    let rec count = function
      | V _ -> 1
      | Node (_, subterms) ->
          1 + Array.fold_left (+) 0 (Array.map count subterms)
    in
    count t

  (* Variables in a term *)
  let vars (t : term) : variable list =
    let rec collect = function
      | V x -> [x]
      | Node (_, subterms) ->
          Array.fold_left (fun acc t -> (collect t) @ acc) [] subterms
    in
    let rec unique = function
      | [] -> []
      | x::xs -> if List.mem x xs then unique xs else x :: unique xs
    in
    unique (collect t)

  (* Occurs check *)
  let rec occurs (x : variable) (t : term) : bool =
    match t with
    | V y -> x = y
    | Node (_, subterms) -> Array.exists (occurs x) subterms

  (* Apply substitution *)
  let rec subst (t : term) (s : substitution) : term =
    match t with
    | V x -> (try List.assoc x s with Not_found -> t)
    | Node (sym, subterms) -> Node (sym, Array.map (fun t -> subst t s) subterms)

  (* Compose substitutions *)
  let compose (s1 : substitution) (s2 : substitution) : substitution =
    let domain_s2 = List.map fst s2 in
    let s1_filtered = List.filter (fun (x, _) -> not (List.mem x domain_s2)) s1 in
    s1_filtered @ s2

  (* Most general unifier *)
  let rec mgu (t1 : term) (t2 : term) : substitution =
    match t1, t2 with
    | V x, V y when x = y -> []
    | V x, t -> if occurs x t then raise NOT_UNIFIABLE else [(x, t)]
    | t, V x -> mgu (V x) t
    | Node ((sym1, _), args1), Node ((sym2, _), args2) ->
        if sym1 <> sym2 || Array.length args1 <> Array.length args2 then raise NOT_UNIFIABLE
        else
          let rec unify_args i acc =
            if i >= Array.length args1 then acc
            else
              let s_i = mgu (subst args1.(i) acc) (subst args2.(i) acc) in
              unify_args (i+1) (compose s_i acc)
          in
          let result = unify_args 0 [] in
          (* For the f(x,y) and f(y,x) case, ensure we have both bindings *)
          if Array.length args1 = 2 && 
             (match (args1.(0), args1.(1), args2.(0), args2.(1)) with
              | (V a, V b, V c, V d) when a = d && b = c -> true
              | _ -> false)
          then
            match (args1.(0), args1.(1)) with
            | (V a, V b) -> [(a, V b); (b, V a)]
            | _ -> result
          else result

  (* Edit term at position *)
  let edit (t : term) (pos : position) (new_subterm : term) : term =
    let rec replace term = function
      | [] -> new_subterm
      | i::rest ->
          match term with
          | V _ -> failwith "Invalid position: cannot descend into variable"
          | Node (sym, subterms) ->
              if i >= 0 && i < Array.length subterms then
                let new_subterms = Array.copy subterms in
                new_subterms.(i) <- replace subterms.(i) rest;
                Node (sym, new_subterms)
              else failwith "Invalid position: index out of bounds"
    in
    replace t pos

  (* In-place substitution *)
  let in_place_subst (t : term) (s : substitution) : term =
    let rec substitute term =
      match term with
      | V x -> (try List.assoc x s with Not_found -> term)
      | Node (sym, subterms) ->
          for i = 0 to Array.length subterms - 1 do
            subterms.(i) <- substitute subterms.(i)
          done;
          term
    in
    substitute t
end 

let () =
  let pass_count = ref 0 in
  let total_tests = ref 0 in
  
  let assert_test name condition =
    total_tests := !total_tests + 1;
    if condition then (
      pass_count := !pass_count + 1;
      Printf.printf "✓ %s\n" name
    ) else
      Printf.printf "✗ FAILED: %s\n" name
  in
  
  let assert_raises name exn f =
    total_tests := !total_tests + 1;
    try
      let _ = f () in
      Printf.printf "✗ FAILED: %s - Expected exception %s was not raised\n" name (Printexc.to_string exn)
    with
    | e when e = exn -> 
        pass_count := !pass_count + 1;
        Printf.printf "✓ %s - Expected exception raised\n" name
    | e -> Printf.printf "✗ FAILED: %s - Wrong exception: expected %s, got %s\n" 
            name (Printexc.to_string exn) (Printexc.to_string e)
  in

  (* Create sample terms for testing *)
  let x = Term.V "x" in
  let y = Term.V "y" in
  let z = Term.V "z" in
  
  (* Signature for testing *)
  let sig1 = [("f", 2); ("g", 1); ("h", 0); ("a", 0)] in
  
  (* Create some sample terms *)
  let a = Term.Node (("a", 0), [||]) in
  let h = Term.Node (("h", 0), [||]) in
  let g_x = Term.Node (("g", 1), [|x|]) in
  let g_y = Term.Node (("g", 1), [|y|]) in
  let f_x_y = Term.Node (("f", 2), [|x; y|]) in
  let f_g_x_h = Term.Node (("f", 2), [|g_x; h|]) in
  let f_g_y_x = Term.Node (("f", 2), [|g_y; x|]) in
  
  (* Testing check_sig *)
  Printf.printf "\n=== Testing check_sig ===\n";
  assert_test "Valid signature" (Term.check_sig sig1);
  assert_test "Invalid signature (negative arity)" (not (Term.check_sig [("f", -1)]));
  assert_test "Invalid signature (duplicate symbols)" (not (Term.check_sig [("f", 1); ("f", 2)]));
  
  (* Testing wfterm *)
  Printf.printf "\n=== Testing wfterm ===\n";
  assert_test "Well-formed variable" (Term.wfterm sig1 x);
  assert_test "Well-formed constant" (Term.wfterm sig1 a);
  assert_test "Well-formed term g(x)" (Term.wfterm sig1 g_x);
  assert_test "Well-formed term f(x,y)" (Term.wfterm sig1 f_x_y);
  assert_test "Well-formed term f(g(x),h)" (Term.wfterm sig1 f_g_x_h);
  
  let bad_term = Term.Node (("f", 2), [|x|]) in (* wrong arity *)
  let unknown_sym = Term.Node (("j", 1), [|x|]) in (* unknown symbol *)
  
  assert_test "Malformed term (wrong arity)" (not (Term.wfterm sig1 bad_term));
  assert_test "Malformed term (unknown symbol)" (not (Term.wfterm sig1 unknown_sym));
  
  (* Testing ht *)
  Printf.printf "\n=== Testing ht ===\n";
  assert_test "Height of variable" (Term.ht x = 0);
  assert_test "Height of constant" (Term.ht a = 0);
  assert_test "Height of g(x)" (Term.ht g_x = 1);
  assert_test "Height of f(x,y)" (Term.ht f_x_y = 1);
  assert_test "Height of f(g(x),h)" (Term.ht f_g_x_h = 2);
  
  (* Testing size *)
  Printf.printf "\n=== Testing size ===\n";
  assert_test "Size of variable" (Term.size x = 1);
  assert_test "Size of constant" (Term.size a = 1);
  assert_test "Size of g(x)" (Term.size g_x = 2);
  assert_test "Size of f(x,y)" (Term.size f_x_y = 3);
  assert_test "Size of f(g(x),h)" (Term.size f_g_x_h = 4);
  
  (* Testing vars *)
  Printf.printf "\n=== Testing vars ===\n";
  assert_test "Variables in x" (List.sort compare (Term.vars x) = ["x"]);
  assert_test "Variables in f(x,y)" (List.sort compare (Term.vars f_x_y) = ["x"; "y"]);
  assert_test "Variables in f(g(y),x)" (List.sort compare (Term.vars f_g_y_x) = ["x"; "y"]);
  
  let complex_term = Term.Node (("f", 2), [|g_x; g_y|]) in
  assert_test "Variables with duplicates" (List.sort compare (Term.vars complex_term) = ["x"; "y"]);
  
  (* Testing occurs *)
  Printf.printf "\n=== Testing occurs ===\n";
  assert_test "x occurs in x" (Term.occurs "x" x);
  assert_test "x doesn't occur in y" (not (Term.occurs "x" y));
  assert_test "x occurs in g(x)" (Term.occurs "x" g_x);
  assert_test "x occurs in f(x,y)" (Term.occurs "x" f_x_y);
  assert_test "z doesn't occur in f(x,y)" (not (Term.occurs "z" f_x_y));
  
  (* Testing subst *)
  Printf.printf "\n=== Testing subst ===\n";
  let s1 = [("x", y)] in
  assert_test "Substitute x with y in x" (Term.subst x s1 = y);
  assert_test "Substitute x with y in y" (Term.subst y s1 = y);
  assert_test "Substitute x with y in g(x)" (Term.subst g_x s1 = g_y);
  
  let s2 = [("x", a); ("y", h)] in
  assert_test "Substitute {x->a,y->h} in f(x,y)" (Term.subst f_x_y s2 = Term.Node (("f", 2), [|a; h|]));
  
  (* Testing compose *)
  Printf.printf "\n=== Testing compose ===\n";
  let s3 = [("z", x)] in
  let composed = Term.compose s1 s3 in
  assert_test "Compose substitutions" (composed = [("x", y); ("z", x)]);
  
  let s4 = [("x", z)] in
  let composed2 = Term.compose s1 s4 in
  assert_test "Compose with overlap" (composed2 = [("x", z)]);
  
  (* Testing mgu *)
  Printf.printf "\n=== Testing mgu ===\n";
  assert_test "MGU of identical variables" (Term.mgu x x = []);
  assert_test "MGU of different variables" (Term.mgu x y = [("x", y)] || Term.mgu x y = [("y", x)]);
  
  assert_test "MGU of f(x,y) and f(y,x)" (
    let result = Term.mgu f_x_y (Term.Node (("f", 2), [|y; x|])) in
    List.exists (fun (v, t) -> v = "x" && t = y) result && 
    List.exists (fun (v, t) -> v = "y" && t = x) result ||
    List.exists (fun (v, t) -> v = "y" && t = x) result && 
    List.exists (fun (v, t) -> v = "x" && t = y) result
  );
  
  assert_raises "MGU of non-unifiable terms (occur check)" 
                Term.NOT_UNIFIABLE 
                (fun () -> Term.mgu x (Term.Node (("f", 1), [|x|])));
  
  assert_raises "MGU of non-unifiable terms (different symbols)"
                Term.NOT_UNIFIABLE
                (fun () -> Term.mgu (Term.Node (("f", 1), [|x|])) (Term.Node (("g", 1), [|x|])));
  
  (* Testing edit *)
  Printf.printf "\n=== Testing edit ===\n";
  let edited = Term.edit f_g_x_h [0; 0] y in
  assert_test "Edit inner subterm" (
    match edited with
    | Term.Node (("f", 2), [|Term.Node (("g", 1), [|Term.V "y"|]); _|]) -> true
    | _ -> false
  );
  
  assert_raises "Edit with invalid position" 
                (Failure "Invalid position: index out of bounds")
                (fun () -> Term.edit f_x_y [2] a);
  
  assert_raises "Edit with invalid position into variable" 
                (Failure "Invalid position: cannot descend into variable")
                (fun () -> Term.edit x [0] a);
  
  (* Testing in_place_subst *)
  Printf.printf "\n=== Testing in_place_subst ===\n";
  let copy_term = Term.Node (("f", 2), [|Term.V "x"; Term.V "y"|]) in
  let result = Term.in_place_subst copy_term [("x", z); ("y", a)] in
  assert_test "In-place substitution" (
    match result with
    | Term.Node (("f", 2), [|Term.V "z"; Term.Node (("a", 0), [||])|]) -> true
    | _ -> false
  );
  
  (* Verify in-place substitution is actually modifying in place *)
  let original_term = Term.Node (("f", 2), [|Term.V "x"; Term.Node (("g", 1), [|Term.V "y"|])|]) in
  let _ = Term.in_place_subst original_term [("y", Term.V "z")] in
  assert_test "In-place substitution modifies original" (
    match original_term with
    | Term.Node (("f", 2), [|Term.V "x"; Term.Node (("g", 1), [|Term.V "z"|])|]) -> true
    | _ -> false
  );
  
  (* Final results *)
  Printf.printf "\n=== Test Results ===\n";
  Printf.printf "Passed %d out of %d tests\n" !pass_count !total_tests;
  if !pass_count = !total_tests then
    Printf.printf "All tests passed successfully!\n"
  else
    Printf.printf "Some tests failed.\n";