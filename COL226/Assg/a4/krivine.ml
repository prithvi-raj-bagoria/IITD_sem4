(* krivine.ml -  Krivine machine for call-by-name lambda calculus *)

(* Lambda calculus expressions *)
type variable = string
type lamexp = 
  | V of variable              (* Variable x *)
  | Lam of variable * lamexp   (* Lambda Lamtraction λx.e *)
  | App of lamexp * lamexp     (* Application (e1 e2) *)
  | Num of int                 (* Integer constant *)
  | Bl of bool               (* Boolean constant *)
  | Plus of lamexp * lamexp    (* Addition e1 + e2 *)
  | Times of lamexp * lamexp   (* Multiplication e1 * e2 *)
  | And of lamexp * lamexp     (* Logical AND e1 && e2 *)
  | Or of lamexp * lamexp      (* Logical OR e1 || e2 *)
  | Not of lamexp              (* Logical NOT !e *)
  | Eq of lamexp * lamexp      (* Equality test e1 == e2 *)
  | Gt of lamexp * lamexp      (* Greater than e1 > e2 *)
  | IfTE of lamexp * lamexp * lamexp (* If-then-else if e1 then e2 else e3 *)
  | Let of variable * lamexp * lamexp (* Let expression: let x = e1 in e2 *)

(* String representation of expressions for debugging *)
let rec string_of_lamexp = function
  | V x -> x
  | Lam (x, e) -> "λ" ^ x ^ "." ^ string_of_lamexp e
  | App (e1, e2) -> "(" ^ string_of_lamexp e1 ^ " " ^ string_of_lamexp e2 ^ ")"
  | Num n -> string_of_int n
  | Bl true -> "true"
  | Bl false -> "false"
  | Plus (e1, e2) -> "(" ^ string_of_lamexp e1 ^ " + " ^ string_of_lamexp e2 ^ ")"
  | Times (e1, e2) -> "(" ^ string_of_lamexp e1 ^ " * " ^ string_of_lamexp e2 ^ ")"
  | And (e1, e2) -> "(" ^ string_of_lamexp e1 ^ " && " ^ string_of_lamexp e2 ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_lamexp e1 ^ " || " ^ string_of_lamexp e2 ^ ")"
  | Not e -> "!" ^ string_of_lamexp e
  | Eq (e1, e2) -> "(" ^ string_of_lamexp e1 ^ " == " ^ string_of_lamexp e2 ^ ")"
  | Gt (e1, e2) -> "(" ^ string_of_lamexp e1 ^ " > " ^ string_of_lamexp e2 ^ ")"
  | IfTE (e1, e2, e3) -> "if " ^ string_of_lamexp e1 ^ " then " ^ 
                         string_of_lamexp e2 ^ " else " ^ string_of_lamexp e3
  | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_lamexp e1 ^ " in " ^ string_of_lamexp e2

(* Mutually recursive closure and environment types *)
type closure = Closure of lamexp * gamma
and gamma = (variable * closure) list

(* Debug mode - set to true for verbose output *)
let debug_mode = ref true
let step_count = ref 0

(* String representation of a closure - concise version *)
let rec string_of_closure (Closure(e, gamma)) =
  let env_vars = if List.length gamma > 0 then 
                   "{" ^ String.concat "," (List.map (fun (x, _) -> x) gamma) ^ "}"
                 else "{}" in
  Printf.sprintf "%s %s" (string_of_lamexp e) env_vars

(* String representation of a stack - shortened *)
let string_of_stack stack =
  match stack with
  | [] -> "∅"  (* Empty set symbol for empty stack *)
  | [single] -> string_of_closure single
  | _ -> Printf.sprintf "%d items" (List.length stack)

(* Reset step counter *)
let reset_step_counter () = step_count := 0

(* Helper functions for working with values *)
let get_num_value = function
  | Closure(Num n, _) -> n
  | closure -> failwith (Printf.sprintf "Expected numeric value, got: %s" (string_of_closure closure))

let get_bool_value = function
  | Closure(Bl b, _) -> b
  | closure -> failwith (Printf.sprintf "Expected boolean value, got: %s" (string_of_closure closure))

(* Helper functions for binary operations *)
let eval_binary_num_op op cl1 cl2 =
  let n1 = get_num_value cl1 in
  let n2 = get_num_value cl2 in
  Closure(Num (op n1 n2), [])

let eval_binary_bool_op op cl1 cl2 =
  let b1 = get_bool_value cl1 in
  let b2 = get_bool_value cl2 in
  Closure(Bl (op b1 b2), [])

let eval_comparison_op op cl1 cl2 =
  let n1 = get_num_value cl1 in
  let n2 = get_num_value cl2 in
  Closure(Bl (if op n1 n2 then true else false), [])

(* Krivine machine evaluation function with improved debug output and extended operations *)
let rec krivine (focus_closure, stack) =
  if !debug_mode then begin
    incr step_count;
    Printf.printf "\n[Step %d] ─────────────────────────────\n" !step_count;
    Printf.printf "Focus: %s\n" (string_of_closure focus_closure);
    Printf.printf "Stack: %s\n" (string_of_stack stack);
  end;
  
  match focus_closure with
  | Closure(V x, gamma) ->
      if !debug_mode then Printf.printf "⟹ VAR: Looking up '%s'\n" x;
      (match List.assoc_opt x gamma with
       | Some cl -> 
           if !debug_mode then Printf.printf "   ✓ Found binding for '%s'\n" x;
           krivine (cl, stack)
       | None -> 
           if !debug_mode then Printf.printf "   ✗ '%s' is free (evaluation stops)\n" x;
           (focus_closure, stack))
  | Closure(App(e1, e2), gamma) ->
      if !debug_mode then begin
        Printf.printf "⟹ APP: Pushing argument and focusing on function\n";
        Printf.printf "   • Push: %s\n" (string_of_lamexp e2);
        Printf.printf "   • Focus: %s\n" (string_of_lamexp e1);
      end;
      let arg_closure = Closure(e2, gamma) in
      krivine (Closure(e1, gamma), arg_closure :: stack)
  
  | Closure(Lam(x, body), gamma) ->
      (match stack with
      | arg_cl :: rest_stack ->
          if !debug_mode then begin
            Printf.printf "⟹ APPLY: Function application\n";
            Printf.printf "   • Bind: '%s' to argument\n" x;
            Printf.printf "   • Body: %s\n" (string_of_lamexp body);
          end;
          let new_gamma = (x, arg_cl) :: gamma in
          krivine (Closure(body, new_gamma), rest_stack)
      | [] -> 
          if !debug_mode then 
            Printf.printf "⟹ HALT: Final value reached (λ with empty stack)\n";
          (focus_closure, stack))
          
  | Closure(Num n, _) ->
      (* Constants evaluate to themselves *)
      if !debug_mode then Printf.printf "⟹ CONST: Numeric value %d\n" n;
      (focus_closure, stack)
      
  | Closure(Bl b, _) ->
      (* Boolean constants evaluate to themselves *)
      if !debug_mode then Printf.printf "⟹ CONST: Boolean value %s\n" (if b = true then "true" else "false");
      (focus_closure, stack)
  
  | Closure(Plus(e1, e2), gamma) ->
      if !debug_mode then Printf.printf "⟹ PLUS: Evaluating addition\n";
      let e1_cl = Closure(e1, gamma) in
      let e2_cl = Closure(e2, gamma) in
      let (v1, _) = krivine (e1_cl, []) in
      let (v2, _) = krivine (e2_cl, []) in
      let result = eval_binary_num_op (+) v1 v2 in
      krivine (result, stack)
      
  | Closure(Times(e1, e2), gamma) ->
      if !debug_mode then Printf.printf "⟹ TIMES: Evaluating multiplication\n";
      let e1_cl = Closure(e1, gamma) in
      let e2_cl = Closure(e2, gamma) in
      let (v1, _) = krivine (e1_cl, []) in
      let (v2, _) = krivine (e2_cl, []) in
      let result = eval_binary_num_op ( * ) v1 v2 in
      krivine (result, stack)
      
  | Closure(And(e1, e2), gamma) ->
      if !debug_mode then Printf.printf "⟹ AND: Evaluating logical AND\n";
      let e1_cl = Closure(e1, gamma) in
      let (v1, _) = krivine (e1_cl, []) in
      let b1 = get_bool_value v1 in
      if b1 = false then
        (* Short-circuit evaluation *)
        krivine (Closure(Bl false, []), stack)
      else
        let e2_cl = Closure(e2, gamma) in
        let (v2, _) = krivine (e2_cl, []) in
        krivine (v2, stack)
  
  | Closure(Or(e1, e2), gamma) ->
      if !debug_mode then Printf.printf "⟹ OR: Evaluating logical OR\n";
      let e1_cl = Closure(e1, gamma) in
      let (v1, _) = krivine (e1_cl, []) in
      let b1 = get_bool_value v1 in
      if b1 = true then
        (* Short-circuit evaluation *)
        krivine (Closure(Bl true, []), stack)
      else
        let e2_cl = Closure(e2, gamma) in
        let (v2, _) = krivine (e2_cl, []) in
        krivine (v2, stack)
  
  | Closure(Not(e), gamma) ->
      if !debug_mode then Printf.printf "⟹ NOT: Evaluating logical NOT\n";
      let e_cl = Closure(e, gamma) in
      let (v, _) = krivine (e_cl, []) in
      let b = get_bool_value v in
      let result = Closure(Bl (if b = true then false else true), []) in
      krivine (result, stack)
  
  | Closure(Eq(e1, e2), gamma) ->
      if !debug_mode then Printf.printf "⟹ EQ: Evaluating equality\n";
      let e1_cl = Closure(e1, gamma) in
      let e2_cl = Closure(e2, gamma) in
      let (v1, _) = krivine (e1_cl, []) in
      let (v2, _) = krivine (e2_cl, []) in
      let result = eval_comparison_op (=) v1 v2 in
      krivine (result, stack)
  
  | Closure(Gt(e1, e2), gamma) ->
      if !debug_mode then Printf.printf "⟹ GT: Evaluating greater than\n";
      let e1_cl = Closure(e1, gamma) in
      let e2_cl = Closure(e2, gamma) in
      let (v1, _) = krivine (e1_cl, []) in
      let (v2, _) = krivine (e2_cl, []) in
      let result = eval_comparison_op (>) v1 v2 in
      krivine (result, stack)
  
  | Closure(IfTE(e1, e2, e3), gamma) ->
      if !debug_mode then Printf.printf "⟹ IF: Evaluating conditional\n";
      let e1_cl = Closure(e1, gamma) in
      let (v1, _) = krivine (e1_cl, []) in
      let b = get_bool_value v1 in
      if b = true then
        let e2_cl = Closure(e2, gamma) in
        krivine (e2_cl, stack)
      else
        let e3_cl = Closure(e3, gamma) in
        krivine (e3_cl, stack)

  | Closure(Let(x, e1, e2), gamma) ->
      if !debug_mode then begin
        Printf.printf "⟹ LET: Binding '%s' to expression\n" x;
        Printf.printf "   • Expr: %s\n" (string_of_lamexp e1);
        Printf.printf "   • Body: %s\n" (string_of_lamexp e2);
      end;
      (* In call-by-name Krivine machine, we don't evaluate e1, just bind it as a closure *)
      let e1_closure = Closure(e1, gamma) in
      let new_gamma = (x, e1_closure) :: gamma in
      krivine (Closure(e2, new_gamma), stack)

(* Unload function to convert closure to lambda term with normalization *)
let rec unload (Closure(e, gamma)) =
  let result = 
    match e with
    | V x ->
        (match List.assoc_opt x gamma with
         | Some cl -> unload cl (* Closure associated to var x in gamma *)
         | None -> V x)       (* Free variable remains as is *)
    
    | Lam(x, body) ->
        (* To avoid variable capture, remove x from env when unloading body *)
        let gamma' = List.remove_assoc x gamma in
        Lam(x, unload (Closure(body, gamma')))
    
    | App(e1, e2) ->
        App(unload (Closure(e1, gamma)), unload (Closure(e2, gamma)))
        
    | Num n -> Num n
    | Bl b -> Bl b
    | Plus(e1, e2) -> Plus(unload(Closure(e1, gamma)), unload(Closure(e2, gamma)))
    | Times(e1, e2) -> Times(unload(Closure(e1, gamma)), unload(Closure(e2, gamma)))
    | And(e1, e2) -> And(unload(Closure(e1, gamma)), unload(Closure(e2, gamma)))
    | Or(e1, e2) -> Or(unload(Closure(e1, gamma)), unload(Closure(e2, gamma)))
    | Not(e) -> Not(unload(Closure(e, gamma)))
    | Eq(e1, e2) -> Eq(unload(Closure(e1, gamma)), unload(Closure(e2, gamma)))
    | Gt(e1, e2) -> Gt(unload(Closure(e1, gamma)), unload(Closure(e2, gamma)))
    | IfTE(e1, e2, e3) -> 
        IfTE(unload(Closure(e1, gamma)), 
             unload(Closure(e2, gamma)), 
             unload(Closure(e3, gamma)))
    | Let(x, e1, e2) -> 
        Let(x, unload(Closure(e1, gamma)), unload(Closure(e2, (x, Closure(e1, gamma)) :: gamma)))
  in
  normalize_church result
  
(* Normalize Church numerals and expressions *)
and normalize_church expr =
  match expr with
  | Lam(f, Lam(x, body)) ->
      let count = count_church_apps f x body in
      if count >= 0 then
        let rec build n = if n = 0 then V x else App(V f, build (n-1)) in
        Lam(f, Lam(x, build count))
      else
        Lam(f, Lam(x, normalize_church body))
      
  | App(e1, e2) -> 
      let n1 = normalize_church e1 in
      let n2 = normalize_church e2 in
      (match n1 with
      | Lam(x, body) -> normalize_church (substitute body x n2)
      | _ -> App(n1, n2))
  
  | Plus(e1, e2) -> Plus(normalize_church e1, normalize_church e2)
  | Times(e1, e2) -> Times(normalize_church e1, normalize_church e2)
  | And(e1, e2) -> And(normalize_church e1, normalize_church e2)
  | Or(e1, e2) -> Or(normalize_church e1, normalize_church e2)
  | Not(e) -> Not(normalize_church e)
  | Eq(e1, e2) -> Eq(normalize_church e1, normalize_church e2)
  | Gt(e1, e2) -> Gt(normalize_church e1, normalize_church e2)
  | IfTE(e1, e2, e3) -> IfTE(normalize_church e1, normalize_church e2, normalize_church e3)
  | Let(x, e1, e2) -> Let(x, normalize_church e1, normalize_church e2)
  | _ -> expr

(* Helper: Count applications of f to x in body *)
and count_church_apps f x expr =
  match expr with
  | V var when var = x -> 0
  | App(V var, rest) when var = f -> 
      let n = count_church_apps f x rest in
      if n >= 0 then n + 1 else -1
  | _ -> -1  (* Not a Church numeral *)

(* Substitute helper for beta reduction *)
and substitute expr var replacement =
  match expr with
  | V x -> if x = var then replacement else V x
  | Lam (x, body) -> 
      if x = var then Lam(x, body) 
      else Lam(x, substitute body var replacement)
  | App(e1, e2) -> App(substitute e1 var replacement, substitute e2 var replacement)
  | Num n -> Num n
  | Bl b -> Bl b
  | Plus(e1,e2) -> Plus(substitute e1 var replacement, substitute e2 var replacement)
  | Times(e1,e2) -> Times(substitute e1 var replacement, substitute e2 var replacement)
  | And(e1,e2) -> And(substitute e1 var replacement, substitute e2 var replacement)
  | Or(e1,e2) -> Or(substitute e1 var replacement, substitute e2 var replacement)
  | Not e -> Not(substitute e var replacement)
  | Eq(e1,e2) -> Eq(substitute e1 var replacement, substitute e2 var replacement)
  | Gt(e1,e2) -> Gt(substitute e1 var replacement, substitute e2 var replacement)
  | IfTE(e1,e2,e3) -> IfTE(substitute e1 var replacement, substitute e2 var replacement, substitute e3 var replacement)
  | Let(x,e1,e2) ->
      if x = var then Let(x, substitute e1 var replacement, e2)
      else Let(x, substitute e1 var replacement, substitute e2 var replacement)

(* Evaluate a lambda term using call-by-name semantics *)
let cbn expr =
  if !debug_mode then begin
    reset_step_counter();
    Printf.printf "\n════════════════════════════════════════\n";
    Printf.printf "EVALUATING: %s\n" (string_of_lamexp expr);
    Printf.printf "════════════════════════════════════════\n";
  end;
  let initial_state = (Closure(expr, []), []) in
  let (final_closure, _) = krivine initial_state in
  let result = unload(final_closure) in
  result

(* Run tests with optional debug mode toggle *)
let run_test ?(debug=false) name expr =
  Printf.printf "\n===== TEST: %s =====\n" name;
  Printf.printf "Expression: %s\n" (string_of_lamexp expr);
  
  let result = cbn expr in
  Printf.printf "Result: %s\n" (string_of_lamexp result);
  
  Printf.printf "\n";;

(*---------TESTS------------*)

(*-LEXICAL SCOPING-*)
(* Test 1: Variable Shadowing *)

(* "let x = 10 in let f = λy.λx.(x + y) in ((f 5) 20)" *)
let shadow_test =
  Let("x", Num(10),
    Let("f", Lam("y", Lam("x", Plus(V "x", V "y"))),
      App(App(V "f", Num(5)), Num(20))))

(* Test 2: Nested Function with Captured Variable *)
(*"let x = 10 in let f = λy.λz.((x + y) + z) in ((f 5) 3)" *)
let closure_test =
  Let("x", Num(10),
    Let("f", Lam("y", Lam("z", Plus(Plus(V "x", V "y"), V "z"))),
      App(App(V "f", Num(5)), Num(3))))

(* Test 3: Higher-Order Function with Lexical Scoping *)
(* "let x = 10 in let apply = λf.λa.(f a) in let g = λy.(x + y) in ((apply g) 5)" *)
let hof_test =
  Let("x", Num(10),
    Let("apply", Lam("f", Lam("a", App(V "f", V "a"))),
      Let("g", Lam("y", Plus(V "x", V "y")),
        App(App(V "apply", V "g"), Num(5)))))

(*-CBN v/s CBV-*)

(* Test 1: Divergent Computation in Unused Argument 
   In call-by-name: Should return 5
   In call-by-value: Should diverge (not terminate)
*)
(*"let omega = ((λx.(x x)) (λx.(x x))) in let constfn = λx.λy.x in ((constfn 5) omega)" *)
let divergent_test =
  Let("omega", App(Lam("x", App(V "x", V "x")), Lam("x", App(V "x", V "x"))),
    Let("constfn", Lam("x", Lam("y", V "x")),
      App(App(V "constfn", Num(5)), V "omega")))

(* Test 2: Multiple Evaluation of Arguments
   In call-by-name: The argument is evaluated on each use (twice)
   In call-by-value: The argument is evaluated once
*)
(*"let double = λf.((f 0) + (f 0)) in (double λ_.(1 + 1))" *)
let multiple_eval_test =
  Let("double", Lam("f", Plus(App(V "f", Num(0)), App(V "f", Num(0)))),
    App(V "double", Lam("_", Plus(Num(1), Num(1)))))

(* Test 3: Conditional with Potentially Divergent Computation
   In call-by-name: Only evaluates the then branch
   In call-by-value: evaluate one branches
*)
(*"if true then 5 else ((λx.(x x)) (λx.(x x)))" *)
let cond_divergent_test =
  IfTE(
    Bl(true),
    Num(5),
    App(Lam("x", App(V "x", V "x")), Lam("x", App(V "x", V "x")))
  )

(*--CHURCH ENCODINGS--*)
(* Church numerals *)
let church_zero = Lam("f", Lam("x", V "x")) (* "λf.λx.x" *)

let church_succ = Lam("n", Lam("f", Lam("x", App(V "f", App(App(V "n", V "f"), V "x"))))) (*"λn.λf.λx.(f ((n f) x))" *)

let church_one = App(church_succ, church_zero) (*"((λn.λf.λx.(f ((n f) x))) λf.λx.x)" *)

let church_two = App(church_succ, church_one) (*"((λn.λf.λx.(f ((n f) x))) ((λn.λf.λx.(f ((n f) x))) λf.λx.x))" *)

let church_three = App(church_succ, church_two)

(* Church addition *)
let church_plus =
  Lam("m", Lam("n", Lam("f", Lam("x", 
    App(App(V "m", V "f"), App(App(V "n", V "f"), V "x")))))) (*"λm.λn.λf.λx.((m f) ((n f) x))" *)

(* Church multiplication *)
let church_mult =
  Lam("m", Lam("n", Lam("f", 
    App(V "m", App(V "n", V "f"))))) (*"λm.λn.λf.(m (n f))" *)

(* Church boolean values *)
let church_true = Lam("t", Lam("f", V "t")) (*"λt.λf.t" *)

let church_false = Lam("t", Lam("f", V "f")) (*"λt.λf.f" *)

(* If-then-else using Church booleans *)
let church_if = Lam("p", Lam("a", Lam("b", App(App(V "p", V "a"), V "b")))) (*"λp.λa.λb.((p a) b)" *)

(* Is-zero predicate *)
let church_is_zero = Lam("n", App(App(V "n", Lam("_", church_false)), church_true)) (*"λn.((n λ_.λt.λf.f) λt.λf.t)" *)

(* Test 1: Addition of Church numerals (1+2) *)
let church_add_test = 
  App(App(church_plus, church_one), church_two)

(* Test 2: Multiplication of Church numerals (2*3) *)
let church_mult_test = 
  App(App(church_mult, church_two), church_three)

(* Test 3: Is-zero predicate test *)
let church_is_zero_test =
  App(App(App(church_if, App(church_is_zero, church_zero)), church_one), church_two)

let () = 
  run_test "" church_is_zero_test;
