(* secd.ml - Implementation of SECD machine for call-by-value lambda calculus *)

(* Lambda calculus expressions *)
type variable = string
type lamexp = 
  | V of variable              (* Variable x *)
  | Lam of variable * lamexp   (* Lambda abstraction λx.e *)
  | App of lamexp * lamexp     (* Application (e1 e2) *)
  | Num of int                 (* Integer constant *)
  | Bl of bool                 (* Boolean constant *)
  | Plus of lamexp * lamexp    (* Addition e1 + e2 *)
  | Times of lamexp * lamexp   (* Multiplication e1 * e2 *)
  | And of lamexp * lamexp     (* Logical AND e1 && e2 *)
  | Or of lamexp * lamexp      (* Logical OR e1 || e2 *)
  | Not of lamexp              (* Logical NOT !e *)
  | Eq of lamexp * lamexp      (* Equality test e1 == e2 *)
  | Gt of lamexp * lamexp      (* Greater than e1 > e2 *)
  | IfTE of lamexp * lamexp * lamexp (* If-then-else: if e1 then e2 else e3 *)
  | Let of variable * lamexp * lamexp (* Let binding: let x = e1 in e2 *)

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
  | IfTE (e1, e2, e3) -> "if " ^ string_of_lamexp e1 ^ " then " ^ string_of_lamexp e2 ^ " else " ^ string_of_lamexp e3
  | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_lamexp e1 ^ " in " ^ string_of_lamexp e2

(* Instruction set for the SECD machine *)
type opcode =
  | LOOKUP of variable     (* Look up a variable in the environment *)
  | MkCLOS of variable * opcode list * lamexp  (* Create a closure *)
  | CONST of int           (* Push integer constant *)
  | BOOL of bool           (* Push boolean constant *)
  | PLUS                   (* Addition operation *)
  | TIMES                  (* Multiplication operation *)
  | AND                    (* Logical AND operation *)
  | OR                     (* Logical OR operation *)
  | NOT                    (* Logical NOT operation *)
  | EQ                     (* Equality test *)
  | GT                     (* Greater than test *)
  | IFTE of opcode list * opcode list  (* If-then-else *)
  | LET of variable * opcode list      (* Let binding *)
  | APP                    (* Function application *)
  | RET                    (* Return from function *)

(* Value type for the SECD machine *)
type value =
| IntVal of int
| BoolVal of bool
| Clos of variable * opcode list * gamma * lamexp

and closure = value  (* Closure is a specific kind of value *)
and gamma = (variable * value) list  (* Environment mapping variables to values *)

let rec string_of_value = function
  | IntVal n -> string_of_int n
  | BoolVal b -> if b then "true" else "false"
  | Clos (_, _, _, orig) -> string_of_lamexp orig

(* Compile an expression to a list of instructions *)
let rec compile e =
  match e with
  | V x -> [LOOKUP x]
  | Num n -> [CONST n]
  | Bl b -> [BOOL b]
  | Lam (x, body) -> [MkCLOS (x, (compile body) @ [RET], Lam(x, body))] 
  | App (e1, e2) -> (compile e1) @ (compile e2) @ [APP]
  | Plus (e1, e2) -> (compile e1) @ (compile e2) @ [PLUS]
  | Times (e1, e2) -> (compile e1) @ (compile e2) @ [TIMES]
  | And (e1, e2) -> (compile e1) @ (compile e2) @ [AND]
  | Or (e1, e2) -> (compile e1) @ (compile e2) @ [OR]
  | Not e -> (compile e) @ [NOT]
  | Eq (e1, e2) -> (compile e1) @ (compile e2) @ [EQ]
  | Gt (e1, e2) -> (compile e1) @ (compile e2) @ [GT]
  | IfTE (e1, e2, e3) -> (compile e1) @ [IFTE (compile e2, compile e3)]
  | Let (x, e1, e2) -> (compile e1) @ [LET (x, compile e2)]
  
(* Add substitution function *)
let rec substitute expr var replacement =
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
      
(* Add Church numeral normalization and related functions *)

(* Normalize Church numerals and expressions *)
let rec normalize_church expr =
  (* Beta-reduce expressions - apply functions to arguments *)
  let rec beta_reduce expr =
    match expr with
    | App(Lam(x, body), arg) -> 
        beta_reduce (substitute body x arg)
    | App(e1, e2) -> 
        let e1' = beta_reduce e1 in
        let e2' = beta_reduce e2 in
        (match e1' with
         | Lam(x, body) -> beta_reduce (App(e1', e2'))
         | _ -> App(e1', e2'))
    | Lam(x, body) -> Lam(x, beta_reduce body)
    | _ -> expr
  in

  (* Convert to canonical Church numeral form *)
let rec church_normalize expr =
  match expr with
  | Lam(f, Lam(x, body)) ->
      (* Check if this is a Church numeral form *)
      let apps = count_nested_apps f x body in
      if apps >= 0 then
        (* Construct canonical Church numeral with 'apps' applications of f *)
        let rec build n = 
          if n = 0 then V x 
          else App(V f, build (n-1)) 
        in
        Lam(f, Lam(x, build apps))
      else
        (* Not a Church numeral, normalize recursively *)
        Lam(f, Lam(x, church_normalize body))
  | App(e1, e2) -> App(church_normalize e1, church_normalize e2)
  | _ -> expr
in

(* Apply beta reduction, then Church normalization *)
let beta_reduced = beta_reduce expr in
church_normalize beta_reduced

(* Count nested applications of a function to find Church numeral value *)
and count_nested_apps f x expr =
  let rec count expr depth =
    match expr with
    | V var when var = x -> depth
    | App(V var, e) when var = f -> count e (depth + 1)
    | App(App(_, _) as fun_expr, arg) ->
        (* Try to recognize more complex application patterns *)
        let fun_val = count fun_expr 0 in
        if fun_val >= 0 then
          let arg_val = count arg 0 in
          if arg_val >= 0 then
            fun_val + arg_val + depth
          else -1
        else -1
    | _ -> -1  (* Not a Church numeral pattern *)
  in
  count expr 0

(* Modify unload_value to include Church numeral normalization *)
let rec unload_value = function
  | IntVal n -> Num n
  | BoolVal b -> Bl b
  | Clos (_, _, env, orig) ->
      let result = List.fold_left 
        (fun acc (x, v') -> substitute acc x (unload_value v')) orig env in
      normalize_church result
      
(* The SECD machine state: (Stack, Environment, Control, Dump) *)
type state = value list * gamma * opcode list * (value list * gamma * opcode list) list

let debug_mode = ref false
let step_count = ref 0

let print_state (s, e, c, d) =
  if !debug_mode then (
    incr step_count;
    Printf.printf "\n[Step %d] ─────────────────────────────\n" !step_count;
    Printf.printf "Stack: [ %s ]\n" (String.concat "; " (List.map string_of_value s));
    Printf.printf "Env: { %s }\n" (String.concat "; " (List.map (fun (x, _) -> x) e));
    Printf.printf "Control: [ %d instrs ]\n" (List.length c);
    Printf.printf "Dump: [ %d items ]\n" (List.length d)
  )

exception Secd_Error of string

let rec secd_machine state =
  print_state state;
  match state with
  | (s, e, [], []) ->
      (match s with
       | v::_ -> v
       | [] -> raise (Secd_Error "Empty stack at termination"))
  | (s, e, [], (s', e', c')::d') ->
      (* We shouldn't reach here now that we have explicit RET instructions *)
      raise (Secd_Error "Control list empty but dump not empty - missing RET instruction")
  | (s, e, instr::c, d) ->
      begin match instr with
      | LOOKUP x ->
          (try
             let v = List.assoc x e in
             secd_machine (v :: s, e, c, d)
           with Not_found ->
              raise (Secd_Error ("Variable " ^ x ^ " not found in environment")))
      | CONST n ->
          secd_machine (IntVal n :: s, e, c, d)
      | BOOL b ->
          secd_machine (BoolVal b :: s, e, c, d)
      | MkCLOS (x, code, orig) ->
          secd_machine (Clos (x, code, e, orig) :: s, e, c, d)
      | PLUS ->
          (match s with
           | IntVal n2 :: IntVal n1 :: s' ->
               secd_machine (IntVal (n1 + n2) :: s', e, c, d)
           | _ -> raise (Secd_Error "PLUS expects two integers"))
      | TIMES ->
          (match s with
           | IntVal n2 :: IntVal n1 :: s' ->
               secd_machine (IntVal (n1 * n2) :: s', e, c, d)
           | _ -> raise (Secd_Error "TIMES expects two integers"))
      | AND ->
          (match s with
           | BoolVal b2 :: BoolVal b1 :: s' ->
               secd_machine (BoolVal (b1 && b2) :: s', e, c, d)
           | _ -> raise (Secd_Error "AND expects two booleans"))
      | OR ->
          (match s with
           | BoolVal b2 :: BoolVal b1 :: s' ->
               secd_machine (BoolVal (b1 || b2) :: s', e, c, d)
           | _ -> raise (Secd_Error "OR expects two booleans"))
      | NOT ->
          (match s with
           | BoolVal b :: s' ->
               secd_machine (BoolVal (not b) :: s', e, c, d)
           | _ -> raise (Secd_Error "NOT expects one boolean"))
      | EQ ->
          (match s with
           | IntVal n2 :: IntVal n1 :: s' ->
               secd_machine (BoolVal (n1 = n2) :: s', e, c, d)
           | _ -> raise (Secd_Error "EQ expects two integers"))
      | GT ->
          (match s with
           | IntVal n2 :: IntVal n1 :: s' ->
               secd_machine (BoolVal (n1 > n2) :: s', e, c, d)
           | _ -> raise (Secd_Error "GT expects two integers"))
      | IFTE (ct, cf) ->
          (match s with
           | BoolVal b :: s' ->
               if b then
                 secd_machine (s', e, ct @ c, d)
               else
                 secd_machine (s', e, cf @ c, d)
           | _ -> raise (Secd_Error "IFTE expects a boolean on stack"))
      | LET (x, code) ->
          (match s with
           | v :: s' ->
               secd_machine (s', (x, v)::e, code @ c, d)
           | _ -> raise (Secd_Error "LET expects a value on stack"))
      | APP ->
          (match s with
           | arg :: Clos (x, code, env_cl, _) :: s' ->
               secd_machine ([], (x, arg)::env_cl, code, (s', e, c)::d)
           | _ -> raise (Secd_Error "APP expects a closure and an argument on stack"))
      | RET ->
          (match s with
           | v :: _ ->  (* Only need the top value, discard the rest of the local stack *)
               (match d with
                | (s'', e'', c'') :: d' ->
                    secd_machine (v :: s'', e'', c'', d')  (* Push result to caller's stack *)
                | _ -> raise (Secd_Error "RET expects a non-empty dump"))
           | _ -> raise (Secd_Error "RET expects a value on stack"))
      end

(* Evaluate an expression using the SECD machine *)
let secd expr =
  if !debug_mode then (
    step_count := 0;
    Printf.printf "\n════════════════════════════════════════\n";
    Printf.printf "SECD EVALUATING: %s\n" (string_of_lamexp expr);
    Printf.printf "════════════════════════════════════════\n";
  );
  let instructions = compile expr in
  let result = secd_machine ([], [], instructions, []) in
  let unloaded = unload_value result in
  (* Always print the result, regardless of debug mode *)
  Printf.printf "RESULT: %s\n" (string_of_lamexp unloaded);
  result

(* Run tests with optional debug mode toggle *)
let run_test ?(debug=false) name expr =
  Printf.printf "\n===== TEST: %s =====\n" name;
  Printf.printf "Expression: %s\n" (string_of_lamexp expr);

  let _ = secd expr in
  
  Printf.printf "\n";;

(*---------TESTS------------*)

(*-----------------
LEXICAL SCOPING
------------------*)

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

(* Test 4: Deep nesting with multiple captured variables *)
(*"let x = 1 in let y = 2 in let z = 3 in let f = λa.(((a + x) + y) + z) in (f 4)" *)
let deep_nesting_test =
  Let("x", Num(1),
    Let("y", Num(2),
      Let("z", Num(3),
        Let("f", Lam("a", Plus(Plus(Plus(V "a", V "x"), V "y"), V "z")),
          App(V "f", Num(4))))))

(* Test 5: Shadowing variables in nested scopes *)
(* "let x = 1 in let f = λx.(x + 10) in let g = λy.let x = 100 in ((f y) + x) in (g 5)" *)
let multi_shadow_test =
  Let("x", Num(1),
    Let("f", Lam("x", Plus(V "x", Num(10))),
      Let("g", Lam("y", 
            Let("x", Num(100),
              Plus(App(V "f", V "y"), V "x"))),
        App(V "g", Num(5)))))

(* Test 6: Function returning function with captured variables *)
(*"let x = 10 in let makeAdder = λy.λz.((x + y) + z) in let add15 = (makeAdder 5) in (add15 20)" *)
let curried_closure_test =
  Let("x", Num(10),
    Let("makeAdder", Lam("y", Lam("z", Plus(Plus(V "x", V "y"), V "z"))),
      Let("add15", App(V "makeAdder", Num(5)),
        App(V "add15", Num(20)))))

(*---------------
CBN v/s CBV
----------------*)

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

(* Test 4: Lazy evaluation advantage - avoiding expensive calculations *)
(*"let expensive = (999999 * 999999) in let ignore_arg = λx.42 in (ignore_arg expensive)" *)
let lazy_eval_test =
  Let("expensive", Times(Num(999999), Num(999999)), 
    Let("ignore_arg", Lam("x", Num(42)),
      App(V "ignore_arg", V "expensive")))

(* Test 5: Multiple use of potentially side-effecting expression *)
(*"let identity = λx.x in let duplicate = λx.((identity x) + (identity x)) in (duplicate (1 + 2))" *)
let multiple_use_test =
  Let("identity", Lam("x", V "x"),
    Let("duplicate", Lam("x", Plus(App(V "identity", V "x"), App(V "identity", V "x"))),
      App(V "duplicate", Plus(Num(1), Num(2)))))

(* Test 6: Thunking to simulate call-by-name in a call-by-value context *)
(*"let apply_to_zero = λf.(f 0) in let thunk = λ_.(5 + 3) in (apply_to_zero thunk)" *)
let thunking_test =
  Let("apply_to_zero", Lam("f", App(V "f", Num(0))),
    Let("thunk", Lam("_", Plus(Num(5), Num(3))),
      App(V "apply_to_zero", V "thunk")))

(*------------------
CHURCH ENCODINGS
--------------------*)
(* Church numerals *)
let church_zero = Lam("f", Lam("x", V "x")) (* "λf.λx.x" *)

let church_succ = Lam("n", Lam("f", Lam("x", App(V "f", App(App(V "n", V "f"), V "x"))))) (*"λn.λf.λx.(f ((n f) x))" *)

let church_one = App(church_succ, church_zero) (*"((λn.λf.λx.(f ((n f) x))) λf.λx.x)" *)

let church_two = App(church_succ, church_one) (*"((λn.λf.λx.(f ((n f) x))) ((λn.λf.λx.(f ((n f) x))) λf.λx.x))" *)

let church_three = App(church_succ, church_two)

(* Church addition - same implementation as Krivine *)
let church_plus =
  Lam("m", Lam("n", Lam("f", Lam("x", 
    App(App(V "m", V "f"), App(App(V "n", V "f"), V "x")))))) (*"λm.λn.λf.λx.((m f) ((n f) x))" *)

(* Church multiplication - same implementation as Krivine *)
let church_mult =
  Lam("m", Lam("n", Lam("f", 
    App(V "m", App(V "n", V "f"))))) (*"λm.λn.λf.(m (n f))" *)

(* Church boolean values *)
let church_true = Lam("t", Lam("f", V "t")) (*"λt.λf.t" *)

let church_false = Lam("t", Lam("f", V "f")) (*"λt.λf.f" *)

(* If-then-else using Church booleans *)
let church_if = Lam("p", Lam("a", Lam("b", App(App(V "p", V "a"), V "b")))) (*"λp.λa.λb.((p a) b)" *)

(* Is-zero predicate - still need the inline version for call-by-value *)
let church_is_zero = 
  Lam("n", 
    App(
      App(
        V "n", 
        Lam("_", Lam("t", Lam("f", V "f")))  (* Inline of church_false *)
      ), 
      Lam("t", Lam("f", V "t"))  (* Inline of church_true *)
    )
  ) 

(* Test 1: Addition of Church numerals (1+2) *)
let church_add_test =
  App(App(church_plus, church_one), church_two)

(* Test 2: Multiplication of Church numerals (2*3) *)
let church_mult_test = 
  App(App(church_mult, church_two), church_three)

(* Test 3: Is-zero predicate test - using Church conditionals *)
let church_is_zero_test =
  App(
    App(
      App(church_if, App(church_is_zero, church_zero)),  (* If zero is zero (should be true) *)
      church_one                                      (* Then return 1 - using Church numeral *)
    ),
    church_two                                        (* Else return 2 - using Church numeral *)
  )

(* Test: Divergent argument not used in call-by-name, but causes issues in call-by-value *)
let church_divergent_test =
  App(
    App(
      App(church_if, church_true),
      church_one
    ),
    App(Lam("x", App(V "x", V "x")), Lam("x", App(V "x", V "x"))) (* Omega - divergent term *)
  )

(*-----------
HOF
------------*)

(* Test 1: Complex nested let expressions with shadowing *)
(* "let x = 10 in let f = λx.(x + 1) in let g = λy.((f y) + x) in (g 5)" *)
let nested_let_test =
  Let("x", Num(10),
    Let("f", Lam("x", Plus(V "x", Num(1))),
      Let("g", Lam("y", Plus(App(V "f", V "y"), V "x")),
        App(V "g", Num(5)))))


(* Test 2: Combining higher-order functions with Church encoding *)
(* "let apply_twice = λf.λx.(f (f x)) in let add_one = λx.(x + 1) in ((apply_twice add_one) 10)" *)
let hof_church_test =
  Let("apply_twice", Lam("f", Lam("x", App(V "f", App(V "f", V "x")))),
    Let("add_one", Lam("x", Plus(V "x", Num(1))),
      App(App(V "apply_twice", V "add_one"), Num(10))))

let () = 
  run_test "" thunking_test;
