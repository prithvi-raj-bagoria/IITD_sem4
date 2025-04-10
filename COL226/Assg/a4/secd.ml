(* secd.ml - Implementation of SECD machine for call-by-name lambda calculus *)

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
  | Lam (x, body) -> [MkCLOS (x, compile body, Lam(x, body))]
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
      secd_machine (List.hd s :: s', e, c', d')
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
           | v :: s' ->
               (match d with
                | (s'', e'', c'') :: d' ->
                    secd_machine (v :: s'', e'', c'', d')
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
