(* secd.ml - Implementation of SECD machine for call-by-name lambda calculus *)

(* Boolean type *)
type myBool = True | False

(* Extended lambda calculus expressions *)
type expr = 
  | Var of string              (* Variable x *)
  | Abs of string * expr       (* Lambda abstraction λx.e *)
  | App of expr * expr         (* Application (e1 e2) *)
  | Num of int                 (* Integer constant *)
  | Bl of myBool               (* Boolean constant *)
  | Plus of expr * expr        (* Addition e1 + e2 *)
  | Times of expr * expr       (* Multiplication e1 * e2 *)
  | And of expr * expr         (* Logical AND e1 && e2 *)
  | Or of expr * expr          (* Logical OR e1 || e2 *)
  | Not of expr                (* Logical NOT !e *)
  | Eq of expr * expr          (* Equality test e1 == e2 *)
  | Gt of expr * expr          (* Greater than e1 > e2 *)
  | IfTE of expr * expr * expr (* If-then-else: if e1 then e2 else e3 *)
  | Let of string * expr * expr (* Let binding: let x = e1 in e2 *)

let rec string_of_expr = function
  | Var x -> x
  | Abs (x, e) -> "λ" ^ x ^ "." ^ string_of_expr e
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | Num n -> string_of_int n
  | Bl True -> "true"
  | Bl False -> "false"
  | Plus (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Times (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | And (e1, e2) -> "(" ^ string_of_expr e1 ^ " && " ^ string_of_expr e2 ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_expr e1 ^ " || " ^ string_of_expr e2 ^ ")"
  | Not e -> "!" ^ string_of_expr e
  | Eq (e1, e2) -> "(" ^ string_of_expr e1 ^ " == " ^ string_of_expr e2 ^ ")"
  | Gt (e1, e2) -> "(" ^ string_of_expr e1 ^ " > " ^ string_of_expr e2 ^ ")"
  | IfTE (e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

(* Instruction set for the SECD machine *)
type instr =
  | VAR of string
  | CONST of int
  | BOOL of myBool
  | ABS of string * instr list * expr    (* modified: add original expression *)
  | PLUS
  | TIMES
  | AND
  | OR
  | NOT
  | EQ
  | GT
  | IFTE of instr list * instr list
  | LET of string * instr list
  | APP

(* Compile an expression to a list of instructions *)
let rec compile e =
  match e with
  | Var x -> [VAR x]
  | Num n -> [CONST n]
  | Bl b -> [BOOL b]
  | Abs (x, body) -> [ABS (x, compile body, Abs(x, body))]    (* modified *)
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
  
(* Value type for the SECD machine *)
type value =
  | IntVal of int
  | BoolVal of myBool
  | Closure of string * instr list * env * expr    (* modified: add original expr *)
and env = (string * value) list

let rec string_of_value = function
  | IntVal n -> string_of_int n
  | BoolVal b -> if b = True then "true" else "false"
  | Closure (_, _, _, orig) -> string_of_expr orig    (* modified *)

(* Add substitution function *)
let rec substitute expr var replacement =
  match expr with
  | Var x -> if x = var then replacement else Var x
  | Abs (x, body) -> 
      if x = var then Abs(x, body) 
      else Abs(x, substitute body var replacement)
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
      
(* Modify unload_value to perform environment substitution *)
let rec unload_value = function
  | IntVal n -> Num n
  | BoolVal b -> Bl b
  | Closure (_, _, env, orig) ->
      List.fold_left (fun acc (x, v) -> substitute acc x (unload_value v)) orig env
      
(* The SECD machine state: (Stack, Environment, Control, Dump) *)
type state = value list * env * instr list * (value list * env * instr list) list

let debug_mode = ref true
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
      | VAR x ->
          (try
             let v = List.assoc x e in
             secd_machine (v :: s, e, c, d)
           with Not_found ->
             raise (Secd_Error ("Unbound variable: " ^ x)))
      | CONST n ->
          secd_machine (IntVal n :: s, e, c, d)
      | BOOL b ->
          secd_machine (BoolVal b :: s, e, c, d)
      | ABS (x, code, orig) ->                    (* modified pattern *)
          secd_machine (Closure (x, code, e, orig) :: s, e, c, d)
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
               secd_machine (BoolVal (if b1 = True && b2 = True then True else False) :: s', e, c, d)
           | _ -> raise (Secd_Error "AND expects two booleans"))
      | OR ->
          (match s with
           | BoolVal b2 :: BoolVal b1 :: s' ->
               secd_machine (BoolVal (if b1 = True || b2 = True then True else False) :: s', e, c, d)
           | _ -> raise (Secd_Error "OR expects two booleans"))
      | NOT ->
          (match s with
           | BoolVal b :: s' ->
               secd_machine (BoolVal (if b = True then False else True) :: s', e, c, d)
           | _ -> raise (Secd_Error "NOT expects one boolean"))
      | EQ ->
          (match s with
           | IntVal n2 :: IntVal n1 :: s' ->
               secd_machine (BoolVal (if n1 = n2 then True else False) :: s', e, c, d)
           | _ -> raise (Secd_Error "EQ expects two integers"))
      | GT ->
          (match s with
           | IntVal n2 :: IntVal n1 :: s' ->
               secd_machine (BoolVal (if n1 > n2 then True else False) :: s', e, c, d)
           | _ -> raise (Secd_Error "GT expects two integers"))
      | IFTE (ct, cf) ->
          (match s with
           | BoolVal b :: s' ->
               if b = True then
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
           | arg :: Closure (x, code, env_cl, orig) :: s' ->   (* modified *)
               secd_machine ([], (x, arg)::env_cl, code, (s', e, c)::d)
           | _ -> raise (Secd_Error "APP expects a closure and an argument on stack"))
      end

(* Evaluate an expression using the SECD machine *)
let secd expr =
  if !debug_mode then (
    step_count := 0;
    Printf.printf "\n════════════════════════════════════════\n";
    Printf.printf "SECD EVALUATING: %s\n" (string_of_expr expr);
    Printf.printf "════════════════════════════════════════\n";
  );
  let instructions = compile expr in
  let result = secd_machine ([], [], instructions, []) in
  let unloaded = unload_value result in
  if !debug_mode then (
    Printf.printf "\n════════════════════════════════════════\n";
    Printf.printf "SECD FINAL RESULT: %s\n" (string_of_value result);
    Printf.printf "SECD UNLOADED RESULT: %s\n" (string_of_expr unloaded);
    Printf.printf "════════════════════════════════════════\n\n";
  );
  result

(* Run tests with optional debug mode toggle *)
let run_test ?(debug=true) name expr =
  Printf.printf "\n===== TEST: %s =====\n" name;
  Printf.printf "Expression: %s\n" (string_of_expr expr);
  
  let old_debug = !debug_mode in
  debug_mode := debug;
  
  let _ = secd expr in
  debug_mode := old_debug;
  Printf.printf "\n";;

(* Global test definitions *)
let id_exp = Abs ("x", Var "x")
let test_expr1 = App (id_exp, id_exp)

(* Test 2: (λx. (λy. (x + y))) (λx. x)
   Use Plus instead of applying the variable "+" *)
let func_exp = Abs ("x", Abs ("y", Plus (Var "x", Var "y")))
let test_expr2 = App (func_exp, id_exp)

let const_exp = Abs ("x", Abs ("y", Var "x"))
let test_expr3 = App (App (const_exp, id_exp), id_exp)

let compose_exp = Abs ("f", Abs ("g", Abs ("x", App (Var "f", App (Var "g", Var "x")))))
let test_expr4 = App (App (App (compose_exp, id_exp), id_exp), id_exp)

let () = 
  run_test "SECD Test 2" test_expr4
