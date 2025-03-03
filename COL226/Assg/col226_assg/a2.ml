open List;;

let epsilon:float = 1e-06;; (*Epsilon for checking isZero*)
(*-----------------
Adding Vectors module from assignment 1
------------------*)

module Vectors = struct 
(* Type definition*)
type vector = float list;;

(* Exception for Dimension and ZeroDivision Error*)
exception DimensionError;;
exception ZeroDIvisionError;;

(* create : int -> float  -> vector // (create n x) creates an n-dimensional vector containing value x, i.e. [x, ..., x]; raise  DimensionError if n < 1*)
let create (n:int) (x:float) : vector =
  if n<1 then raise DimensionError
  else 
    let rec create_tr n acc = 
      if n=0 then acc
      else create_tr (n-1) (x::acc)
    in create_tr n [];;

(* dim:  vector -> int // gives the dimension of the vector (should be >= 1)*)
let dim (v:vector) : int =
  if v=[] then raise DimensionError
  else
  let rec dim_tr (v:vector) (len:int) = match v with
    [] -> len
  | _::xs -> dim_tr xs (len+1)
  in dim_tr v 0;;

(*is_zero:  vector -> bool  //  checks that a given  vector v (of dim n) is the zero vector of dimension n*)
let is_zero (v: vector) =
  if v=[] then raise DimensionError
  else 
    let rec is_zero_tr v = match v with
      [] -> true
    | x::xs -> if (abs_float x ) < epsilon then is_zero_tr xs
              else false
    in is_zero_tr v;;
    
(* unit: int -> int -> vector // (unit n  j ) creates a unit vector of dim n, with a 1.0 in the j-th coordinate  (1 <= j <= n); raise DimensionError if (1 <= j <= n) is violated*)
let unit (n: int) (j: int) : vector =
  if not (1 <= j && j <= n) then 
    raise DimensionError
  else
    let rec unit_tr (x: int) (j: int) (acc: vector) : vector =
      if x = 0 then acc
      else if x = j then unit_tr (x - 1) j (1.0 :: acc)
      else unit_tr (x - 1) j (0.0 :: acc)
    in 
    unit_tr n j []

(*scale: float -> vector -> vector // given float c and vector v, returns the vector  c v, whose coordinates are those of v multiplied by scalar c*)
let scale (c: float) (v: vector) : vector =
  if v=[] then raise DimensionError
  else
  List.map (fun x -> c *. x) v;;

(*addv : vector -> vector -> vector //  adds given vectors v1 and v2 (these should of the same dimension n. else raise the exception DimensionError) to return  v1 + v2*)
let  addv (v1:vector) (v2:vector) : vector =
  if (dim v1) <> (dim v2) then raise DimensionError
  else 
    let rec addv_helper v1 v2  = 
    if v1=[] && v2=[] then []
    else (hd v1 +. hd v2):: addv_helper (tl v1) (tl v2)
  in addv_helper v1 v2;;
  
(*dot_prod: vector -> vector -> float // takes the dot product of two given vectors v1 and v2 (of the same dimension n, else raise the exception DimensionError)  to return  v1 . v2 *)
let dot_prod (v1:vector) (v2:vector) : float =
  if (dim v1 <> dim v2) || dim v1 <1 then raise DimensionError
  else
    let rec dot_prod_tr v1 v2 acc = 
      if v1=[] && v2=[] then  acc
      else dot_prod_tr (tl v1) (tl v2) (acc +. (hd v1 *. hd v2))
      in dot_prod_tr v1 v2 0.;;
    
(*inv: vector -> vector //  given a vector  v, returns the vector that is its vector additive inverse. *)
let  inv (v:vector) : vector = 
  if v=[] then raise DimensionError
  else
  List.map (fun x -> -.x) v;;

(*length: vector -> float // return the length of a given vector v*)
let length (v:vector) : float = sqrt (dot_prod v v);; (* I have provided the correctness of this property in three extra properties*)

(*angle: vector -> vector -> float // given vectors v1 and v2 find the (smaller) angle between them in radians. *)

let angle (v1: vector) (v2: vector) : float = 
  if is_zero v1 || is_zero v2 then raise ZeroDIvisionError
  else
    let dot = dot_prod v1 v2 in
    let len1 = length v1 in
    let len2 = length v2 in
    let cos_theta = dot /. (len1 *. len2) in
    let cos_theta_clamped =
      if cos_theta < -1. then -1.
      else if cos_theta > 1. then 1.
      else cos_theta
  in  acos cos_theta_clamped;;
end;;

open Vectors;;  (*Opened Module Vectors*)

(* Defining Types for our DSL*)
type types =  Bool    (* boolean *)
            | Scalar   (* a scalar — any float value *)
            | Vector of int   (* n-dimensional with elements of type float*)
;;
(*Defining expressions for DSL*)
type expr =  
    T | F   (* Boolean constants *)
|   ConstS of float    (* Scalar constants *)
|   ConstV of float list    (* Vector constants *)
|   Add of expr * expr   (* overloaded — disjunction of two booleans or sum of  two scalars or sum of two vectors of the same dimension *)
|   Inv of expr     (* overloaded — negation of a boolean or additive inverse of  a scalar or additive inverse of a vector *)
|   ScalProd of expr * expr   (* overloaded — conjunction of two booleans or product of a scalar with another scalar or product of a scalar and a vector *)
|   DotProd of expr * expr  (* dot product of two vectors of the same dimension *)
|   Mag of expr   (* overloaded: absolute value of a scalar or magnitude of a vector *)
|   Angle of expr * expr  (* in radians, the angle between two vectors *)
|   IsZero of expr (* overloaded: checks if a boolean expression evaluates to F,  or if a given scalar is within epsilon of 0.0 or is the vector close — within epsilon on each coordinate —  to the zero vector *)
|   Cond of expr * expr * expr  (* "if_then_else" --  if the first expr evaluates to T then evaluate the second expr, else the third expr *)
;;

(*---------------------------------
TYPE CHECKER
----------------------------------*)
exception Wrong of expr;; (*Exception for type mismatch*)

let rec type_of e = match e with 
  T -> Bool
| F -> Bool
| ConstS _ -> Scalar
| ConstV vec -> if vec=[] then raise (Wrong e) else  Vector (dim vec) (*checking vec shouldn't be empty*)

| Add(e1,e2) -> (* Bool*Bool -> Bool OR Scalar*Scalar-> Scalar OR Vector*Vector->Vector*)
  (match (type_of e1, type_of e2) with
  | (Bool,Bool) -> Bool
  | (Scalar,Scalar) -> Scalar
  | (Vector n1,Vector n2) -> if (n1=n2 && n1>0) then Vector n1 else raise (Wrong e) (*Checking for same dimension and empty vector*)
  | _ -> raise (Wrong e))

| Inv e ->  (*Bool->Bool OR Scalar->Scalar OR Vector -> Vector*)
  (match (type_of e) with
  | Bool -> Bool
  | Scalar -> Scalar
  | Vector n1 when n1>0 ->  Vector n1 (*Checking for empty vector*)
  |_ -> raise (Wrong e))

| ScalProd (e1,e2) -> (* Bool*Bool -> Bool OR Scalar*Scalar-> Scalar OR Scalar*Vector->Vector*)
  (match (type_of e1,type_of e2) with
  | (Bool,Bool) -> Bool
  | (Scalar,Scalar) -> Scalar
  | (Scalar,Vector n1) when n1>0 ->  Vector n1
  | (Vector n1 , Scalar) when n1>0 ->  Vector n1
  | _ -> raise (Wrong e))

| DotProd (e1,e2) -> (* Vector*Vector->Vector*)
  (match (type_of e1,type_of e2) with
  | (Vector n1 , Vector n2) -> if(n1=n2 && n1>0) then Scalar else raise (Wrong e)
  | _ -> raise (Wrong e))

| Mag e1 -> (* Scalar -> Scalar OR Vector->Scalar*)
  (match (type_of e1) with
  | Scalar -> Scalar
  | Vector n1 when n1>0 ->  Scalar
  | _ -> raise (Wrong e))

| Angle (e1,e2) -> (*Vector*Vector -> Scalar*)
  (match(type_of e1,type_of e2) with
  | (Vector n1,Vector n2) ->  if (n1=n2 && n1>0) then Scalar else raise (Wrong e)
  | _ -> raise (Wrong e))

| IsZero e1 -> (*Bool->Bool OR Scalar->Bool OR Vector->Bool*)
  (match(type_of e1) with
  | Bool -> Bool
  | Scalar -> Bool
  | Vector n1 when n1>0 -> Bool
  | _-> raise (Wrong e))

| Cond(e1,e2,e3) -> (*bool*'a*'a -> 'a*)
  (if (type_of e1= Bool && (type_of e2) = (type_of e3)) then type_of e2 else raise (Wrong e));;

(*-----------------
DEFINITONAL INTERPRETER
-------------------*)

type values = B of bool | S of float | V of vector;; (*values which are given by interpreter*)

let rec eval e = match e with
  | T -> B true
  | F -> B false
  | ConstS x -> S x
  | ConstV v -> if v <> [] then V v else raise (Wrong e)  (* Empty vector checked *)
  | Add(e1, e2) -> let v1 = eval e1 in
                   let v2 = eval e2 in
                   (match (v1,v2) with
                   | (B b1, B b2) -> B (b1 || b2) (*Bool OR*)
                   | (S x1, S x2) -> S (x1 +. x2) (*Scalar Addition*)
                   | (V v1, V v2) when (v1<>[] && v2 <>[])  && (dim v1 = dim v2)-> V (addv v1 v2) (*Vector addition using addv in Vectors module*)
                   | _ -> raise (Wrong e))  
  | Inv(e1) -> let v1 = eval e1 in
                  (match v1 with
                  | B b -> B (not b)  (*Negation of Bool*)
                  | S x -> S (-.x)  (*Additive inverse of Scalar*)
                  | V v when v<>[] -> V (inv v)  (*Additive inverse of Vector using inv in Vectors module*)
                  | _ -> raise (Wrong e))
  | ScalProd(e1, e2) -> let v1 = eval e1 in
                        let v2 = eval e2 in
                        (match (v1,v2) with
                        | (B b1, B b2) -> B (b1 && b2) (*Bool AND*)
                        | (S x1, S x2) -> S (x1 *. x2)  (*Scalar Multiplication*)
                        | (S x, V v) when v<>[] -> V (scale x v) (*Scalar*Vector using scale in Vectors module*)
                        | (V v, S x) when v<>[] -> V (scale x v) (*Vector*Scalar using scale in Vectors module*)
                        | _ -> raise (Wrong e))
  | DotProd(e1, e2) -> let v1 = eval e1 in
                       let v2 = eval e2 in
                       (match (v1,v2) with
                       | (V v1, V v2) when (v1<>[] && v2<>[]) && (dim v1 = dim v2)-> S (dot_prod v1 v2) (* Dot product using dot_prod in Vectors module*)
                       | _ -> raise (Wrong e))
  | Mag(e1) -> let v1 = eval e1 in
                (match v1 with
                | S x -> S (abs_float x)
                | V v when (v<>[]) -> S (length v)  (* using length in Vectors module*)
                | _ -> raise (Wrong e))
  | Angle(e1, e2) -> let v1 = eval e1 in
                     let v2 = eval e2 in
                     (match (v1,v2) with
                     | (V v1, V v2) when (v1<>[] && v2<>[]) && (dim v1 = dim v2) && (is_zero v1 = false && is_zero v2 =false)-> S (angle v1 v2) (* using angle in Vectors module*)
                     | _ -> raise (Wrong e))

  | IsZero(e1) -> let v1 = eval e1 in
                  (match v1 with
                  | B b -> B (not b)
                  | S x -> B (abs_float x < epsilon)  (* using epsilon instead of 0.0 *)
                  | V v when v<>[] -> B (is_zero v) (* using is_zero from Vectors module*)
                  | _ -> raise (Wrong e))
  | Cond(e1, e2, e3) -> 
    let v1 = eval e1 in
    (match v1 with
    | B true -> if (type_of e2) = (type_of e3) then eval e2 else raise (Wrong e)  (* Checking e2 and e2 have same types*)
    | B false -> if (type_of e2) = (type_of e3) then eval e3 else raise (Wrong e)
    | _ -> raise (Wrong e));;