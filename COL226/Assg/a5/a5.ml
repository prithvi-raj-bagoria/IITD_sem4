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
    let rec has_duplicates = function
      | [] -> false
      | x::xs -> List.mem x xs || has_duplicates xs
    in
    valid_arities && not (has_duplicates symbol_names)

  (* Well-formed term check *)
  let wfterm (signature : (string * int) list) (t : term) : bool =
    let arity_map = Hashtbl.create (List.length signature) in
    List.iter (fun (sym, arity) -> Hashtbl.add arity_map sym arity) signature;
    let rec is_wf = function
      | V _ -> true
      | Node ((sym, _), subterms) ->
          try
            let expected_arity = Hashtbl.find arity_map sym in
            Array.length subterms = expected_arity &&
            Array.for_all is_wf subterms
          with Not_found -> false
    in
    is_wf t

  (* Height of a term *)
  let ht (t : term) : int =
    let rec height = function
      | V _ -> 0
      | Node (_, subterms) ->
          if Array.length subterms = 0 then 0
          else 1 + Array.fold_left max 0 (Array.map height subterms)
    in
    height t

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
          Array.fold_left (fun acc t -> List.rev_append (collect t) acc) [] subterms
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
    let s1' = List.map (fun (x, t) -> (x, subst t s2)) s1 in
    let domain_s1 = List.map fst s1 in
    let s2' = List.filter (fun (x, _) -> not (List.mem x domain_s1)) s2 in
    s1' @ s2'

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
          unify_args 0 []

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
