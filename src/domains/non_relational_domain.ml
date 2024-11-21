(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)

(*
   Lifts a value domain (abstracting sets of integers)
   into a non-relational domain (abstracting sets of maps,
   from variables to integers).
 *)

open Abstract_syntax_tree
open Abstract_syntax_printer
open Value_domain
open Domain

let non_relational_debug = ref false

let debugstr = "-- DEBUG: "


(* Add debug trace to a value domain. *)
module DebugValue(V : VALUE_DOMAIN) = (struct

  type t = V.t

  let debug_int_unary_op op i o =
    if !non_relational_debug then Format.printf "%s%s %a ~> %a@." debugstr op V.print i V.print o

  let debug_int_binary_int_op op i1 i2 o =
    if !non_relational_debug then Format.printf "%s%a %s %a ~> %a@." debugstr V.print i1 op V.print i2 V.print o

  let debug_int_binary_int_int_op op i1 i2 o1 o2 =
    if !non_relational_debug then Format.printf "%s%a %s %a ~> %a, %a@." debugstr V.print i1 op V.print i2 V.print o1 V.print o2

  let top = V.top

  let bottom = V.bottom

  let print = V.print

  let unary v op =
    let rep = V.unary v op in
    let () = debug_int_unary_op (string_of_int_unary_op op) v rep in
    rep

  let binary v1 v2 op =
    let rep = V.binary v1 v2 op in
    let () = debug_int_binary_int_op (string_of_int_binary_op op) v1 v2 rep in
    rep

  let const z =
    let rep = V.const z in
    if !non_relational_debug then Format.printf "%sconst %a ~> %a@." debugstr Z.pp_print z V.print rep;
    rep

  let rand z1 z2 =
    let rep = V.rand z1 z2 in
    if !non_relational_debug then Format.printf "%srand %a %a ~> %a@." debugstr Z.pp_print z1 Z.pp_print z2 V.print rep;
      rep

  let join v1 v2 =
    let rep = V.join v1 v2 in
    let () = debug_int_binary_int_op "⊔" v1 v2 rep in
    rep

  let meet v1 v2 =
    let rep = V.meet v1 v2 in
    let () = debug_int_binary_int_op "⊓" v1 v2 rep in
    rep

  let widen v1 v2 =
    let rep = V.widen v1 v2 in
    let () = debug_int_binary_int_op "▿" v1 v2 rep in
    rep

  let subset v1 v2 =
    let rep = V.subset v1 v2 in
    if !non_relational_debug then Format.printf "%s%a ⊏ %a ~> %b@." debugstr V.print v1 V.print v2 rep ;
    rep

  let is_bottom v =
    let rep = V.is_bottom v in
    if !non_relational_debug then Format.printf "%sis_bottom %a ~> %b@." debugstr V.print v rep ;
    rep

  let compare v1 v2 cmp_op =
    let rep1, rep2 = V.compare v1 v2 cmp_op in
    let () = debug_int_binary_int_int_op (string_of_compare_op cmp_op) v1 v2 rep1 rep2 in
    rep1, rep2

  let bwd_unary v op v' =
    let rep = V.bwd_unary v op v' in
    if !non_relational_debug then
      Format.printf "%s%s %a (bwd from %a) ~> %a@."
        debugstr
        (string_of_int_unary_op op)
        V.print v
        V.print v'
        V.print rep;
    rep

  let bwd_binary v1 v2 op v' =
    let rep1, rep2 = V.bwd_binary v1 v2 op v' in
    if !non_relational_debug then
      Format.printf "%s%a %s %a (bwd from %a) ~> %a, %a@."
        debugstr
        V.print v1
        (string_of_int_binary_op op)
        V.print v2
        V.print v'
        V.print rep1
        V.print rep2;
    rep1, rep2

end : VALUE_DOMAIN)


(* The module is parameterized by a domain V abstracting sets of integers *)
module NonRelational(V : VALUE_DOMAIN) = (struct


  (* types *)
  (* ***** *)

  module V = DebugValue(V)

  (* a map, with variables (strings) as keys *)
  module VarMap = Mapext.Make(String)

  (* type of non-bottom abstract elements:
     maps each variable to an abstract value
   *)
  type env = V.t VarMap.t


  (* type of abstract elements;
     either a map from variables to (non-bottom) abstract values;
     or bottom (empty set)
   *)
  type t = Val of env | BOT


  (* propagates bottom *)
  exception Empty


  (* utilities *)
  (* ********* *)


  (* an integer expression tree, where each node is annotated
     with an abstract set of integers, in V;
     useful for assignemnt and compare
   *)
  type atree =
    | A_unary of int_unary_op * atree * V.t
    | A_binary of int_binary_op * atree * V.t * atree * V.t
    | A_var of var * V.t
    | A_cst of V.t


  (* evaluates an integer expression, by calling the abstract operator from V;
     returns an abstract value for the expression,
     but also an expression tree with each node annotated by an abstract value
   *)
  let rec eval (m:env) (e:int_expr) : atree * V.t =
    match e with

    | AST_int_unary (op,(e1,_)) ->
        let a1,v1 = eval m e1 in
        A_unary (op,a1,v1),
        V.unary v1 op

    | AST_int_binary (op,(e1,_),(e2,_)) ->
        let a1,v1 = eval m e1 in
        let a2,v2 = eval m e2 in
        A_binary (op,a1,v1,a2,v2),
        V.binary v1 v2 op

    | AST_identifier (var,_) ->
        let v = VarMap.find var m in
        A_var (var, v),
        v

    | AST_int_const (c,_) ->
        let v = V.const (Z.of_string c) in
        A_cst v,
        v

    | AST_rand ((c1,_),(c2,_)) ->
        let v = V.rand (Z.of_string c1) (Z.of_string c2) in
        A_cst v,
        v


  (* backward refinement of integer expressions;
     given an annotated tree, and a target value,
     refine the environment using the variables in the expression

     it can sometimes detect that the target value is not reachable
     (e.g., unsatisfiable comparison)
     in which case it raises Empty
   *)
  let rec refine (m:env) (a:atree) (r:V.t) : env =
    match a with

    | A_unary (op,a1,v1) ->
        (* propagate downward *)
        refine m a1 (V.bwd_unary v1 op r)

    | A_binary (op,a1,v1,a2,v2) ->
        (* propagate downward *)
        let w1,w2 = V.bwd_binary v1 v2 op r in
        refine (refine m a1 w1) a2 w2

    | A_var (var,v) ->
        (* refine the variable value *)
        let w = V.meet v r in
        if V.is_bottom w then raise Empty;
        VarMap.add var w m

    | A_cst v ->
        (* test for satisfiability *)
        if V.is_bottom (V.meet v r) then raise Empty;
        m


  (* implements the comparison
     may raise Empty
   *)
  let apply_compare (m:env) (e1:int_expr) (op:compare_op) (e2:int_expr) : env =
    (* evaluate forward each argument expression *)
    let a1,v1 = eval m e1
    and a2,v2 = eval m e2 in
    (* apply comparison *)
    let r1,r2 = V.compare v1 v2 op in
    (* propagate backward on both argument expressions *)
    refine (refine m a1 r1) a2 r2


  (* interface implementation *)
  (* ************************ *)


  (* initial environment *)
  let init () =
    Val VarMap.empty

  (* empty environment *)
  let bottom () =
    BOT

  (* add a (0-initialized) variable to the environment *)
  let add_var a var = match a with
  | BOT -> BOT
  | Val m ->
      Val (VarMap.add var (V.const Z.zero) m)

  (* remove a variable from the environment *)
  let del_var a var = match a with
  | BOT -> BOT
  | Val m ->
      Val (VarMap.remove var m)


  (* assignment *)
  let assign a var e = match a with
  | BOT -> BOT
  | Val m ->
      let _,v = eval m e in
      if V.is_bottom v then BOT
      else Val (VarMap.add var v m)


  (* compare *)
  let compare a e1 op e2 = match a with
  | BOT -> BOT
  | Val m ->
      try Val (apply_compare m e1 op e2)
      with Empty -> BOT


  (* join *)
  let join a b = match a,b with
  | BOT,x | x,BOT -> x
  | Val m, Val n ->
      Val (VarMap.map2z (fun _ x y -> V.join x y) m n)

  (* meet *)
  let meet a b = match a,b with
  | BOT,x | x,BOT -> x
  | Val m, Val n ->
      try Val
          (VarMap.map2z
             (fun _ x y ->
               let r = V.meet x y in
               if V.is_bottom r then raise Empty;
               r
             ) m n)
      with Empty -> BOT

  (* widening, similar to join *)
  let widen a b = match a,b with
  | BOT,x | x,BOT -> x
  | Val m, Val n ->
      Val (VarMap.map2z (fun _ x y -> V.widen x y) m n)


  (* check inclusion *)
  let subset a b = match a,b with
  | BOT,_ -> true
  | _,BOT -> false
  | Val m, Val n ->
      VarMap.for_all2z (fun _ x y -> V.subset x y) m n


  (* check the emptiness *)
  let is_bottom a =
    a = BOT


  (* print the abstract element on some variables *)
  let print fmt a vars =
    match a with
    | BOT -> Format.fprintf fmt "⊥"
    | Val m ->
        Format.fprintf fmt "[";
        let first = ref true in
        List.iter
          (fun var ->
            let v= VarMap.find var m in
            if !first then first := false else Format.fprintf fmt ",";
            Format.fprintf fmt " %s in %a" var V.print v
          )
          vars;
        Format.fprintf fmt " ]"

  (* print the abstract element on all variables *)
  let print_all fmt a =
    match a with
    | BOT -> Format.fprintf fmt "⊥"
    | Val m ->
        Format.fprintf fmt "[";
        let first = ref true in
        VarMap.iter
          (fun var v ->
            if !first then first := false else Format.fprintf fmt ",";
            Format.fprintf fmt " %s in %a" var V.print v
          )
          m;
        Format.fprintf fmt " ]"

end : DOMAIN)
