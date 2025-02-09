open Abstract_syntax_tree
open Abstract_syntax_printer
open Domain

let trace = ref false

let unroll = ref 0 
let delay = ref 0
let wide_delay = ref 0
let iteration_count = ref 0

let error ext s =
  Format.printf "%s: ERROR: %s@\n" (string_of_extent ext) s

let fatal_error ext s =
  Format.printf "%s: FATAL ERROR: %s@\n" (string_of_extent ext) s;
  exit 1

module type INTERPRETER =
sig
  val eval_prog: prog -> unit
end

module Interprete(D : DOMAIN) =
(struct

  type t = D.t

  (* Filtre l'état en fonction d'une expression booléenne et d'un booléen.
     Contribue à l'abstraction logique en exploitant la sémantique du domaine D. *)
  let filter (a:t) (e:bool_expr ext) (r:bool) : t =

    let rec doit a (e,_) r = match e with

    | AST_bool_unary (AST_NOT, e) ->
        doit a e (not r)
    | AST_bool_binary (AST_AND, e1, e2) ->
        (if r then D.meet else D.join) (doit a e1 r) (doit a e2 r)
    | AST_bool_binary (AST_OR, e1, e2) ->
        (if r then D.join else D.meet) (doit a e1 r) (doit a e2 r)
    | AST_bool_const b ->
        if b = r then a else D.bottom ()

    | AST_compare (cmp, (e1,_), (e2,_)) ->
        let inv = function
        | AST_EQUAL         -> AST_NOT_EQUAL
        | AST_NOT_EQUAL     -> AST_EQUAL
        | AST_LESS          -> AST_GREATER_EQUAL
        | AST_LESS_EQUAL    -> AST_GREATER
        | AST_GREATER       -> AST_LESS_EQUAL
        | AST_GREATER_EQUAL -> AST_LESS
        in
        let cmp = if r then cmp else inv cmp in
        D.compare a e1 cmp e2

    in
    doit a e r

  (* Évalue une instruction en tenant compte du domaine abstrait D 
     et construit la sémantique pas à pas (interprétation abstraite). *)
  let rec eval_stat (a:t) ((s,ext):stat ext) : t =
    let r = match s with

    | AST_block (decl,inst) ->
        let a =
          List.fold_left
            (fun a ((_,v),_) -> D.add_var a v)
            a decl
        in
        let a = List.fold_left eval_stat a inst in
        List.fold_left
          (fun a ((_,v),_) -> D.del_var a v)
          a decl

    | AST_assign ((i,_),(e,_)) ->
        D.assign a i e

    | AST_if (e,s1,Some s2) ->
        let t = eval_stat (filter a e true ) s1 in
        let f = eval_stat (filter a e false) s2 in
        D.join t f

    | AST_if (e,s1,None) ->
        let t = eval_stat (filter a e true ) s1 in
        let f = filter a e false in
        D.join t f

    (* Implémente un point fixe (fixpoint_iteration) pour la boucle.
       Combine un unroll initial et l'utilisation d'une étape de widening
       pour contrôler la convergence dans le domaine D. *)
    | AST_while (e, s) ->
      let rec fixpoint_iteration (transform: t -> t) (current: t)
        (widen_steps: int) (unroll_steps: int): t =
        if unroll_steps > 0 then
          let next = eval_stat (filter current e true) s in
          fixpoint_iteration transform next widen_steps (unroll_steps - 1)
        else if widen_steps = 0 then
          let altered = transform current in
          if D.subset altered current then altered
          else fixpoint_iteration transform (D.widen current altered) 0 0
        else
          let altered = transform current in
          if D.subset altered current then altered
          else fixpoint_iteration transform altered (widen_steps - 1) 0
      in
      let transform_func x = D.join a (eval_stat (filter x e true) s) in
      let loop_invariant = fixpoint_iteration transform_func a !wide_delay !unroll in
      filter loop_invariant e false

    | AST_assert e ->
      let res = filter a e false in
      if not (D.is_bottom res) then error ext "assertion failure";
      filter a e true       

    | AST_print l ->
        let l' = List.map fst l in
        Format.printf "%s: %a@\n"
          (string_of_extent ext) (fun fmt v -> D.print fmt a v) l';
        a

    | AST_PRINT_ALL ->
        Format.printf "%s: %a@\n"
          (string_of_extent ext) D.print_all a;
        a

    | AST_HALT ->
        D.bottom ()
    in

    if !trace then
      Format.printf "stat trace: %s: %a@\n"
        (string_of_extent ext) D.print_all r;
    r

  (* Démarre l'évaluation du programme dans l'état initial, puis affiche la fin de l'analyse. *)
  let eval_prog (l:prog) : unit =
    iteration_count := 0;
    let _ = List.fold_left eval_stat (D.init()) l in
    Format.printf "analysis ended@\n";
    ()

end : INTERPRETER)