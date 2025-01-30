(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)


(*
  Abstract interpreter by induction on the syntax.
  Parameterized by an abstract domain.
*)


open Abstract_syntax_tree
open Abstract_syntax_printer
open Domain


(* parameters *)
(* ********** *)


(* for debugging *)
let trace = ref false


let unroll = ref 0 
let delay = ref 0 

(* utilities *)
(* ********* *)


(* print errors *)
let error ext s =
  Format.printf "%s: ERROR: %s@\n" (string_of_extent ext) s

let fatal_error ext s =
  Format.printf "%s: FATAL ERROR: %s@\n" (string_of_extent ext) s;
  exit 1



(* interpreter signature *)
(* ********************* *)


(* an interpreter only exports a single function, which does all the work *)
module type INTERPRETER =
sig
  (* analysis of a program, given its abstract syntax tree *)
  val eval_prog: prog -> unit
end



(* interpreter *)
(* *********** *)


(* the interpreter is parameterized by the choice of a domain D
   of signature Domain.DOMAIN
 *)

module Interprete(D : DOMAIN) =
(struct

  (* abstract element representing a set of environments;
     given by the abstract domain
   *)
  type t = D.t


  (* utility function to reduce the compexity of testing boolean expressions;
     it handles the boolean operators &&, ||, ! internally, by induction
     on the syntax, and call the domain's function D.compare, to handle
     the arithmetic part

     if r=true, keep the states that may satisfy the expression;
     if r=false, keep the states that may falsify the expression
   *)
  let filter (a:t) (e:bool_expr ext) (r:bool) : t =

    (* recursive exploration of the expression *)
    let rec doit a (e,_) r = match e with

    (* boolean part, handled recursively *)
    | AST_bool_unary (AST_NOT, e) ->
        doit a e (not r)
    | AST_bool_binary (AST_AND, e1, e2) ->
        (if r then D.meet else D.join) (doit a e1 r) (doit a e2 r)
    | AST_bool_binary (AST_OR, e1, e2) ->
        (if r then D.join else D.meet) (doit a e1 r) (doit a e2 r)
    | AST_bool_const b ->
        if b = r then a else D.bottom ()

    (* arithmetic comparison part, handled by D *)
    | AST_compare (cmp, (e1,_), (e2,_)) ->
        (* utility function to negate the comparison, when r=false *)
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


  (* interprets a statement, by induction on the syntax *)
  let rec eval_stat (a:t) ((s,ext):stat ext) : t =
    let r = match s with

    | AST_block (decl,inst) ->
        (* add the local variables *)
        let a =
          List.fold_left
            (fun a ((_,v),_) -> D.add_var a v)
            a decl
        in
        (* interpret the block recursively *)
        let a = List.fold_left eval_stat a inst in
        (* destroy the local variables *)
        List.fold_left
          (fun a ((_,v),_) -> D.del_var a v)
          a decl

    | AST_assign ((i,_),(e,_)) ->
        (* assigment is delegated to the domain *)
        D.assign a i e

    | AST_if (e,s1,Some s2) ->
        (* compute both branches *)
        let t = eval_stat (filter a e true ) s1 in
        let f = eval_stat (filter a e false) s2 in
        (* then join *)
        D.join t f

    | AST_if (e,s1,None) ->
        (* compute both branches *)
        let t = eval_stat (filter a e true ) s1 in
        let f = filter a e false in
        (* then join *)
        D.join t f

    | AST_while (e,s) ->
      let rec do_unroll (remain:int) (a:t) : t = 
        if remain = 0 then
          let rec fix (f:t -> t) (x:t) curr_delay : t =
            let fx = f x in
            if D.subset fx x then fx
            else if curr_delay > 0 then 
              let new_delay = curr_delay - 1 in
              fix f fx new_delay
            else fix f (D.widen x fx) curr_delay
          in
          let f x = D.join a (eval_stat (filter x e true) s) in
          let inv = fix f a !delay in
          filter inv e false
        else
          let body = eval_stat (filter a e true) s in 
          let after = do_unroll (remain-1) body in
          D.join after (filter a e false)
      in
      do_unroll !unroll a

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
        (* after halt, there are no more environments *)
        D.bottom ()

    in

    (* tracing, useful for debugging *)
    if !trace then
      Format.printf "stat trace: %s: %a@\n"
        (string_of_extent ext) D.print_all r;
    r


  (* entry-point of the program analysis *)
  let eval_prog (l:prog) : unit =
    (* simply analyze each statement in the program *)
    let _ = List.fold_left eval_stat (D.init()) l in
    (* nothing useful to return *)
    Format.printf "analysis ended@\n";
    ()


end : INTERPRETER)