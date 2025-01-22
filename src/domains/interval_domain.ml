(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)

(* domaine des intervalles pour l'analyse statique *)

open Abstract_syntax_tree
open Value_domain
open Z 

(* définition des bornes d'un intervalle *)
type bound =
  | PosInf
  | NegInf
  | Finite of Z.t

(* définition des intervalles *)
type interval =
  | Range of bound * bound
  | EmptySet

module BoundOps = struct
  (* comparaison de deux bornes *)
  let compare_bounds a b = match (a, b) with
    | NegInf, NegInf | PosInf, PosInf -> 0
    | NegInf, _ | _, PosInf -> -1
    | PosInf, _ | _, NegInf -> 1
    | Finite x, Finite y -> Z.compare x y

  (* fonctions min et max pour les bornes *)
  let min_bound a b = if compare_bounds a b <= 0 then a else b
  let max_bound a b = if compare_bounds a b >= 0 then a else b

  (* inversion d'une borne *)
  let negate = function
    | PosInf -> NegInf
    | NegInf -> PosInf
    | Finite x -> Finite (Z.neg x)
end

module IntervalDomain = struct
  open BoundOps

  type t = interval

  (* création d'un intervalle *)
  let create l u = if compare_bounds l u > 0 then EmptySet else Range (l, u)

  (* affichage d'un intervalle *)
  let print fmt = function
    | EmptySet -> Format.fprintf fmt "⊥"
    | Range (l, u) ->
      let display fmt = function
        | Finite x -> Format.fprintf fmt "%s" (Z.to_string x)
        | PosInf -> Format.fprintf fmt "+∞"
        | NegInf -> Format.fprintf fmt "-∞"
      in
      Format.fprintf fmt "[%a, %a]" display l display u

  (* application générique d'une opération binaire sur des intervalles *)
  let apply_op op x y = match (x, y) with
    | EmptySet, _ | _, EmptySet -> EmptySet
    | Range (l1, u1), Range (l2, u2) ->
      let values = [op l1 l2; op l1 u2; op u1 l2; op u1 u2] in
      create (List.fold_left min_bound PosInf values) (List.fold_left max_bound NegInf values)

  (* opérations arithmétiques *)
  let add = apply_op (fun a b -> match (a, b) with
    | Finite x, Finite y -> Finite (Z.add x y)
    | NegInf, _ | _, NegInf -> NegInf
    | PosInf, _ | _, PosInf -> PosInf)

  let sub x y = add x (apply_op (fun a _ -> negate a) y x)

  let multiply = apply_op (fun a b -> match (a, b) with
    | Finite x, Finite y -> Finite (Z.mul x y)
    | _, Finite y when Z.equal y Z.zero -> Finite Z.zero
    | Finite x, _ when Z.equal x Z.zero -> Finite Z.zero
    | PosInf, PosInf | NegInf, NegInf -> PosInf
    | PosInf, NegInf | NegInf, PosInf -> NegInf
    | _ -> PosInf)

let divide x y =
  match y with
  | Range (Finite z1, Finite z2) when Z.equal z1 Z.zero && Z.equal z2 Z.zero -> EmptySet
  | Range (Finite _, Finite z) when Z.equal z Z.zero -> EmptySet
  | _ ->
      apply_op
        (fun a b ->
          match (a, b) with
          | Finite _, Finite y when Z.equal y Z.zero -> Finite Z.zero
          | Finite x, Finite y -> Finite (Z.div x y)
          | PosInf, Finite y when Z.gt y Z.zero -> PosInf
          | NegInf, Finite y when Z.gt y Z.zero -> NegInf
          | _ -> PosInf)
        x y


  (* valeurs spéciales *)
  let full_range = Range (NegInf, PosInf)
  let empty = EmptySet
  let singleton c = Range (Finite c, Finite c)
  let random_range a b = if Z.compare a b > 0 then EmptySet else Range (Finite a, Finite b)
end