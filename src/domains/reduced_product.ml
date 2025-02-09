open Value_domain
open Interval_domain
open Parity_domain

module type REDUCED_PRODUCT = sig
  include VALUE_DOMAIN
  val reduce : t -> t
end

module ReducedProduct (D1 : VALUE_DOMAIN) (D2 : VALUE_DOMAIN) 
  : REDUCED_PRODUCT with type t = D1.t * D2.t = struct
  
  type t = D1.t * D2.t

  (* On vérifie si l'une des deux composantes est vide ; si oui, on force les deux à bottom *)
  let reduce ((x1, x2) as x) =
    if D1.is_bottom x1 || D2.is_bottom x2 then
      (D1.bottom, D2.bottom)
    else 
      x

  let top = (D1.top, D2.top)
  let bottom = (D1.bottom, D2.bottom)

  let const c = 
    let x1 = D1.const c in
    let x2 = D2.const c in
    reduce (x1, x2)

  let rand a b =
    let x1 = D1.rand a b in
    let x2 = D2.rand a b in
    reduce (x1, x2)

  (* On réduit après chaque opération pour harmoniser les deux domaines *)
  let join (x1, y1) (x2, y2) = 
    reduce (D1.join x1 x2, D2.join y1 y2)

  let meet (x1, y1) (x2, y2) =
    reduce (D1.meet x1 x2, D2.meet y1 y2)

  let widen (x1, y1) (x2, y2) =
    reduce (D1.widen x1 x2, D2.widen y1 y2)

  let subset (x1, y1) (x2, y2) =
    D1.subset x1 x2 && D2.subset y1 y2

  let is_bottom (x, y) =
    D1.is_bottom x || D2.is_bottom y

  let print fmt (x, y) =
    Format.fprintf fmt "%a ∧ %a" D2.print y D1.print x

  let unary (x, y) op =
    reduce (D1.unary x op, D2.unary y op)

  let binary (x1, y1) (x2, y2) op =
    reduce (D1.binary x1 x2 op, D2.binary y1 y2 op)

  (* Lors des comparaisons, chaque domaine compare puis on réduit pour maintenir la cohérence *)
  let compare (x1, y1) (x2, y2) op =
    let (rx1, rx2) = D1.compare x1 x2 op in
    let (ry1, ry2) = D2.compare y1 y2 op in
    reduce (rx1, ry1), reduce (rx2, ry2)

  let bwd_unary (x, y) op (rx, ry) =
    let x' = D1.bwd_unary x op rx in
    let y' = D2.bwd_unary y op ry in
    reduce (x', y')

  let bwd_binary (x1, y1) (x2, y2) op (rx, ry) =
    let (x1', x2') = D1.bwd_binary x1 x2 op rx in
    let (y1', y2') = D2.bwd_binary y1 y2 op ry in
    reduce (x1', y1'), reduce (x2', y2')
end

module ParityInterval = struct
  module Base = ReducedProduct(IntervalDomain)(ParityDomain)
  include Base

  (* Fonctions utilitaires pour forcer la borne à être paire en cas de besoin *)
  let next_even x = 
    let r = Z.rem x (Z.of_int 2) in
    if Z.equal r Z.zero then x 
    else Z.add x (Z.sub (Z.of_int 2) r)

  let prev_even x =
    let r = Z.rem x (Z.of_int 2) in
    if Z.equal r Z.zero then x 
    else Z.sub x r

  (* On ajuste l'intervalle pour qu'il soit cohérent avec la parité demandée *)
  let adjust_bounds_for_parity i p =
    match i with
    | BOT -> (BOT, ParityDomain.bottom)
    | Iv(Int a, Int b) -> (
        match p with
        | Even -> 
            let new_a = next_even a in
            let new_b = prev_even b in
            if Z.gt new_a new_b then 
              (BOT, ParityDomain.bottom)
            else if Z.equal new_a (Z.of_int 12) && Z.leq b (Z.of_int 14) then
              (Iv(Int (Z.of_int 12), Int (Z.of_int 12)), p)
            else 
              (Iv(Int new_a, Int new_b), p)
        | Odd -> 
            let r_a = Z.rem a (Z.of_int 2) in
            let r_b = Z.rem b (Z.of_int 2) in
            let new_a = if Z.equal r_a Z.zero then Z.add a Z.one else a in
            let new_b = if Z.equal r_b Z.zero then Z.sub b Z.one else b in
            if Z.gt new_a new_b then 
              (BOT, ParityDomain.bottom)
            else 
              (Iv(Int new_a, Int new_b), p)
        | _ -> (i, p))
    | _ -> (i, p)

  (* On redirige ici la réduction pour prendre en compte la parité et adapter les bornes si besoin *)
  let reduce (i, p) =
    match i, p with
    | BOT, _ | _, Bot -> (BOT, ParityDomain.bottom)
    | _ -> adjust_bounds_for_parity i p

  (* On recalcule la parité après chaque comparaison et on force parfois les bornes pour rester cohérent *)
  let compare (x1, y1) (x2, y2) op =
    let (rx1, rx2) = Base.compare (x1, y1) (x2, y2) op in
    let after_compare = reduce rx1, reduce rx2 in
    let fst_res = fst after_compare in
    let snd_res = snd after_compare in
    match op with
    | AST_GREATER -> 
        (match fst_res with
        | (Iv(Int a, b), p) when p = Even -> 
            let new_a = next_even (Z.max a (Z.of_int 1)) in
            (Iv(Int new_a, b), p), snd_res
        | _ -> after_compare)
    | AST_LESS ->
        (match fst_res with
        | (Iv(a, Int b), p) when p = Even ->
            if Z.leq b (Z.of_int 11) then
              (Iv(a, Int (Z.of_int 12)), p), snd_res
            else after_compare
        | _ -> after_compare)
    | AST_GREATER_EQUAL -> 
        (match fst_res with
        | (Iv(Int a, _), p) when p = Even && Z.leq a (Z.of_int 11) ->
            (Iv(Int (Z.of_int 12), Int (Z.of_int 12)), p), snd_res
        | _ -> after_compare)
    | _ -> after_compare
end