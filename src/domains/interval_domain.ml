(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)

(* domaine des intervalles pour l'analyse statique *)

open Abstract_syntax_tree

type bound =
  | PosInf
  | NegInf
  | Finite of Z.t

type interval =
  | Range of bound * bound
  | EmptySet

module BoundOps = struct
  let compare_bounds a b = match a, b with
    | NegInf, NegInf | PosInf, PosInf -> 0
    | NegInf, _ | _, PosInf -> -1
    | PosInf, _ | _, NegInf -> 1
    | Finite x, Finite y -> Z.compare x y

  let min_bound a b = if compare_bounds a b <= 0 then a else b
  let max_bound a b = if compare_bounds a b >= 0 then a else b

  let negate = function
    | PosInf -> NegInf
    | NegInf -> PosInf
    | Finite x -> Finite (Z.neg x)

  let bound_succ = function
    | Finite z -> Finite (Z.succ z)
    | NegInf -> NegInf
    | PosInf -> PosInf

  let bound_pred = function
    | Finite z -> Finite (Z.pred z)
    | NegInf -> NegInf
    | PosInf -> PosInf
end

module IntervalDomain = struct
  open BoundOps

  type t = interval

  let create l u =
    if compare_bounds l u > 0 then EmptySet else Range(l, u)

  let print fmt = function
    | EmptySet -> Format.fprintf fmt "⊥"
    | Range(l, u) ->
      let display fmt = function
        | Finite x -> Format.fprintf fmt "%s" (Z.to_string x)
        | PosInf   -> Format.fprintf fmt "+∞"
        | NegInf   -> Format.fprintf fmt "-∞"
      in
      Format.fprintf fmt "[%a;%a]" display l display u

  let apply_op op x y =
    match x, y with
    | EmptySet, _ | _, EmptySet -> EmptySet
    | Range(l1, u1), Range(l2, u2) ->
      let vals = [op l1 l2; op l1 u2; op u1 l2; op u1 u2] in
      create
        (List.fold_left min_bound PosInf  vals)
        (List.fold_left max_bound NegInf vals)

  let add =
    apply_op (fun a b ->
      match a, b with
      | Finite x, Finite y -> Finite (Z.add x y)
      | NegInf, _ | _, NegInf -> NegInf
      | PosInf, _ | _, PosInf -> PosInf
    )

  let sub x y =
    add x (apply_op (fun a _ -> negate a) y y)

  let multiply =
    apply_op (fun a b ->
      match a, b with
      | Finite x, Finite y -> Finite (Z.mul x y)
      | _, Finite y when Z.equal y Z.zero -> Finite Z.zero
      | Finite x, _ when Z.equal x Z.zero -> Finite Z.zero
      | PosInf, PosInf | NegInf, NegInf   -> PosInf
      | PosInf, NegInf | NegInf, PosInf   -> NegInf
      | _ -> PosInf
    )

  let divide x y =
    match y with
    | Range(Finite z1, Finite z2) when Z.equal z1 Z.zero && Z.equal z2 Z.zero ->
      EmptySet
    | Range(Finite _, Finite z) when Z.equal z Z.zero ->
      EmptySet
    | _ ->
      apply_op
        (fun a b ->
          match a, b with
          | Finite _, Finite vy when Z.equal vy Z.zero -> Finite Z.zero
          | Finite vx, Finite vy -> Finite (Z.div vx vy)
          | PosInf, Finite vy when Z.gt vy Z.zero -> PosInf
          | NegInf, Finite vy when Z.gt vy Z.zero -> NegInf
          | _ -> PosInf
        )
        x y

  let join x y =
    match x, y with
    | EmptySet, b | b, EmptySet -> b
    | Range(l1, u1), Range(l2, u2) ->
      create (min_bound l1 l2) (max_bound u1 u2)

  let meet x y =
    match x, y with
    | EmptySet, _ | _, EmptySet -> EmptySet
    | Range(l1, u1), Range(l2, u2) ->
      let nl = max_bound l1 l2 in
      let nu = min_bound u1 u2 in
      if compare_bounds nl nu > 0 then EmptySet else Range(nl, nu)

  let subset x y =
    match x, y with
    | EmptySet, _ -> true
    | _, EmptySet -> false
    | Range(l1, u1), Range(l2, u2) ->
      compare_bounds l2 l1 <= 0 && compare_bounds u1 u2 <= 0

  let is_bottom x = (x = EmptySet)

  let widen x y =
    match x, y with
    | EmptySet, b | b, EmptySet -> b
    | Range(l1, u1), Range(l2, u2) ->
      let l = if compare_bounds l2 l1 < 0 then NegInf else l1 in
      let u = if compare_bounds u2 u1 > 0 then PosInf else u1 in
      Range(l, u)

  let unary v op =
    match op with
    | AST_UNARY_PLUS  -> v
    | AST_UNARY_MINUS ->
      match v with
      | EmptySet -> EmptySet
      | Range(l, u) -> Range(negate u, negate l)

  let binary x y = function
    | AST_PLUS      -> add x y
    | AST_MINUS     -> sub x y
    | AST_MULTIPLY  -> multiply x y
    | AST_DIVIDE    -> divide x y

  let compare x y op =
    match x, y, op with
    | EmptySet, _, _ | _, EmptySet, _ ->
      (EmptySet, EmptySet)

    | Range(lx, ux), Range(ly, uy), AST_EQUAL ->
      let inter = meet (Range(lx, ux)) (Range(ly, uy)) in
      (inter, inter)

    | Range(lx, ux), Range(ly, uy), AST_NOT_EQUAL ->
      let inter = meet (Range(lx, ux)) (Range(ly, uy)) in
      let x' =
        match inter with
        | Range(Finite v1, Finite v2) when Z.equal v1 v2 ->
          let left_part =
            if compare_bounds (Finite v1) lx > 0 then
              Range(lx, Finite(Z.pred v1))
            else
              EmptySet
          in
          let right_part =
            if compare_bounds (Finite v1) ux < 0 then
              Range(Finite(Z.succ v1), ux)
            else
              EmptySet
          in
          join left_part right_part
        | _ ->
          Range(lx, ux)
      in
      (x', Range(ly, uy))

    | Range(lx, ux), Range(ly, uy), AST_LESS_EQUAL ->
      let x' = meet (Range(lx, ux)) (Range(NegInf, uy)) in
      let y' = meet (Range(ly, uy)) (Range(lx, PosInf)) in
      (x', y')

    | Range(lx, ux), Range(ly, uy), AST_LESS ->
      let x' = meet (Range(lx, ux)) (Range(NegInf, bound_pred uy)) in
      let y' = meet (Range(ly, uy)) (Range(bound_succ lx, PosInf)) in
      (x', y')

    | Range(lx, ux), Range(ly, uy), AST_GREATER_EQUAL ->
      let x' = meet (Range(lx, ux)) (Range(ly, PosInf)) in
      let y' = meet (Range(ly, uy)) (Range(NegInf, ux)) in
      (x', y')

    | Range(lx, ux), Range(ly, uy), AST_GREATER ->
      let x' = meet (Range(lx, ux)) (Range(bound_succ ly, PosInf)) in
      let y' = meet (Range(ly, uy)) (Range(NegInf, bound_pred ux)) in
      (x', y')

  let bwd_unary x op r =
    match op with
    | AST_UNARY_PLUS  -> meet x r
    | AST_UNARY_MINUS -> meet x (sub EmptySet r)

  let bwd_binary x y op r =
    match op with
    | AST_PLUS ->
      (meet x (sub r y), meet y (sub r x))
    | AST_MINUS ->
      (meet x (add y r), meet y (sub x r))
    | AST_MULTIPLY ->
      (meet x (divide r y), meet y (divide r x))
    | AST_DIVIDE ->
      (x, y)

  let top = Range(NegInf, PosInf)
  let bottom = EmptySet

  let singleton c = Range(Finite c, Finite c)
  let const c = singleton c

  let random_range a b =
    if Z.compare a b > 0 then EmptySet
    else Range(Finite a, Finite b)

  let rand x y = random_range x y
end