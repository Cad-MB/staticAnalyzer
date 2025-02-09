open Abstract_syntax_tree
open Value_domain

type parity =
  | Even
  | Odd
  | Top
  | Bot

module ParityDomain : sig
  include VALUE_DOMAIN with type t = parity
  val is_even : t -> bool
  val is_odd : t -> bool
end = struct
  type t = parity

  let top = Top
  let bottom = Bot

  let is_even = function
    | Even -> true
    | _ -> false

  let is_odd = function
    | Odd -> true
    | _ -> false

  (* Détermine la parité d'une constante en se basant sur x mod 2 *)
  let const x = 
    if Z.equal (Z.rem x (Z.of_int 2)) Z.zero then Even
    else Odd

  (* Renvoie Top si on a un intervalle de valeurs > 1, Bot si x > y (intervalle vide) *)
  let rand x y = 
    if Z.equal x y then const x
    else if Z.gt x y then Bot
    else Top

  (* Fusion de deux parités : Top si elles diffèrent (hors Bot) *)
  let join x y = match x, y with
    | Bot, v | v, Bot -> v
    | Even, Even -> Even
    | Odd, Odd -> Odd
    | _, _ -> Top

  (* Intersection de deux parités : Bot si elles ne peuvent coexister *)
  let meet x y = match x, y with
    | Bot, _ | _, Bot -> Bot
    | Top, v | v, Top -> v
    | Even, Even -> Even
    | Odd, Odd -> Odd
    | _, _ -> Bot

  (* L'élargissement réutilise la sémantique de join pour rester approximatif *)
  let widen = join

  (* Vérifie l'inclusion dans le domaine, Bot ⊆ tout, tout ⊆ Top *)
  let subset x y = match x, y with
    | Bot, _ -> true
    | _, Top -> true
    | Even, Even -> true
    | Odd, Odd -> true
    | _, _ -> false

  let is_bottom x = x = Bot

  let print fmt = function
    | Even -> Format.fprintf fmt "even"
    | Odd -> Format.fprintf fmt "odd"
    | Top -> Format.fprintf fmt "⊤"
    | Bot -> Format.fprintf fmt "⊥"

  (* Les opérations unaire + et - ne changent pas la parité *)
  let unary x = function
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> x

  (* Déduit la parité d'une opération binaire sur x et y selon l'opérateur *)
  let binary x y op = match op with
    | AST_PLUS | AST_MINUS ->
        (match x, y with
         | Even, Even | Odd, Odd -> Even
         | Even, Odd | Odd, Even -> Odd
         | Bot, _ | _, Bot -> Bot
         | _, _ -> Top)
    | AST_MULTIPLY ->
        (match x, y with
         | Bot, _ | _, Bot -> Bot
         | Even, _ | _, Even -> Even
         | Odd, Odd -> Odd
         | _, _ -> Top)
    | AST_DIVIDE ->
        (match x, y with
         | Bot, _ | _, Bot -> Bot
         | _, Even -> Bot  (* Division par un pair → incertaine, on choisit Bot *)
         | Even, Odd -> Even
         | Odd, Odd -> Odd
         | _, _ -> Top)

  (* Calcule l'effet d'une comparaison : si x ≠ y → Bot si x=y autrement *)
  let compare : t -> t -> compare_op -> t * t = fun x y op ->
    if x = Bot || y = Bot then Bot, Bot
    else match op with
      | AST_EQUAL ->
          if x = y then x, y
          else if x = Top then y, y
          else if y = Top then x, x
          else Bot, Bot
      | AST_NOT_EQUAL ->
          if x = y then Bot, Bot
          else x, y
      | _ ->
          x, y

  (* Arrière pour l'uniaire : on recalcule la rencontre pour rester cohérent *)
  let bwd_unary x op r = meet x (unary r op)

  (* Arrière pour le binaire : on refait la rencontre pour x et y *)
  let bwd_binary x y _op r =
    meet x r, meet y r
end