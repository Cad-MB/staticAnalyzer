open Value_domain

module I = Interval_domain.IntervalDomain
module T = Interval_domain

type disjunctive_interval = 
  | DBot
  | DIntervals of T.intervalTyp list

module DisjunctiveIntervalDomain : VALUE_DOMAIN with type t = disjunctive_interval = struct
  type t = disjunctive_interval

  let merge_overlapping_intervals intervals =
    let sorted = List.filter (fun i -> not (i = T.BOT)) intervals in
    let rec merge_process = function
      | [] | [_] as l -> l
      | i1 :: i2 :: rest ->
          if I.is_bottom (I.meet i1 i2) then
            i1 :: merge_process (i2 :: rest)
          else
            merge_process ((I.join i1 i2) :: rest)
    in
    merge_process sorted

  let top = DIntervals [I.top]
  let bottom = DBot

  let const c = DIntervals [I.const c]

  let rand a b = DIntervals [I.rand a b]

  let join x y = match x, y with
    | DBot, v | v, DBot -> v
    | DIntervals i1, DIntervals i2 ->
        (* Conserver les intervalles séparés pour garder les disjonctions. *)
        DIntervals (i1 @ i2)

  let meet x y = match x, y with
    | DBot, _ | _, DBot -> DBot
    | DIntervals i1, DIntervals i2 ->
        let results = List.fold_left (fun acc int1 ->
          List.fold_left (fun acc' int2 ->
            let m = I.meet int1 int2 in
            if m = T.BOT then acc'
            else m :: acc'
          ) acc i2
        ) [] i1 in
        match results with
        | [] -> DBot
        | ints -> DIntervals ints

  let widen x y = match x, y with
    | DBot, v | v, DBot -> v
    | DIntervals i1, DIntervals i2 ->
        try 
          let widened = List.map2 I.widen i1 i2 in
          DIntervals (merge_overlapping_intervals widened)
        with Invalid_argument _ -> top

  let subset x y = match x, y with
    | DBot, _ -> true
    | _, DBot -> false
    | DIntervals i1, DIntervals i2 ->
        List.for_all (fun int1 ->
          List.exists (fun int2 -> I.subset int1 int2) i2
        ) i1

  let is_bottom = function
    | DBot -> true
    | DIntervals [] -> true
    | DIntervals l -> List.for_all (fun x -> x = T.BOT) l

  let print fmt = function
    | DBot -> Format.fprintf fmt "⊥"
    | DIntervals ints ->
        Format.fprintf fmt "{";
        List.iteri (fun i int ->
          if i > 0 then Format.fprintf fmt " ∨ ";
          I.print fmt int
        ) ints;
        Format.fprintf fmt "}"

  let unary x op = match x with
    | DBot -> DBot
    | DIntervals ints ->
        DIntervals (List.map (fun i -> I.unary i op) ints)

  let binary x y op = match x, y with
    | DBot, _ | _, DBot -> DBot
    | DIntervals i1, DIntervals i2 ->
        let results = List.fold_left (fun acc int1 ->
          List.fold_left (fun acc' int2 ->
            let res = I.binary int1 int2 op in
            if res = T.BOT then acc'
            else res :: acc'
          ) acc i2
        ) [] i1 in
        match results with
        | [] -> DBot
        | ints -> DIntervals ints (* Ne pas fusionner pour garder les disjonctions *)

  let compare x y op = match x, y with
    | DBot, _ | _, DBot -> DBot, DBot
    | DIntervals i1, DIntervals i2 ->
        let process_intervals int1 int2 =
            let (rx, ry) = I.compare int1 int2 op in
            if rx = T.BOT || ry = T.BOT then []
            else [(rx, ry)]
        in
        let results = List.fold_left (fun acc int1 ->
            List.fold_left (fun acc' int2 ->
                acc' @ process_intervals int1 int2
            ) acc i2
        ) [] i1
        in
        match results with
        | [] -> DBot, DBot
        | l -> 
            let rx, ry = List.split l in
            DIntervals rx, DIntervals ry

  let bwd_unary x op r = match x, r with
    | DBot, _ | _, DBot -> DBot
    | DIntervals i1, DIntervals i2 ->
        let results = List.fold_left (fun acc int1 ->
          List.fold_left (fun acc' int2 ->
            let res = I.bwd_unary int1 op int2 in
            if res = T.BOT then acc'
            else res :: acc'
          ) acc i2
        ) [] i1 in
        match results with
        | [] -> DBot
        | ints -> DIntervals ints (* Ne pas fusionner pour garder les disjonctions *)

  let bwd_binary x y op r = match x, y, r with
    | DBot, _, _ | _, DBot, _ | _, _, DBot -> DBot, DBot
    | DIntervals i1, DIntervals i2, DIntervals i3 ->
        let results = List.fold_left (fun acc int1 ->
          List.fold_left (fun acc' int2 ->
            List.fold_left (fun acc'' int3 ->
              let (rx, ry) = I.bwd_binary int1 int2 op int3 in
              if rx = T.BOT || ry = T.BOT then acc''
              else (rx, ry) :: acc''
            ) acc' i3
          ) acc i2
        ) [] i1 in
        match results with
        | [] -> DBot, DBot
        | l -> 
            let rx, ry = List.split l in
            DIntervals rx, DIntervals ry
end