(*
  Cours "Typage et Analyse Statique" - Master STL
  Sorbonne Université
  Antoine Miné 2015-2022
*)

(*
  Pretty-printer for abstract syntax trees.
*)

open Format
open Abstract_syntax_tree

(* locations *)
val string_of_position: position -> string
val string_of_extent: extent -> string

(* printers *)
val print_typ: formatter -> typ -> unit
val print_var: formatter -> var -> unit
val print_int_expr: formatter -> int_expr -> unit
val print_bool_expr: formatter -> bool_expr -> unit
val print_stat: string -> formatter -> stat -> unit
val print_block: string -> formatter -> (typ * var) ext list -> stat ext list -> unit
val print_prog: formatter -> prog -> unit
val string_of_int_unary_op : int_unary_op -> string
val string_of_bool_unary_op : bool_unary_op -> string
val string_of_int_binary_op : int_binary_op -> string
val string_of_compare_op : compare_op -> string
val string_of_bool_binary_op : bool_binary_op -> string
