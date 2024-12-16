open! Core

val detect_loop_with_affine_accumulator
  :  ?size:int
  -> 'a Base.Hashtbl.Key.t
  -> f:('a -> 'b -> 'a * 'b)
  -> init_state:'a
  -> init_acc:'b
  -> skip_to:int
  -> compute_final_accumulator:
       (found_in_loop:'b -> loops:int -> before_loop:'b -> after_loop:'b -> 'b)
  -> 'a * 'b

val detect_loop_and_skip_to_mapped
  :  ?size:int
  -> map:('a -> 'b)
  -> unmap:('b -> 'c)
  -> 'b Base.Hashtbl.Key.t
  -> f:('a -> 'a)
  -> init:'a
  -> skip_to:int
  -> 'c

val detect_loop_and_skip_to
  :  ?size:int
  -> 'a Base.Hashtbl.Key.t
  -> f:('a -> 'a)
  -> init:'a
  -> skip_to:int
  -> 'a

module type Comparable_sexpable = sig
  type t [@@deriving compare, sexp_of]

  include Comparator.S with type t := t
end

module type Comparable_sexpable_summable = sig
  type t [@@deriving compare, sexp_of]

  include Container.Summable with type t := t
end

val dijkstra
  :  ?verbose:unit
  -> (module Comparable_sexpable with type t = 'key)
  -> (module Comparable_sexpable with type t = 'priority)
  -> step:('key -> 'priority -> ('key * 'priority) Iter.t)
  -> sorted_start_positions:('key * 'priority) list
  -> is_goal:('key -> bool)
  -> 'key * 'priority

val a_star
  :  ?verbose:unit
  -> (module Comparable_sexpable with type t = 'key)
  -> (module Comparable_sexpable_summable with type t = 'priority)
  -> heuristic:('key -> 'priority)
  -> step:('key -> 'priority -> ('key * 'priority) Iter.t)
  -> start_positions:('key * 'priority) list
  -> is_goal:('key -> bool)
  -> 'key * 'priority

type 'a branching_paths =
  | Branching of
      { shared_end : 'a list
      ; branches : 'a branching_paths list
      ; shared_rev_start : 'a list
      }
  | Non_branching of 'a list
[@@deriving sexp, compare]

(** inefficient and wrong *)
val a_star_all_paths
  :  ?verbose:unit
  -> (module Comparable_sexpable with type t = 'key)
  -> (module Comparable_sexpable_summable with type t = 'priority)
  -> heuristic:('key -> 'priority)
  -> step:('key -> 'priority -> ('key * 'priority) Iter.t)
  -> start_positions:('key * 'priority) list
  -> is_goal:('key -> bool)
  -> 'key * 'priority * 'key branching_paths
