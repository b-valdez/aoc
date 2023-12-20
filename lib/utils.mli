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

val a_star
  :  ?verbose:unit
  -> ('key, _) Comparator.comparator
  -> ('priority, _) Comparator.comparator
  -> step:('key -> 'priority -> ('key * 'priority) Iter.t)
  -> sorted_start_positions:('key * 'priority) list
  -> is_goal:('key -> bool)
  -> 'key * 'priority
