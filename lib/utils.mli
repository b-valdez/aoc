open! Core

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
