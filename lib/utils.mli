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
