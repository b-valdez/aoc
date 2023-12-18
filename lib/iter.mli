(** @inline *)
include
  module type of IterLabels
  with module Map := IterLabels.Map
   and module Set := IterLabels.Set

(** {2 Added for AOC} *)

(** converts e.g. [Core.Map.iteri map] to a [Iter.t] *)
val of_map_iteri : (f:(key:'a -> data:'b -> unit) -> unit) -> ('a * 'b) t

type 'a gen =
  { next : 'b. 'b -> 'a
  ; free : unit -> unit
  }

val to_gen : 'a t -> 'a gen
