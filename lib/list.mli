include module type of Core.List (** @inline *)

(** {2 Added for AOC} *)

(** Drops the largest prefix for which [f] is true and returns its size *)
val count_drop_while : f:('a -> bool) -> 'a t -> int * 'a t
