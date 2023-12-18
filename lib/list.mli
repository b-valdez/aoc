include module type of Core.List (** @inline *)

(** {2 Added for AOC} *)

(** Drops the largest prefix for which [f] is true and returns its size *)
val count_drop_while : f:('a -> bool) -> 'a t -> int * 'a t

(** [update_concat ~equal list el replacement] updates [list] by replacing the first instance of [el] with the elements in [replacement]. Raises if [el] not in [list].*)
val update_concat : 'a t -> 'a -> 'a t -> equal:('a -> 'a -> bool) -> 'a t
