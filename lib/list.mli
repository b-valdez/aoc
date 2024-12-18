include module type of Core.List with module Assoc := Core.List.Assoc (** @inline *)

module Assoc : sig
  (** inline *)
  include module type of Core.List.Assoc

  (** {2 Added for AOC} *)

  val update
    :  ('a, 'b) t
    -> 'a
    -> f:('b option -> 'b option)
    -> equal:('a -> 'a -> bool)
    -> ('a, 'b) t
end

(** {2 Added for AOC} *)

(** Drops the largest prefix for which [f] is true and returns its size *)
val count_drop_while : f:('a -> bool) -> 'a t -> int * 'a t

(** [update_concat ~equal list el replacement] updates [list] by replacing the first instance of [el] with the elements in [replacement]. Raises if [el] not in [list].*)
val update_concat : 'a t -> 'a -> 'a t -> equal:('a -> 'a -> bool) -> 'a t

val maybe_cons : 'a option -> 'a t -> 'a t

val printf_list
  :  ?sep:(out_channel -> unit)
  -> (out_channel -> 'a -> unit)
  -> out_channel
  -> 'a list
  -> unit
