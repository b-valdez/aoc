(** @inline *)
include
  module type of IterLabels
  with module Map := IterLabels.Map
   and module Set := IterLabels.Set

(** {2 Added for AOC} *)

(** @inline*)
include
  Core.Monad.Syntax
  with type 'a t := 'a t
   and module Let_syntax.Let_syntax.Open_on_rhs = Infix

(** converts e.g. [Core.Map.iteri map] to a [Iter.t] *)
val of_map_iteri : (f:(key:'a -> data:'b -> unit) -> unit) -> ('a * 'b) t

val from_labelled_iter2 : (f:('a -> 'b -> unit) -> unit) -> ('a * 'b) t

type 'a gen = private
  { mutable next : unit -> 'a
  ; mutable free : unit -> unit
  }

val to_gen : 'a t -> 'a gen

type 'a functional_gen =
  { next : unit -> 'a functional_gen
  ; free : unit -> 'a functional_gen
  ; has_next : bool
  ; latest : ('a, exn) result
  }

val to_functional_gen : 'a t -> 'a functional_gen
val tap : 'a t -> f:('a -> unit) -> 'a t
