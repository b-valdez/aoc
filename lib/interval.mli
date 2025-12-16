type t = int * int [@@deriving sexp, compare]

val mem : t -> int -> bool
val is_disjoint : t -> t -> bool

type set

val set_of_interval : t -> set

(** O(n^2)*)
val set_of_iter : t Iter.t -> set

(** Ordered according to the first value of each interval, O(n) *)
val set_of_ordered_list : t list -> set

(** Sorts and then calls set_of_ordered_list, O(n * ln(n)) *)
val set_of_list : t list -> set

(** O(n + m) *)
val merge : set -> set -> set

(** O(n) *)
val length : set -> int

(** O(n) *)
val set_mem : set -> int -> bool
