open Core

type 'a t = 'a array array

val t_of_sexp : (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t
val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t

module Position : sig
  type t = int * int [@@deriving sexp, compare, hash]

  include Comparable.S with type t := t
end

module Direction : sig
  type t =
    [ `E
    | `N
    | `S
    | `W
    ]
  [@@deriving enumerate, sexp, compare, equal]

  type horizontal =
    [ `E
    | `W
    ]
  [@@deriving enumerate]

  type vertical =
    [ `N
    | `S
    ]
  [@@deriving enumerate]

  val step : int * int -> [< `E | `N | `S | `W ] -> int * int
end

val ( .?() ) : 'a t -> int * int -> 'a option
val ( .^() ) : 'a t -> int * int -> 'a
