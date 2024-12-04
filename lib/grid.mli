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
    [ `N
    | `E
    | `S
    | `W
    ]
  [@@deriving enumerate, sexp, compare, equal]

  type horizontal =
    [ `E
    | `W
    ]

  val all_of_horizontal : t list

  type vertical =
    [ `N
    | `S
    ]

  val all_of_vertical : t list

  type diagonals =
    [ `NE
    | `NW
    | `SE
    | `SW
    ]
  [@@deriving enumerate, sexp, compare, equal]

  type t_with_diagonals =
    [ t
    | diagonals
    ]
  [@@deriving enumerate, sexp, compare, equal]

  val step : int * int -> [< t_with_diagonals ] -> int * int

  type turn =
    | Left
    | Right
  [@@deriving enumerate, compare, equal, sexp]

  val turn : t -> turn -> t
  val turn_diagonals : diagonals -> turn -> diagonals
  val opposite : t -> t
  val opposite_diagonals : diagonals -> diagonals
end

val ( .?() ) : 'a t -> int * int -> 'a option
val ( .^() ) : 'a t -> int * int -> 'a
val height : 'a t -> int
val width : 'a t -> int
val in_grid : 'a t -> Position.t -> bool
val iter : 'a t -> f:('a -> unit) -> unit
val iteri : 'a t -> f:(Position.t -> 'a -> unit) -> unit
