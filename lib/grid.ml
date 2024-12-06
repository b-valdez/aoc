open! Core

type 'a t = 'a array array [@@deriving sexp]

module Position = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
end

module Direction = struct
  type t =
    [ `N
    | `E
    | `S
    | `W
    ]
  [@@deriving enumerate, sexp, compare, equal]

  include (val Comparator.make ~compare ~sexp_of_t)

  type horizontal =
    [ `E
    | `W
    ]
  [@@deriving enumerate]

  let all_of_horizontal = (all_of_horizontal :> t list)

  type vertical =
    [ `N
    | `S
    ]
  [@@deriving enumerate]

  let all_of_vertical = (all_of_vertical :> t list)

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

  let step (x, y) = function
    | `N -> x, y - 1
    | `E -> x + 1, y
    | `S -> x, y + 1
    | `W -> x - 1, y
    | `NE -> x + 1, y - 1
    | `NW -> x - 1, y - 1
    | `SE -> x + 1, y + 1
    | `SW -> x - 1, y + 1
  ;;

  type turn =
    | Left
    | Right
  [@@deriving enumerate, compare, equal, sexp]

  let turn direction turn =
    match direction, turn with
    | `E, Left | `W, Right -> `N
    | `W, Left | `E, Right -> `S
    | `N, Left | `S, Right -> `W
    | `S, Left | `N, Right -> `E
  ;;

  let turn_diagonals direction turn =
    match direction, turn with
    | `NE, Left | `SW, Right -> `NW
    | `SW, Left | `NE, Right -> `SE
    | `NW, Left | `SE, Right -> `SW
    | `SE, Left | `NW, Right -> `NE
  ;;

  let opposite = function
    | `N -> `S
    | `S -> `N
    | `E -> `W
    | `W -> `E
  ;;

  let opposite_diagonals = function
    | `NE -> `SW
    | `SW -> `NE
    | `SE -> `NW
    | `NW -> `SE
  ;;
end

let ( .?() ) : 'a t -> _ -> 'a option =
  fun grid (i, j) -> Option.try_with @@ fun () -> grid.(j).(i)
;;

let ( .^() ) : 'a t -> _ -> 'a = fun grid (i, j) -> grid.(j).(i)
let height grid = Array.length grid
let width grid = Array.length grid.(0)

let in_grid grid (x, y) =
  Int.between ~low:0 ~high:(height grid - 1) y
  && Int.between ~low:0 ~high:(width grid - 1) x
;;

let iter grid ~f = Array.iter grid ~f:(fun row -> Array.iter row ~f:(fun cell -> f cell))

let iteri grid ~f =
  Array.iteri grid ~f:(fun y row -> Array.iteri row ~f:(fun x cell -> f (x, y) cell))
;;
