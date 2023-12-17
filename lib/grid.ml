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

  let step (x, y) = function
    | `N -> x, y - 1
    | `E -> x + 1, y
    | `S -> x, y + 1
    | `W -> x - 1, y
  ;;
end

let ( .?() ) : 'a t -> _ -> 'a option =
  fun grid (i, j) -> Option.try_with @@ fun () -> grid.(j).(i)
;;

let ( .^() ) : 'a t -> _ -> 'a = fun grid (i, j) -> grid.(j).(i)
