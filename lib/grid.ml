open! Core

type 'a t = 'a array array [@@deriving sexp]

module Position = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let ( - ) = Tuple2.map2 ~f:( - )
  let ( + ) = Tuple2.map2 ~f:( + )
  let ( mod ) t m = Tuple2.map t ~f:(fun t -> t mod m)
  let ( / ) t i = Tuple2.map t ~f:(fun t -> t / i)
  let ( * ) t i = Tuple2.map t ~f:(fun t -> t * i)
end

module Direction = struct
  type t =
    [ `N
    | `E
    | `S
    | `W
    ]
  [@@deriving enumerate, sexp, compare, equal]

  include Comparable.Make (struct
      type t =
        [ `N
        | `E
        | `S
        | `W
        ]
      [@@deriving sexp, compare]
    end)

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

  type (_, _) restriction =
    | Cardinal : (t, [> t ]) restriction
    | Horizontal : (horizontal, [> vertical ]) restriction
    | Vertical : (vertical, [> horizontal ]) restriction
    | Diagonal : (diagonals, [> diagonals ]) restriction
    | None : (t_with_diagonals, t_with_diagonals) restriction

  type turn =
    | Left
    | Right
  [@@deriving enumerate, compare, equal, sexp]

  let[@unroll 1] rec turn
    : type input result.
      restrict_to:(input, result) restriction -> input -> turn -> result
    =
    fun ~restrict_to direction turn_ : result ->
    match restrict_to, (direction, turn_) with
    | None, ((#horizontal as direction), turn_) ->
      turn ~restrict_to:Horizontal direction turn_
    | None, ((#vertical as direction), turn_) ->
      turn ~restrict_to:Vertical direction turn_
    | None, ((#diagonals as direction), turn_) ->
      turn ~restrict_to:Diagonal direction turn_
    | Cardinal, ((#horizontal as direction), turn_) ->
      turn ~restrict_to:Horizontal direction turn_
    | Cardinal, ((#vertical as direction), turn_) ->
      turn ~restrict_to:Vertical direction turn_
    | Horizontal, (`E, Left | `W, Right) -> `N
    | Horizontal, (`W, Left | `E, Right) -> `S
    | Vertical, (`N, Left | `S, Right) -> `W
    | Vertical, (`S, Left | `N, Right) -> `E
    | Diagonal, (`NE, Left | `SW, Right) -> `NW
    | Diagonal, (`SW, Left | `NE, Right) -> `SE
    | Diagonal, (`NW, Left | `SE, Right) -> `SW
    | Diagonal, (`SE, Left | `NW, Right) -> `NE
    | _ -> .
  ;;

  let turn_diagonals direction turn_ = turn ~restrict_to:Diagonal direction turn_
  let turn' direction turn_ = turn ~restrict_to:None direction turn_
  let turn direction turn_ = turn ~restrict_to:Cardinal direction turn_

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

let horizontal_project (x, _) = if x < 0 then Some `W else if x > 0 then Some `E else None
let vertical_project (_, y) = if y < 0 then Some `N else if y > 0 then Some `S else None

let ( .?() ) : 'a t -> _ -> 'a option =
  fun grid (i, j) -> Option.try_with @@ fun () -> grid.(j).(i)
;;

let ( .^() ) : 'a t -> _ -> 'a = fun grid (i, j) -> grid.(j).(i)
let ( .^()<- ) = fun grid (i, j) el -> grid.(j).(i) <- el
let height grid = Array.length grid
let width grid = Array.length grid.(0)

let in_grid grid (x, y) =
  Int.between ~low:0 ~high:(height grid - 1) y
  && Int.between ~low:0 ~high:(width grid - 1) x
;;

let iter grid ~f = Array.iter grid ~f:(fun row -> Array.iter row ~f)

let iteri grid ~f =
  Array.iteri grid ~f:(fun y row -> Array.iteri row ~f:(fun x cell -> f (x, y) cell))
;;

let map grid ~f = Array.map grid ~f:(fun row -> Array.map row ~f)

let rec iteri_from grid ~pos ~direction ~f =
  let pos = Direction.step pos direction in
  if in_grid grid pos
  then (
    f pos grid.^(pos);
    iteri_from grid ~pos ~direction ~f)
;;
