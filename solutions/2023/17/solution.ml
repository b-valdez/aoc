open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  grid Char.get_digit_exn
;;

type turn =
  | Left
  | Right

let turn direction turn =
  match direction, turn with
  | `E, Left | `W, Right -> `N
  | `W, Left | `E, Right -> `S
  | `N, Left | `S, Right -> `W
  | `S, Left | `N, Right -> `E
;;

module State = struct
  type t =
    { position : Position.t
    ; facing : Direction.t
    }
  [@@deriving compare, sexp]

  include (val Comparator.make ~sexp_of_t ~compare)
end

let step grid min_step max_step state heat_loss =
  let open State in
  let f (side, steps) =
    let turned_direction = turn state.facing side in
    let thunk () =
      let position, heat_loss =
        Fn.apply_n_times
          ~n:steps
          (fun (pos, sum) ->
            let next_pos = Direction.step pos turned_direction in
            let sum = sum + grid.^(next_pos) in
            next_pos, sum)
          (state.position, heat_loss)
      in
      { position; facing = turned_direction }, heat_loss
    in
    Option.try_with thunk
  in
  Iter.product (Iter.doubleton Left Right) Iter.(min_step -- max_step)
  |> Iter.filter_map ~f
;;

module Queue = struct
  module Queue = Psq.Make (State) (Int)
  include Queue

  include
    Sexpable.Of_sexpable
      (struct
        type t = (State.t * int) list [@@deriving sexp]
      end)
      (struct
        type t = Queue.t

        let to_sexpable = Queue.to_list
        let of_sexpable = Queue.of_sorted_list
      end)
end

let part1 grid =
  let grid_width = Array.length grid.(0) in
  let grid_length = Array.length grid in
  let is_goal state =
    [%equal: int * int] (grid_width - 1, grid_length - 1) state.State.position
  in
  let rec aux visited queue =
    match Queue.pop queue with
    | None -> assert false
    | Some ((state, heat_loss), _) when is_goal state -> heat_loss
    | Some ((state, heat_loss), rest) ->
      let rest =
        step grid 1 3 state heat_loss
        |> Iter.filter ~f:(fun (state, _) -> not @@ Set.mem visited state)
        |> Iter.fold ~init:rest ~f:(fun rest (state, heat_loss) ->
          Queue.push state heat_loss rest)
      in
      (aux [@tailcall]) (Set.add visited state) rest
  in
  aux
    (Set.empty (module State))
    (Queue.of_sorted_list
       [ State.{ position = 0, 0; facing = `E }, 0; { position = 0, 0; facing = `S }, 0 ])
;;

let part2 grid =
  let grid_width = Array.length grid.(0) in
  let grid_length = Array.length grid in
  let is_goal state =
    [%equal: int * int] (grid_width - 1, grid_length - 1) state.State.position
  in
  let rec aux visited queue =
    match Queue.pop queue with
    | None -> assert false
    | Some ((state, heat_loss), _) when is_goal state -> heat_loss
    | Some ((state, heat_loss), rest) ->
      let rest =
        step grid 4 10 state heat_loss
        |> Iter.filter ~f:(fun (state, _) -> not @@ Set.mem visited state)
        |> Iter.fold ~init:rest ~f:(fun rest (state, heat_loss) ->
          Queue.push state heat_loss rest)
      in
      (aux [@tailcall]) (Set.add visited state) rest
  in
  aux
    (Set.empty (module State))
    (Queue.of_sorted_list
       [ State.{ position = 0, 0; facing = `E }, 0; { position = 0, 0; facing = `S }, 0 ])
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 102 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 94 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 866 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 1010 |}]
;;
