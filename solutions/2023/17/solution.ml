open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  grid Char.get_digit_exn
;;

module State = struct
  type t =
    { position : Position.t
    ; is_horizontal_facing : bool
    }
  [@@deriving compare, sexp]

  include (val Comparator.make ~sexp_of_t ~compare)
end

let step grid min_step max_step state heat_loss =
  let open State in
  let f (direction, steps) =
    let thunk () =
      let position, heat_loss =
        Fn.apply_n_times
          ~n:steps
          (fun (pos, sum) ->
            let next_pos = Direction.step pos direction in
            let sum = sum + grid.^(next_pos) in
            next_pos, sum)
          (state.position, heat_loss)
      in
      { position; is_horizontal_facing = not state.is_horizontal_facing }, heat_loss
    in
    Option.try_with thunk
  in
  let next_directions : Direction.t list =
    if state.is_horizontal_facing
    then [%all: Direction.vertical]
    else [%all: Direction.horizontal]
  in
  Iter.product (Iter.of_list next_directions) Iter.(min_step -- max_step)
  |> Iter.filter_map ~f
;;

let is_goal grid state =
  [%equal: int * int] (width grid - 1, height grid - 1) state.State.position
;;

let part1 grid =
  a_star
    State.comparator
    Int.comparator
    ~step:(step grid 1 3)
    ~is_goal:(is_goal grid)
    ~sorted_start_positions:
      [ { position = 0, 0; is_horizontal_facing = false }, 0
      ; { position = 0, 0; is_horizontal_facing = true }, 0
      ]
  |> snd
;;

let part2 grid =
  a_star
    State.comparator
    Int.comparator
    ~step:(step grid 4 10)
    ~is_goal:(is_goal grid)
    ~sorted_start_positions:
      [ { position = 0, 0; is_horizontal_facing = false }, 0
      ; { position = 0, 0; is_horizontal_facing = true }, 0
      ]
  |> snd
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
