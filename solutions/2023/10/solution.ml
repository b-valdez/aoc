open! Aoc_std
open Grid

(* TODO use Direction.t*)

type direction =
  | N
  | E
  | S
  | W
[@@deriving enumerate, sexp]

type pipe =
  | Start
  | Empty
  | NE
  | NS
  | NW
  | ES
  | EW
  | SW
[@@deriving equal, sexp]

let pipe_of_char = function
  | 'S' -> Start
  | '.' -> Empty
  | 'L' -> NE
  | '|' -> NS
  | 'J' -> NW
  | 'F' -> ES
  | '-' -> EW
  | '7' -> SW
  | _ -> raise_notrace @@ Invalid_argument "pipe_of_char"
;;

let step (x, y) = function
  | N -> x, y - 1
  | E -> x + 1, y
  | S -> x, y + 1
  | W -> x - 1, y
;;

let loop start grid =
  let next_direction pipe direction =
    match pipe, direction with
    | Start, _ -> assert false
    | NE, W | NS, N | NW, E -> N
    | NE, S | ES, N | EW, E -> E
    | NS, S | ES, W | SW, E -> S
    | NW, S | EW, W | SW, N -> W
    | _ -> raise_notrace Exit
  in
  let unfold (pos, direction) =
    let pipe = grid.^(pos) in
    if [%equal: pipe] Start pipe
    then None
    else (
      let next_direction = next_direction pipe direction in
      let next_pos = step pos next_direction in
      Some (pos, (next_pos, next_direction)))
  in
  [%all: direction]
  |> Iter.of_list
  |> Iter.map ~f:(fun d -> step start d, d)
  |> Iter.map ~f:(fun start ->
    Or_error.try_with (fun () ->
      Iter.unfoldr unfold start |> Iter.pair_with_idx |> Iter.to_rev_list))
  |> Iter.keep_ok
  |> Iter.head_exn
;;

let parser =
  let open Angstrom in
  let%map start, grid = grid_start ~equal:[%equal: pipe] pipe_of_char Start in
  start, grid, loop start grid
;;

let part1 (_, _, loop) =
  let length = List.hd_exn loop |> fst in
  let middle = length / 2 in
  List.find_exn loop ~f:(Tuple2.get1 >> equal middle) |> fst |> ( + ) 1
;;

let part2 (start, grid, loop) =
  let loop_pipes =
    Set.of_list (module Tuple.Comparator (Core.Int) (Core.Int)) (loop |> List.map ~f:snd)
  in
  let start_pipe =
    let connected_to_north =
      let north = step start N in
      match grid.?(north) with
      | Some (NS | ES | SW) -> Set.mem loop_pipes north
      | _ -> false
    in
    let connected_to_south =
      let south = step start S in
      match grid.?(south) with
      | Some (NS | NE | NW) -> Set.mem loop_pipes south
      | _ -> false
    in
    let connected_to_east =
      let east = step start E in
      match grid.?(east) with
      | Some (NW | EW | SW) -> Set.mem loop_pipes east
      | _ -> false
    in
    match connected_to_north, connected_to_east, connected_to_south with
    | true, true, true | false, false, false -> assert false
    | true, true, false -> NE
    | true, false, true -> NS
    | false, true, true -> ES
    | true, false, false -> NW
    | false, true, false -> EW
    | false, false, true -> SW
  in
  let pipe pos =
    if not @@ [%equal: int * int] start pos then grid.^(pos) else start_pipe
  in
  let open Iter in
  fold
    (0 -- (width grid - 1))
    ~init:0
    ~f:(fun count x ->
      fold
        (0 -- (height grid - 1))
        ~init:(count, true, None)
        ~f:(fun (count, is_outside, seen) y ->
          let on_pipe =
            Core.Set.mem loop_pipes (x, y) || [%equal: int * int] start (x, y)
          in
          let next_is_outside, next_seen =
            if on_pipe
            then (
              match pipe (x, y), seen with
              | EW, _ -> not is_outside, None
              | NS, seen -> is_outside, seen
              | NE, Some `E | NW, Some `W -> is_outside, None
              | NE, Some `W | NW, Some `E -> not is_outside, None
              | ES, None -> is_outside, Some `E
              | SW, None -> is_outside, Some `W
              | _, _ -> assert false)
            else is_outside, None
          in
          if (not on_pipe) && not is_outside
          then count + 1, next_is_outside, next_seen
          else count, next_is_outside, next_seen)
      |> Tuple3.get1)
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 8 |}];
  printf "%d" @@ part2 @@ parse_string parser Sample2.sample2;
  [%expect {| 10 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 6942 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 297 |}]
;;
