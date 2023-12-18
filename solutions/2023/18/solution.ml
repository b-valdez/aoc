open! Aoc_std

let direction_of_char = function
  | 'U' -> `N
  | 'R' -> `E
  | 'D' -> `S
  | 'L' -> `W
  | _ -> assert false
;;

let direction_of_digit = function
  | 3 -> `N
  | 0 -> `E
  | 1 -> `S
  | 2 -> `W
  | _ -> assert false
;;

let parser =
  let open Angstrom in
  let line =
    let%mapn direction = any_char >>| direction_of_char
    and steps = space *> nat
    and steps2 = string " (#" *> take 5
    and direction2 = digit >>| direction_of_digit <* char ')' in
    let steps2 = Int.Hex.of_string ("0x" ^ steps2) in
    (direction, steps), (direction2, steps2)
  in
  lines line
;;

let trench =
  let set = Grid.Position.Set.empty in
  List.fold
    ~init:(set, (0, 0))
    ~f:(fun acc (direction, steps) ->
      let open Grid.Direction in
      Fn.apply_n_times
        ~n:steps
        (fun (trench, pos) ->
          let pos = step pos direction in
          let colored = Set.add trench pos in
          colored, pos)
        acc)
  >> fst
;;

let lagoon_size trench =
  let min_x, max_x, min_y, max_y =
    Set.fold trench ~init:(0, 0, 0, 0) ~f:(fun (min_x, max_x, min_y, max_y) (x, y) ->
      let min_x, max_x =
        if x < min_x then x, max_x else if x > max_x then min_x, x else min_x, max_x
      in
      let min_y, max_y =
        if y < min_y then y, max_y else if y > max_y then min_y, y else min_y, max_y
      in
      min_x, max_x, min_y, max_y)
  in
  let open Iter in
  fold (min_x -- max_x) ~init:0 ~f:(fun count x ->
    fold (min_y -- max_y) ~init:(count, None, false) ~f:(fun (count, seen, is_inside) y ->
      let in_trench = Set.mem trench (x, y) in
      let next_seen, next_is_inside =
        match seen, in_trench with
        | None, false -> None, is_inside
        | Some _, false -> assert false, is_inside
        | None, true ->
          let adjacencies =
            List.filter [ `E; `W ] ~f:(fun side ->
              Set.mem trench (Grid.Direction.step (x, y) side))
          in
          (match adjacencies with
           | [ _; _ ] -> None, not is_inside
           | [ direction ] -> Some direction, is_inside
           | _ -> assert false)
        | Some direction, true ->
          let adjacent =
            List.find [ `E; `W ] ~f:(fun side ->
              Set.mem trench (Grid.Direction.step (x, y) side))
          in
          (match adjacent with
           | Some direction' when [%equal: Grid.Direction.t] direction direction' ->
             None, is_inside
           | Some _ -> None, not is_inside
           | None -> Some direction, is_inside)
      in
      if in_trench || is_some next_seen || is_inside
      then count + 1, next_seen, next_is_inside
      else count, next_seen, next_is_inside)
    |> Tuple3.get1)
;;

let part1 = List.map ~f:fst >> trench >> lagoon_size

(** this is much too inefficient for even the sample: Span rectangles! *)
let part2 _ = failwith "TODO"

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 62 |}];
  printf "%d" @@ part2 parsed;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure TODO)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Aoc_2023_18__Solution.part2 in file "solutions/2023/18/solution.ml" (inlined), line 96, characters 14-29
  Called from Aoc_2023_18__Solution.(fun) in file "solutions/2023/18/solution.ml", line 102, characters 17-29
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 35991 |}];
  printf "%d" @@ part2 parsed;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure TODO)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Aoc_2023_18__Solution.part2 in file "solutions/2023/18/solution.ml" (inlined), line 96, characters 14-29
  Called from Aoc_2023_18__Solution.(fun) in file "solutions/2023/18/solution.ml", line 121, characters 17-29
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;
