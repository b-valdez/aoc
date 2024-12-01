open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  blocks tf_grid_lazy
;;

let find_reflection_row grid =
  let open Iter in
  1 -- (height grid - 1)
  |> filter ~f:(fun y ->
    1 -- Int.min y (height grid - y)
    |> for_all ~f:(fun dy -> [%equal: bool array] grid.(y + dy - 1) grid.(y - dy)))
  |> head
;;

let find_reflection_col grid =
  let open Iter in
  1 -- (width grid - 1)
  |> filter ~f:(fun x ->
    1 -- Int.min x (width grid - x)
    |> for_all ~f:(fun dx ->
      0 -- (height grid - 1)
      |> for_all ~f:(fun y -> [%equal: bool] grid.^(x + dx - 1, y) grid.^(x - dx, y))))
  |> head
;;

let part1 =
  List.sum
    (module Int)
    ~f:(fun grid ->
      match find_reflection_row grid, lazy (find_reflection_col grid) with
      | Some row, _ -> row * 100
      | None, (lazy (Some col)) -> col
      | None, (lazy None) -> assert false)
;;

let find_smudged_reflection_row grid =
  let open Iter in
  1 -- (height grid - 1)
  |> filter ~f:(fun y ->
    let smudges =
      1 -- Int.min y (height grid - y)
      |> fold ~init:0 ~f:(fun smudges dy ->
        Array.fold2_exn
          ~init:smudges
          grid.(y + dy - 1)
          grid.(y - dy)
          ~f:(fun smudges a b -> if [%equal: bool] a b then smudges else smudges + 1))
    in
    1 = smudges)
  |> head
;;

let find_smudged_reflection_col grid =
  let open Iter in
  1 -- (width grid - 1)
  |> filter ~f:(fun x ->
    let smudges =
      1 -- Int.min x (width grid - x)
      |> fold ~init:0 ~f:(fun smudges dx ->
        0 -- (height grid - 1)
        |> fold ~init:smudges ~f:(fun smudges y ->
          if [%equal: bool] grid.^(x + dx - 1, y) grid.^(x - dx, y)
          then smudges
          else smudges + 1))
    in
    1 = smudges)
  |> head
;;

let part2 =
  List.sum
    (module Int)
    ~f:(fun grid ->
      match find_smudged_reflection_row grid, lazy (find_smudged_reflection_col grid) with
      | Some row, _ -> row * 100
      | None, (lazy (Some col)) -> col
      | None, (lazy None) -> assert false)
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  {%expect| 405 |};
  printf "%d" @@ part2 parsed;
  {%expect| 400 |}
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  {%expect| 27300 |};
  printf "%d" @@ part2 parsed;
  {%expect| 29276 |}
;;
