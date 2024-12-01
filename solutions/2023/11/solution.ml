open! Aoc_std

let parser =
  let open Angstrom in
  let%map galaxies = sparse_tf_grid in
  let open Iter in
  let empty_rows =
    0 -- (Set.iter galaxies |> from_labelled_iter |> map ~f:snd |> max_exn ~lt:( < ))
    |> filter ~f:(fun y -> Set.for_all galaxies ~f:(fun (_, y') -> y <> y'))
    |> to_array
    |> Int.Set.of_sorted_array_unchecked
  in
  let empty_cols =
    0 -- (Set.iter galaxies |> from_labelled_iter |> map ~f:fst |> max_exn ~lt:( < ))
    |> filter ~f:(fun x -> Set.for_all galaxies ~f:(fun (x', _) -> x <> x'))
    |> to_array
    |> Int.Set.of_sorted_array_unchecked
  in
  galaxies, empty_rows, empty_cols
;;

let distance_sum ~distance (galaxies, empty_rows, empty_cols) =
  let open Iter in
  let iterate_galaxies = Set.iter galaxies |> from_labelled_iter in
  let galaxy_pairs =
    iterate_galaxies
    |> flat_map ~f:(fun g ->
      Set.split_lt_ge galaxies g
      |> snd
      |> Set.iter
      |> from_labelled_iter
      >|= Tuple2.create g)
  in
  fold galaxy_pairs ~init:0 ~f:(fun sum ((x, y), (x', y')) ->
    let y, y' = min_max ~compare y y' in
    sum
    + x'
    - x
    + ((distance - 1) * Set.count empty_cols ~f:(Int.between ~low:x ~high:x'))
    + y'
    - y
    + ((distance - 1) * Set.count empty_rows ~f:(Int.between ~low:y ~high:y')))
;;

let part1 = distance_sum ~distance:2
let part2 = distance_sum ~distance:1_000_000

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  {%expect| 374 |};
  printf "%d" @@ distance_sum ~distance:10 parsed;
  {%expect| 1030 |};
  printf "%d" @@ distance_sum ~distance:100 parsed;
  {%expect| 8410 |}
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  {%expect| 9445168 |};
  printf "%d" @@ part2 parsed;
  {%expect| 742305960572 |}
;;
