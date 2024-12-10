open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  grid Char.get_digit_exn
;;

let part1 map =
  let rec find_trails expected pos =
    match map.?(pos) with
    | Some 9 when expected = 9 -> Iter.singleton pos
    | Some expected' when expected = expected' ->
      Direction.all
      |> List.iter
      |> Iter.from_labelled_iter
      |> Iter.flat_map ~f:(fun direction ->
        find_trails (expected + 1) (Direction.step pos direction))
    | _ -> Iter.empty
  in
  iteri map
  |> Parallel_iter.from_labelled_iter2
  |> Parallel_iter.map ~f:(fun (pos, _) ->
    find_trails 0 pos |> Iter.sort_uniq ~cmp:Position.compare |> Iter.length)
  |> Parallel_iter.sum
;;

let part2 map =
  let rec find_trails expected pos =
    match map.?(pos) with
    | Some 9 when expected = 9 -> 1
    | Some expected' when expected = expected' ->
      Direction.all
      |> List.iter
      |> Iter.from_labelled_iter
      |> Iter.map ~f:(fun direction ->
        find_trails (expected + 1) (Direction.step pos direction))
      |> Iter.sum
    | _ -> 0
  in
  iteri map
  |> Parallel_iter.from_labelled_iter2
  |> Parallel_iter.map ~f:(fun (pos, _) -> find_trails 0 pos)
  |> Parallel_iter.sum
;;
