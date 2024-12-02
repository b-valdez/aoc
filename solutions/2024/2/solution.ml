open! Aoc_std

let parser =
  let open Angstrom in
  sep_by1 (string " ") nat <* (end_of_line <|> end_of_input)
;;

let list_diffs = function
  | [] -> assert false
  | first :: list ->
    List.fold_map ~init:first ~f:(fun last current -> current, last - current) list |> snd
;;

let part1 =
  Parallel_iter.of_cursor
  >> Parallel_iter.filter_count ~f:(fun list ->
    let diffs = list_diffs list in
    List.for_all diffs ~f:(function
      | 1 | 2 | 3 -> true
      | _ -> false)
    || List.for_all diffs ~f:(function
      | -1 | -2 | -3 -> true
      | _ -> false))
;;

(* TODO de-duplication of effort, try to minimize the number each list is iterated on *)

(* precondition: has a problem *)
let dampen_problem list diffs =
  let aux ( ~+/- ) =
    let[@warning "-8"] (Some (problem_index, _)) =
      List.findi diffs ~f:(fun _ level -> ~+/-level < 1 || ~+/-level > 3)
    in
    (let list = List.filteri list ~f:(fun index _ -> index <> problem_index) in
     let diffs = list_diffs list in
     List.for_all diffs ~f:(fun level -> ~+/-level > 0 && 4 > ~+/-level))
    ||
    let list = List.filteri list ~f:(fun index _ -> index <> problem_index + 1) in
    let diffs = list_diffs list in
    List.for_all diffs ~f:(fun level -> ~+/-level > 0 && 4 > ~+/-level)
  in
  aux ( ~- ) || aux ( ~+ )
;;

let part2 =
  Parallel_iter.of_cursor
  >> Parallel_iter.filter_count ~f:(fun list ->
    let diffs = list_diffs list in
    List.for_all diffs ~f:(function
      | 1 | 2 | 3 -> true
      | _ -> false)
    || List.for_all diffs ~f:(function
      | -1 | -2 | -3 -> true
      | _ -> false)
    || dampen_problem list diffs)
;;
