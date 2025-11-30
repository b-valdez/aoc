open! Aoc_std

let parser =
  let open Angstrom in
  sep_by1 (string " ") nat <* (end_of_line <|> end_of_input)
;;

let list_diffs = function
  | [] -> assert false
  | first :: list ->
    Iter.of_list list
    |> Iter.fold_map ~init:first ~f:(fun last current -> current, last - current)
;;

type state =
  | Initial
  | Last of int
  | Dampening_first of int
  | Dampening of int * int
  | Already_dampened

let dampen_problem iter ( ~+- ) =
  match
    Iter.fold iter ~init:Initial ~f:(fun state diff ->
      match state with
      | Initial ->
        if Int.between ~low:1 ~high:3 ~+-diff then Last diff else Dampening_first diff
      | Last last ->
        if Int.between ~low:1 ~high:3 ~+-diff then Last diff else Dampening (last, diff)
      | Dampening_first first ->
        if Int.between ~low:1 ~high:3 ~+-(first + diff)
        then Already_dampened
        else if Int.between ~low:1 ~high:3 ~+-diff
        then Already_dampened
        else raise_notrace Exit
      | Dampening (second_to_last, last) ->
        if
          Int.between ~low:1 ~high:3 ~+-(second_to_last + last)
          && Int.between ~low:1 ~high:3 ~+-diff
        then Already_dampened
        else if
          Int.between ~low:1 ~high:3 ~+-(last + diff)
          && Int.between ~low:1 ~high:3 ~+-second_to_last
        then Already_dampened
        else raise_notrace Exit
      | Already_dampened ->
        if Int.between ~low:1 ~high:3 ~+-diff
        then Already_dampened
        else raise_notrace Exit)
  with
  | exception Exit -> false
  | Initial | Last _ | Already_dampened | Dampening_first _ | Dampening _ -> true
;;

let common =
  Parallel_iter.from_iter
  >> Parallel_iter.map ~f:(fun list ->
    let diffs = list_diffs list in
    let[@warning "-8"] [| iter1; iter2; iter3; iter4 |] = tee_iter diffs ~n:4 in
    let[@warning "-8"] (Some current_runner) = Moonpool.get_current_runner () in
    let increasing =
      Moonpool.Fut.spawn ~on:current_runner (fun () ->
        Iter.for_all iter1 ~f:(Int.between ~low:1 ~high:3))
    in
    let decreasing =
      Moonpool.Fut.spawn ~on:current_runner (fun () ->
        Iter.for_all iter2 ~f:(Int.between ~low:(-3) ~high:(-1)))
    in
    let dampened_increasing =
      Moonpool.Fut.spawn ~on:current_runner (fun () -> dampen_problem iter3 ( ~+ ))
    in
    let dampened_decreasing =
      Moonpool.Fut.spawn ~on:current_runner (fun () -> dampen_problem iter4 ( ~- ))
    in
    Moonpool.await
    @@
    let open Moonpool.Fut.Infix in
    let+ without_problem =
      let+ increasing = increasing
      and+ decreasing = decreasing in
      increasing || decreasing
    and+ dampened_problem =
      let+ dampened_increasing = dampened_increasing
      and+ dampened_decreasing = dampened_decreasing in
      dampened_increasing || dampened_decreasing
    in
    without_problem, dampened_problem)
  >> tee_parallel_iter ~n:2
  >> fun[@warning "-8"] [| a; b |] -> a, b
;;

let part1 seq = Parallel_iter.filter_count ~f:fst seq
let part2 seq = Parallel_iter.filter_count ~f:snd seq
