open! Aoc_std

let parser =
  let open Angstrom in
  pair ~sep:(string "   " *> return ()) nat <* (end_of_line <|> end_of_input)
;;

let part1 cursor =
  let list f =
    cursor
    |> Parallel_iter.of_cursor
    |> Parallel_iter.map ~f
    |> Parallel_iter.sort ~padded:true ~compare
  in
  Array.zip_exn (list fst) (list snd)
  |> Array.sum (module Int) ~f:(fun (a, b) -> abs (a - b))
;;

let part2 iter =
  Iter.fold iter ~init:Int.Map.empty ~f:(fun seen (left, right) ->
    let seen =
      Map.update seen left ~f:(function
        | None -> 1, 0
        | Some (left_count, right_count) -> left_count + 1, right_count)
    in
    let seen =
      Map.update seen right ~f:(function
        | None -> 0, 1
        | Some (left_count, right_count) -> left_count, right_count + 1)
    in
    seen)
  |> Map.sumi (module Int) ~f:(fun ~key ~data:(left, right) -> key * left * right)
;;
