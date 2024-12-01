open! Aoc_std

(* TODO split into part1 and part2 *)
let extrapolate list =
  let rec extrapolate sgn acc_hd acc_tl seq =
    let hd = List.hd_exn seq in
    let (all0, last), diffs =
      List.fold_map
        (List.tl_exn seq)
        ~init:(hd = 0, hd)
        ~f:(fun (all0, last) el -> (all0 && el = 0, el), el - last)
    in
    if all0
    then acc_hd, acc_tl
    else (extrapolate [@tailcall]) ~-sgn (acc_hd + (sgn * hd)) (last + acc_tl) diffs
  in
  (extrapolate [@tailcall]) 1 0 0 list
;;

let parser =
  let open Angstrom in
  sep_by1 (char ' ' <* commit) int
  >>| extrapolate
  <* (end_of_line <|> end_of_input)
  <* commit
;;

let part1 = Parallel_iter.of_cursor >> Parallel_iter.map ~f:snd >> Parallel_iter.sum
let part2 = Parallel_iter.of_cursor >> Parallel_iter.map ~f:fst >> Parallel_iter.sum

let%expect_test "sample" =
  run
  @@ fun _ ->
  let cursor = parse_file_into_stream "sample.blob" parser |> tap in
  let part1 () = xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 114 |}) in
  let part2 () = xprintf "%d" (part2 cursor) ~expect:(fun () -> {%expect| 2 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  run
  @@ fun _ ->
  let cursor = parse_file_into_stream "input.blob" parser |> tap in
  let part1 () =
    xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 1584748274 |})
  in
  let part2 () = xprintf "%d" (part2 cursor) ~expect:(fun () -> {%expect| 1026 |}) in
  fork_join_array [| part1; part2 |]
;;
