open! Aoc_std

let parser =
  let open Angstrom in
  let%bind winning =
    string "Card"
    *> spaces
    *> nat
    *> char ':'
    *> commit
    *> many_unique_till (module String) (take 3) (string " |" <* commit)
  in
  count_till (take 3 >>| Set.mem winning) (end_of_line <|> end_of_input) <* commit
;;

let part1 cursor =
  let open Parallel_iter in
  of_cursor cursor
  |> map ~f:(function
    | 0 -> 0
    | n -> 1 lsl (n - 1))
  |> sum
;;

let part2 seq =
  let[@tail_mod_cons] rec add_future_winnings so_far to_be_added times =
    if to_be_added = 0
    then so_far
    else (
      match so_far with
      | [] -> List.init to_be_added ~f:(Fn.const times)
      | hd :: tl ->
        (hd + times) :: (add_future_winnings [@tailcall]) tl (to_be_added - 1) times)
  in
  let f acc winnings =
    Moonpool.Fut.map acc ~f:(function
      | sum, [] -> sum + 1, add_future_winnings [] winnings 1
      | sum, hd :: tl -> sum + 1 + hd, add_future_winnings tl winnings (1 + hd))
  in
  Iter.fold seq ~init:(Moonpool.Fut.return (0, [])) ~f |> Moonpool.await |> fst
;;

let%expect_test "sample" =
  run
  @@ fun _ ->
  let seq, stream =
    parse_file_into_iter "sample.blob" parser |> Parallel_iter.iter_into_iter_and_stream
  in
  let cursor = tap stream in
  let part1 () = xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 13 |}) in
  let part2 () = xprintf "%d" (part2 seq) ~expect:(fun () -> {%expect| 30 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  run
  @@ fun _ ->
  let seq, stream =
    parse_file_into_iter "input.blob" parser |> Parallel_iter.iter_into_iter_and_stream
  in
  let cursor = tap stream in
  let part1 () = xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 21213 |}) in
  let part2 () = xprintf "%d" (part2 seq) ~expect:(fun () -> {%expect| 8549735 |}) in
  fork_join_array [| part1; part2 |]
;;
