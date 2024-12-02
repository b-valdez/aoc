let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let seq = parse_file_into_iter "sample.blob" parser in
  let seq1, seq2 = common seq in
  let part1 () = xprintf "%d" (part1 seq1) ~expect:(fun () -> {%expect| 2 |}) in
  let part2 () = xprintf "%d" (part2 seq2) ~expect:(fun () -> {%expect| 4 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let seq = parse_file_into_iter "input.blob" parser in
  let seq1, seq2 = common seq in
  let part1 () = xprintf "%d" (part1 seq1) ~expect:(fun () -> {%expect| 472 |}) in
  let part2 () = xprintf "%d" (part2 seq2) ~expect:(fun () -> {%expect| 520 |}) in
  fork_join_array [| part1; part2 |]
;;
