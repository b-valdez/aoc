let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun[@warning "-26"] _ ->
  let sample () =
    let available, k = parse_file_prefix "sample.blob" parse_available in
    let[@warning "-8"] [| seq1; seq2 |] =
      continue_parsing_into_iter k parser |> tee_iter ~n:2
    in
    let part1 () =
      xprintf "%d" (part1 available seq1) ~expect:(fun () -> {%expect| 6 |})
    in
    let part2 () =
      xprintf "%d" (part2 available seq2) ~expect:(fun () -> {%expect| 16 |})
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let available, k = parse_file_prefix "input.blob" parse_available in
    let[@warning "-8"] [| seq1; seq2 |] =
      continue_parsing_into_iter k parser |> tee_iter ~n:2
    in
    let part1 () =
      xprintf "%d" (part1 available seq1) ~expect:(fun () -> {%expect| 287 |})
    in
    let part2 () =
      xprintf "%d" (part2 available seq2) ~expect:(fun () -> {%expect| 571894474468161 |})
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
