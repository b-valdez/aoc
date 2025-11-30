let%expect_test ("test" [@tags "disabled"] (* TODO *)) =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun[@warning "-26"] _ ->
  let sample () =
    let cursor = parse_file_into_stream "sample.blob" parser |> tap in
    let part1 () =
      xprintf "%d" (solve 2 cursor) ~expect:(fun () -> {%expect| 126384 |})
    in
    let part2 () =
      xprintf "%d" (solve 25 cursor) ~expect:(fun () -> {%expect| 154115708116294 |})
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let cursor = parse_file_into_stream "input.blob" parser |> tap in
    let part1 () =
      xprintf "%d" (solve 2 cursor) ~expect:(fun () -> {%expect| 184180 |})
    in
    let part2 () =
      xprintf "%d" (solve 25 cursor) ~expect:(fun () -> {%expect| 231309103124520 |})
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
