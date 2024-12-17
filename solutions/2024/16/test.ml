let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun [@warning "-26"] _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let processed = common parsed in
    xprintf "%d" (part1 processed) ~expect:(fun () -> {%expect| 7036 |});
    xprintf "%d" (part2 processed) ~expect:(fun () -> {%expect| 45 |})
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let processed = common parsed in
    xprintf "%d" (part1 processed) ~expect:(fun () -> {%expect| 143564 |});
    xprintf "%d" (part2 processed) ~expect:(fun () -> {%expect| 593 |})
  in
  fork_join_array [| sample; input |]
;;
