let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun [@warning "-26"] _ ->
  let sample () =
    let seq = parse_file_into_iter "sample.blob" parser in
    xprintf "%d" (part1 seq) ~expect:(fun () -> {%expect| 3 |})
  in
  let input () =
    let seq = parse_file_into_iter "input.blob" parser in
    xprintf "%d" (part1 seq) ~expect:(fun () -> {%expect| 3065 |})
  in
  fork_join_array [| sample; input |]
;;
