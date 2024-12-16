let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run ~timeout:20.
  @@ fun [@warning "-26"] _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 7036 |}) in
    let part2 () =
      xprintf "%d" (part2 parsed) ~expect:(fun () -> [%expect.unreachable])
    in
    fork_join_array [| part1 |]
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 143564 |}) in
    let part2 () =
      xprintf "%d" (part2 parsed) ~expect:(fun () -> [%expect.unreachable])
    in
    fork_join_array [| part1 |]
  in
  fork_join_array [| sample; input |]
;;
