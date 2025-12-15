let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun[@warning "-26"] _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let part1 () =
      xprintf "%d" (part1 Sample parsed) ~expect:(fun () -> [%expect {| 40 |}])
    in
    let part2 () =
      xprintf "%d" (part2 parsed) ~expect:(fun () -> [%expect {| 25272 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let part1 () =
      xprintf "%d" (part1 Input parsed) ~expect:(fun () -> [%expect {| 50568 |}])
    in
    let part2 () =
      xprintf "%d" (part2 parsed) ~expect:(fun () -> [%expect {| 36045012 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
