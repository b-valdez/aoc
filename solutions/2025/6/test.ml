let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let part1 () =
      xprintf "%d" (part1 parsed) ~expect:(fun () -> [%expect {| 4277556 |}])
    in
    let part2 () =
      xprintf "%d" (part2 parsed) ~expect:(fun () -> [%expect {| 3263827 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let part1 () =
      xprintf "%d" (part1 parsed) ~expect:(fun () -> [%expect {| 4878670269096 |}])
    in
    let part2 () =
      xprintf "%d" (part2 parsed) ~expect:(fun () -> [%expect {| 8674740488592 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
