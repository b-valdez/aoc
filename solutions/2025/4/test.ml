let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let sample () =
    let grid = parse_file "sample.blob" parser in
    let part1 () = xprintf "%d" (part1 grid) ~expect:(fun () -> [%expect {| 13 |}]) in
    let part2 () = xprintf "%d" (part2 grid) ~expect:(fun () -> [%expect {| 43 |}]) in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let grid = parse_file "input.blob" parser in
    let part1 () = xprintf "%d" (part1 grid) ~expect:(fun () -> [%expect {| 1486 |}]) in
    let part2 () = xprintf "%d" (part2 grid) ~expect:(fun () -> [%expect {| 9024 |}]) in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
