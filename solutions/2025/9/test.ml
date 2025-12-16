let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> [%expect {| 50 |}]) in
    let part2 () = xprintf "%d" (part2 parsed) ~expect:(fun () -> [%expect {| 24 |}]) in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let part1 () =
      xprintf "%d" (part1 parsed) ~expect:(fun () -> [%expect {| 4740155680 |}])
    in
    let part2 () =
      xprintf "%d" (part2 parsed) ~expect:(fun () -> [%expect {| 1543501936 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
