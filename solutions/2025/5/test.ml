let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let sample () =
    let fresh, k = parse_file_prefix "sample.blob" parse_prefix in
    let iter1 = continue_parsing_into_iter k parser in
    let part1 () =
      xprintf "%d" (part1 fresh iter1) ~expect:(fun () -> [%expect {| 3 |}])
    in
    let part2 () = xprintf "%d" (part2 fresh) ~expect:(fun () -> [%expect {| 14 |}]) in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let fresh, k = parse_file_prefix "input.blob" parse_prefix in
    let iter1 = continue_parsing_into_iter k parser in
    let part1 () =
      xprintf "%d" (part1 fresh iter1) ~expect:(fun () -> [%expect {| 798 |}])
    in
    let part2 () =
      xprintf "%d" (part2 fresh) ~expect:(fun () -> [%expect {| 366181852921027 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
