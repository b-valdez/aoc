let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun[@warning "-8"] _ ->
  let sample () =
    let start, k = parse_file_prefix "sample.blob" prefix_parser in
    let [| iter1; iter2 |] = continue_parsing_into_iter k parser |> tee_iter ~n:2 in
    let part1 () =
      xprintf "%d" (part1 start iter1) ~expect:(fun () -> [%expect {| 21 |}])
    in
    let part2 () =
      xprintf "%d" (part2 start iter2) ~expect:(fun () -> [%expect {| 40 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let start, k = parse_file_prefix "input.blob" prefix_parser in
    let [| iter1; iter2 |] = continue_parsing_into_iter k parser |> tee_iter ~n:2 in
    let part1 () =
      xprintf "%d" (part1 start iter1) ~expect:(fun () -> [%expect {| 1651 |}])
    in
    let part2 () =
      xprintf "%d" (part2 start iter2) ~expect:(fun () -> [%expect {| 108924003331749 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
