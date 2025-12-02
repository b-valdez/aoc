let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun[@warning "-26"] _ ->
  let sample () =
    let[@warning "-8"] iter1 = parse_file_into_iter "sample.blob" parser in
    let part1 () =
      xprintf "%d" (part1 iter1) ~expect:(fun () -> [%expect {| 1227775554 |}])
    in
    let part2 () =
      xprintf "%d" (part2 Iter.empty) ~expect:(fun () -> [%expect.unreachable])
    in
    fork_join_array [| part1 |]
  in
  let input () =
    let[@warning "-8"] iter1 = parse_file_into_iter "input.blob" parser in
    let part1 () =
      xprintf "%d" (part1 iter1) ~expect:(fun () -> [%expect {| 8576933996 |}])
    in
    let part2 () =
      xprintf "%d" (part2 Iter.empty) ~expect:(fun () -> [%expect.unreachable])
    in
    fork_join_array [| part1 |]
  in
  fork_join_array [| sample; input |]
;;
