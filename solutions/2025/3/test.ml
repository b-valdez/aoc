let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun[@warning "-8"] _ ->
  let sample () =
    let [| iter1; iter2 |] = parse_file_into_iter "sample.blob" parser |> tee_iter ~n:2 in
    let part1 () = xprintf "%d" (part1 iter1) ~expect:(fun () -> [%expect {| 357 |}]) in
    let part2 () =
      xprintf "%d" (part2 iter2) ~expect:(fun () -> [%expect {| 3121910778619 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let [| iter1; iter2 |] = parse_file_into_iter "input.blob" parser |> tee_iter ~n:2 in
    let part1 () = xprintf "%d" (part1 iter1) ~expect:(fun () -> [%expect {| 17535 |}]) in
    let part2 () =
      xprintf "%d" (part2 iter2) ~expect:(fun () -> [%expect {| 173577199527257 |}])
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
