let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let sample () =
    let[@warning "-8"] [| iter1; iter2 |] =
      parse_file_into_iter "sample.blob" parser |> tee_iter ~n:2
    in
    let part1 () = xprintf "%d" (part1 iter1) ~expect:(fun () -> [%expect {| 3 |}]) in
    let part2 () = xprintf "%d" (part2 iter2) ~expect:(fun () -> [%expect {| 6 |}]) in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let[@warning "-8"] [| iter1; iter2 |] =
      parse_file_into_iter "input.blob" parser |> tee_iter ~n:2
    in
    let part1 () = xprintf "%d" (part1 iter1) ~expect:(fun () -> [%expect {| 997 |}]) in
    let part2 () = xprintf "%d" (part2 iter2) ~expect:(fun () -> [%expect {| 5978 |}]) in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
