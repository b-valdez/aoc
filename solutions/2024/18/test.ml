let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun[@warning "-26"] _ ->
  let sample () =
    let[@warning "-8"] [| seq1; seq2 |] =
      parse_file_into_iter "sample.blob" parser |> tee_iter ~n:2
    in
    let part1 () = xprintf "%d" (part1 12 6 seq1) ~expect:(fun () -> {%expect| 22 |}) in
    let part2 () =
      xprint_s
        ([%sexp_of: Grid.Position.t] @@ part2 6 seq2)
        ~expect:(fun () -> {%expect| (6 1) |})
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let[@warning "-8"] [| seq1; seq2 |] =
      parse_file_into_iter "input.blob" parser |> tee_iter ~n:2
    in
    let part1 () =
      xprintf "%d" (part1 1024 70 seq1) ~expect:(fun () -> {%expect| 382 |})
    in
    let part2 () =
      xprint_s
        ([%sexp_of: Grid.Position.t] @@ part2 70 seq2)
        ~expect:(fun () -> {%expect| (6 36) |})
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
