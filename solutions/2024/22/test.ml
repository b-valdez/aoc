let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run ~timeout:20.
  @@ fun [@warning "-26"] _ ->
  let sample () =
    let cursor = parse_file_into_stream "sample.blob" parser |> tap in
    let part1 () =
      xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 37327623 |})
    in
    let part2 () =
      xprint_s
        ([%sexp_of: Int4.t * int] @@ part2 cursor)
        ~expect:(fun () -> {%expect| ((-9 9 -1 0) 24) |})
    in
    fork_join_array [| part1; part2 |]
  in
  let sample2 () =
    let cursor = parse_file_into_stream "sample2.blob" parser |> tap in
    let part1 () =
      xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 37990510 |})
    in
    let part2 () =
      xprint_s
        ([%sexp_of: Int4.t * int] @@ part2 cursor)
        ~expect:(fun () -> {%expect| ((-2 1 -1 3) 23) |})
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let cursor = parse_file_into_stream "input.blob" parser |> tap in
    let part1 () =
      xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 18525593556 |})
    in
    let part2 () =
      xprintf "%d" (snd @@ part2 cursor) ~expect:(fun () -> {%expect| 2089 |})
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; sample2; input |]
;;
