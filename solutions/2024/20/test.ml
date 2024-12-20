let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun [@warning "-26"] _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let honest_path = honest_path parsed in
    let part1_20 () =
      xprintf "%d" (part1 ~min_diff:12 parsed honest_path) ~expect:(fun () ->
        {%expect| 8 |})
    in
    let part1_21 () =
      xprintf "%d" (part1 ~min_diff:13 parsed honest_path) ~expect:(fun () ->
        {%expect| 5 |})
    in
    let part2_50 () =
      xprintf "%d" (part2 ~min_diff:50 parsed honest_path) ~expect:(fun () ->
        {%expect| 285 |})
    in
    let part2_51 () =
      xprintf "%d" (part2 ~min_diff:51 parsed honest_path) ~expect:(fun () ->
        {%expect| 253 |})
    in
    fork_join_array [| part1_20; part1_21; part2_50; part2_51 |]
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let honest_path = honest_path parsed in
    let part1 () =
      xprintf "%d" (part1 parsed honest_path) ~expect:(fun () -> {%expect| 1343 |})
    in
    let part2 () =
      xprintf "%d" (part2 parsed honest_path) ~expect:(fun () -> {%expect| 982891 |})
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
