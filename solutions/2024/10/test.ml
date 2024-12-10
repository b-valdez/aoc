let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let grid = parse_file "sample.blob" parser in
  let part1 () = xprintf "%d" (part1 grid) ~expect:(fun () -> {%expect| 36 |}) in
  let part2 () = xprintf "%d" (part2 grid) ~expect:(fun () -> {%expect| 81 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let grid = parse_file "input.blob" parser in
  let part1 () = xprintf "%d" (part1 grid) ~expect:(fun () -> {%expect| 557 |}) in
  let part2 () = xprintf "%d" (part2 grid) ~expect:(fun () -> {%expect| 1062 |}) in
  fork_join_array [| part1; part2 |]
;;
