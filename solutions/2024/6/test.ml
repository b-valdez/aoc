let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let parsed = parse_file "sample.blob" parser in
  let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 41 |}) in
  let part2 () = xprintf "%d" (part2 parsed) ~expect:(fun () -> {%expect| 6 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let parsed = parse_file "input.blob" parser in
  let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 4778 |}) in
  let part2 () = xprintf "%d" (part2 parsed) ~expect:(fun () -> {%expect| 1618 |}) in
  fork_join_array [| part1; part2 |]
;;
