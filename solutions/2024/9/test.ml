let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let seq = parse_file_into_iter "sample.blob" parser in
  let part1 () = xprintf "%d" (part1 seq) ~expect:(fun () -> {%expect| 1928 |}) in
  let part2 () = xprintf "%d" (part2 seq) ~expect:(fun () -> {%expect| 2858 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let seq = parse_file_into_iter "input.blob" parser in
  let part1 () =
    xprintf "%d" (part1 seq) ~expect:(fun () -> {%expect| 6471961544878 |})
  in
  let part2 () =
    xprintf "%d" (part2 seq) ~expect:(fun () -> {%expect| 6511178035564 |})
  in
  fork_join_array [| part1; part2 |]
;;
