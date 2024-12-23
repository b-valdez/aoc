let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let cursor = parse_file_into_stream "sample.blob" parser |> tap in
  let part1 () = xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 480 |}) in
  let part2 () =
    xprintf "%d" (part2 cursor) ~expect:(fun () -> {%expect| 875318608908 |})
  in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let cursor = parse_file_into_stream "input.blob" parser |> tap in
  let part1 () = xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 33921 |}) in
  let part2 () =
    xprintf "%d" (part2 cursor) ~expect:(fun () -> {%expect| 82261957837868 |})
  in
  fork_join_array [| part1; part2 |]
;;
