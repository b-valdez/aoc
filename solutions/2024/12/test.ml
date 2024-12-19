let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let grid = parse_file "sample.blob" parser in
  let stream = Stream.create () in
  let cursor = tap stream in
  let part1 () = xprintf "%d" (part1 stream grid) ~expect:(fun () -> {%expect| 1930 |}) in
  let part2 () = xprintf "%d" (part2 cursor grid) ~expect:(fun () -> {%expect| 1206 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let grid = parse_file "input.blob" parser in
  let stream = Stream.create () in
  let cursor = tap stream in
  let part1 () =
    xprintf "%d" (part1 stream grid) ~expect:(fun () -> {%expect| 1522850 |})
  in
  let part2 () =
    xprintf "%d" (part2 cursor grid) ~expect:(fun () -> {%expect| 953738 |})
  in
  fork_join_array [| part1; part2 |]
;;
