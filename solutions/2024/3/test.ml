let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let part1 () =
    let iter = parse_file_into_iter "sample.blob" parser |> Parallel_iter.from_iter in
    xprintf "%d" (part1 iter) ~expect:(fun () -> {%expect| 161 |})
  in
  let part2 () =
    let iter = parse_file_into_iter "sample2.blob" parser in
    xprintf "%d" (part2 iter) ~expect:(fun () -> {%expect| 48 |})
  in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let iter, stream =
    parse_file_into_iter "input.blob" parser |> Parallel_iter.iter_into_iter_and_stream
  in
  let part1 () =
    xprintf
      "%d"
      (part1 (tap stream |> Parallel_iter.of_cursor))
      ~expect:(fun () -> {%expect| 174960292 |})
  in
  let part2 () = xprintf "%d" (part2 iter) ~expect:(fun () -> {%expect| 56275602 |}) in
  fork_join_array [| part1; part2 |]
;;
