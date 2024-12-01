let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let iter, stream =
    parse_file_into_iter "sample.blob" parser
    |> Parallel_iter.iter_into_iter_and_stream ~padded:true
  in
  let part1 () = xprintf "%d" (part1 (tap stream)) ~expect:(fun () -> {%expect| 11 |}) in
  let part2 () = xprintf "%d" (part2 iter) ~expect:(fun () -> {%expect| 31 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let iter, stream =
    parse_file_into_iter "input.blob" parser
    |> Parallel_iter.iter_into_iter_and_stream ~padded:true
  in
  let part1 () =
    xprintf "%d" (part1 (tap stream)) ~expect:(fun () -> {%expect| 2367773 |})
  in
  let part2 () = xprintf "%d" (part2 iter) ~expect:(fun () -> {%expect| 21271939 |}) in
  fork_join_array [| part1; part2 |]
;;
