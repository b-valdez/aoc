let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun [@warning "-26"] _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 7 |}) in
    let part2 () =
      xprintf "%s" (part2 parsed) ~expect:(fun () -> {%expect| co,de,ka,ta |})
    in
    fork_join_array [| part1; part2 |]
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 1314 |}) in
    let part2 () =
      xprintf "%s" (part2 parsed) ~expect:(fun () ->
        {%expect| bg,bu,ce,ga,hw,jw,nf,nt,ox,tj,uu,vk,wp |})
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
