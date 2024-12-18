let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun [@warning "-26"] _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let part1 () =
      xprintf
        "%a"
        List.(printf_list (fun out -> fprintf out "%d"))
        (part1 parsed)
        ~expect:(fun () -> {%expect| 4,6,3,5,6,3,5,2,1,0 |})
    in
    part1 ()
  in
  let sample2 () =
    let parsed = parse_file "sample2.blob" parser in
    let part2 () = xprintf "%d" (part2 parsed) ~expect:(fun () -> {%expect| 117440 |}) in
    part2 ()
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let part1 () =
      xprintf
        "%a"
        List.(printf_list (fun out -> fprintf out "%d"))
        (part1 parsed)
        ~expect:(fun () -> {%expect| 7,3,0,5,7,1,4,0,5 |})
    in
    let part2 () =
      xprintf "%d" (part2 parsed) ~expect:(fun () -> {%expect| 202972175280682 |})
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; sample2; input |]
;;
