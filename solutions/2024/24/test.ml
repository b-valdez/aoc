let%expect_test "test" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun[@warning "-26"] _ ->
  let sample () =
    let parsed = parse_file "sample.blob" parser in
    let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 2024 |}) in
    part1 ()
  in
  let input () =
    let parsed = parse_file "input.blob" parser in
    let part1 () =
      xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 49574189473968 |})
    in
    let part2 () =
      (* ckb,kbs,ksv,nbd,tqq,z06,z20,z39 *)
      let swaps = function
        | "z06" -> "ksv"
        | "ksv" -> "z06"
        | "kbs" -> "nbd"
        | "nbd" -> "kbs"
        | "z20" -> "tqq"
        | "tqq" -> "z20"
        | "ckb" -> "z39"
        | "z39" -> "ckb"
        | unchanged -> unchanged
      in
      part2 parsed swaps ~expect:(fun () -> {%expect| no incorrect digits |})
    in
    fork_join_array [| part1; part2 |]
  in
  fork_join_array [| sample; input |]
;;
