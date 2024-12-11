let%expect_test "sample" =
  let open! Aoc_std in
  let open Solution in
  run
  @@ fun _ ->
  let stones = parse_file "sample.blob" parser in
  let after_25 = blink 25 stones in
  printf "%d" (Map.sum (module Int) after_25 ~f:Fn.id);
  {%expect| 55312 |};
  printf "%d" (blink 50 after_25 |> Map.sum (module Int) ~f:Fn.id);
  {%expect| 65601038650482 |}
;;

let%expect_test "input" =
  let open! Aoc_std in
  let open Solution in
  run ~timeout:15.
  @@ fun _ ->
  let stones = parse_file "input.blob" parser in
  let after_25 = blink 25 stones in
  printf "%d" (Map.sum (module Int) after_25 ~f:Fn.id);
  {%expect| 211306 |};
  printf "%d" (blink 50 after_25 |> Map.sum (module Int) ~f:Fn.id);
  {%expect| 250783680217283 |}
;;
