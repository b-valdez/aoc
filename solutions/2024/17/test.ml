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
    let specialize = function
      | 3 | 4 | 5 | 8 | 10 | 12 | 13 | 17 | 18 | 19 | 20 -> `Known false
      | 6 | 7 | 9 | 11 | 14 | 15 | 16 -> `Known true
      | _ -> `Unknown
    in
    let part2 () =
      let result =
        Mutex.protect xprint_mutex (fun () ->
          let result = part2 parsed ~specialize ~depth:7 ~digits_of_last_a:20 in
          [%expect {| |}];
          result)
      in
      xprint_s
        ([%sexp_of: (int * (int * int Blang.t) list) list]
         @@ part2 parsed ~specialize ~depth:7 ~digits_of_last_a:20)
        ~expect:(fun () ->
          {%expect|
            ((0 ((0 false) (1 false) (2 false))) (3 ((0 true) (1 true) (2 false)))
             (5 ((0 true) (1 false) (2 true))) (4 ((0 false) (1 false) (2 true)))
             (3 ((0 true) (1 true) (2 false))) (0 ((0 false) (1 false) (2 false)))
             (0
              ((0 false) (1 false) (2 false) (3 21) (4 22) (5 23) (6 24) (7 25) (8 26)
               (9 27) (10 28) (11 29) (12 30) (13 31) (14 32) (15 33) (16 34) (17 35)
               (18 36) (19 37))))
            |})
    in
    let result = 0b000011100101011000000 in
    xprintf "%d" result ~expect:(fun () -> {%expect| 117440 |});
    let verify () =
      xprintf
        "%a"
        List.(printf_list (fun out -> fprintf out "%d"))
        (part1 ({ a = result; b = 0; c = 0 }, snd parsed))
        ~expect:(fun () -> {%expect| 0,3,5,4,3,0 |})
    in
    fork_join_array [| part2; verify |]
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
      let specialize = function
        | 43 | 44 | 45 | 47 -> `Known true
        | 41 | 42 | 46 | 48 | 49 | 50 | 51 | 52 | 53 | 54 -> `Known false
        | 40 -> `Known false
        | 39 -> `Known true
        | 38 -> `Known false
        | 37 -> `Known false
        | 36 -> `Known true
        | 35 -> `Known true
        | 34 -> `Known false
        | 33 -> `Known true
        | 32 -> `Known false
        | 31 -> `Known false
        | 30 -> `Known false
        | 29 -> `Known true
        | 28 -> `Known false
        | 27 -> `Known false
        | 26 -> `Known true
        | 25 -> `Known false
        | 24 -> `Known false
        | 23 -> `Known false
        | 22 -> `Known true
        | 21 -> `Known true
        | 20 -> `Known false
        | 19 -> `Known true
        | 18 -> `Known false
        | 17 -> `Known false
        | 16 -> `Known false
        | 13 -> `Known true
        | 12 -> `Known false
        | 15 -> `Known false
        | 14 -> `Known false
        | 10 -> `Known false
        | 11 -> `Known true
        | 9 -> `Known true
        | 8 -> `Known false
        | 7 -> `Known false
        | 6 -> `Known false
        | 3 -> `Known true
        | 4 -> `Known false
        | 5 -> `Known true
        | 2 -> `Known false
        | 1 -> `Known true
        | 0 -> `Known false
        | _ -> `Unknown
      in
      xprint_s
        ([%sexp_of: (int * (int * int Blang.t) list) list]
         @@ part2 parsed ~specialize ~depth:16 ~digits_of_last_a:7)
        ~expect:(fun () ->
          {%expect|
            ((2 ((0 false) (1 true) (2 false))) (4 ((0 false) (1 false) (2 true)))
             (1 ((0 true) (1 false) (2 false))) (1 ((0 true) (1 false) (2 false)))
             (7 ((0 true) (1 true) (2 true))) (5 ((0 true) (1 false) (2 true)))
             (4 ((0 false) (1 false) (2 true))) (6 ((0 false) (1 true) (2 true)))
             (0 ((0 false) (1 false) (2 false))) (3 ((0 true) (1 true) (2 false)))
             (1 ((0 true) (1 false) (2 false))) (4 ((0 false) (1 false) (2 true)))
             (5 ((0 true) (1 false) (2 true))) (5 ((0 true) (1 false) (2 true)))
             (3 ((0 true) (1 true) (2 false))) (0 ((0 false) (1 false) (2 false))))
            |})
    in
    let verify () =
      let input = 0b101110001001101000100100011010000010101000101010 in
      xprintf "%d" input ~expect:(fun () -> {%expect| 202972175280682 |});
      let result = Solution.part1 ({ a = input; b = 0; c = 0 }, snd parsed) in
      xprintf
        "%a"
        List.(printf_list (fun out -> fprintf out "%d"))
        result
        ~expect:(fun () -> {%expect| 2,4,1,1,7,5,4,6,0,3,1,4,5,5,3,0 |})
    in
    fork_join_array [| part1; part2; verify |]
  in
  fork_join_array [| sample; sample2; input |]
;;
