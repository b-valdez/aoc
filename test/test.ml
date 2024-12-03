open! Aoc_std

let%expect_test "take_from_iter" =
  Moonpool_fib.main (fun _ ->
    [ 1; 2; 3; 4; 5; 6; 7; 8 ]
    |> List.iter
    |> Parallel_iter.from_labelled_iter ~yield_every:1
    |> Parallel_iter.tap ~f:(printf "before take: %i\n")
    |> Parallel_iter.take 2
    |> Parallel_iter.iter ~f:(printf "after take: %i\n"));
  [%expect
    {|
    before take: 1
    after take: 1
    before take: 2
    after take: 2
    |}]
;;

let%expect_test "take_from_fun" =
  Moonpool_fib.main (fun _ ->
    let i = ref 0 in
    Parallel_iter.from_fun ~yield_every:1 (fun () ->
      if !i < 10
      then (
        incr i;
        Some !i)
      else None)
    |> Parallel_iter.tap ~f:(printf "before take: %i\n")
    |> Parallel_iter.take 2
    |> Parallel_iter.iter ~f:(printf "after take: %i\n"));
  [%expect
    {|
    before take: 1
    after take: 1
    before take: 2
    after take: 2
    |}]
;;

let%expect_test "take_singleton" =
  Moonpool_fib.main (fun _ ->
    Parallel_iter.singleton 1
    |> Parallel_iter.tap ~f:(printf "before take: %i\n")
    |> Parallel_iter.take 0
    |> Parallel_iter.iter ~f:(printf "after take: %i\n"));
  [%expect {| |}]
;;

let%expect_test "take_doubleton" =
  Moonpool_fib.main (fun (_ : Moonpool.Runner.t) ->
    Parallel_iter.doubleton 1 2
    |> Parallel_iter.tap ~f:(printf "before take: %i\n")
    |> Parallel_iter.take 1
    |> Parallel_iter.iter ~f:(printf "after take: %i\n"));
  [%expect
    {|
    before take: 1
    after take: 1
    |}]
;;

(** TODO test the other creators *)
