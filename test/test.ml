open! Aoc_std
open Picos_std_structured

let%expect_test "bundle_flock" =
  Moonpool_fib.main (fun _ ->
    Bundle.join_after (fun bundle ->
      (try
         Bundle.join_after (fun inner ->
           Bundle.fork inner Control.block;
           Bundle.fork bundle (fun () ->
             print_endline "terminate inner bundle";
             Bundle.terminate inner;
             Control.yield ();
             Bundle.fork bundle (fun () -> print_endline "outer can still spawn"));
           print_endline "end of inner bundle")
       with
       | Control.Terminate -> print_endline "inner raised Terminate");
      print_endline "inner bundle joined");
    print_endline "outer bundle joined");
  [%expect
    {|
    end of inner bundle
    terminate inner bundle
    inner bundle joined
    outer can still spawn
    outer bundle joined
    |}]
;;

let%expect_test "take_from_iter" =
  Moonpool_fib.main (fun _ ->
    [ 1; 2; 3; 4; 5; 6; 7; 8 ]
    |> List.iter
    |> Parallel_iter.from_labelled_iter
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
    Parallel_iter.from_fun (fun () ->
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
  [%expect {| before take: 1 |}]
;;

let%expect_test "take_doubleton" =
  Moonpool_fib.main (fun _ ->
    Parallel_iter.doubleton 1 2
    |> Parallel_iter.tap ~f:(printf "before take: %i\n")
    |> Parallel_iter.take 1
    |> Parallel_iter.iter ~f:(printf "after take: %i\n"));
  [%expect {|
    before take: 1
    after take: 1
    before take: 2
    |}]
;;

(** TODO test the other creators *)
