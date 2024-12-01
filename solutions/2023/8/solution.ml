open! Aoc_std

type lr =
  | Left
  | Right
[@@deriving sexp]

let parser =
  let open Angstrom in
  let open Let_syntax in
  let node =
    let%mapn id = take 3 <* take 4
    and left = take 3 <* take 2
    and right = take 3 <* take 1 in
    id, (left, right)
  in
  let%mapn directions =
    many1 (char 'L' *> return Left <|> char 'R' *> return Right <* commit)
    <* end_of_line
    <* end_of_line
    <* commit
  and nodes = lines node in
  directions, nodes |> String.Map.of_alist_exn
;;

let part1 (directions, nodes) =
  let directions = Iter.of_list directions |> Iter.cycle in
  let should_continue = function
    | "ZZZ" -> `Stop
    | _ -> `Continue
  in
  Iter.fold_while directions ~init:("AAA", 0) ~f:(fun (pos, steps) ->
    let left, right = Map.find_exn nodes pos in
    function
    | Left -> (left, steps + 1), should_continue left
    | Right -> (right, steps + 1), should_continue right)
  |> snd
;;

(** Assumption, checked for the given inputs: each start leads to only one end, an end encountered after n steps will also be encountered every k * n steps*)
let part2 (directions, nodes) =
  let directions = Iter.of_list directions |> Iter.cycle in
  let should_continue next steps =
    if not @@ String.is_suffix next ~suffix:"Z"
    then (next, steps + 1), `Continue
    else (next, steps + 1), `Stop
  in
  let start_positions = Map.keys nodes |> List.filter ~f:(String.is_suffix ~suffix:"A") in
  (* TODO use the loopfinding from Util? *)
  let find_loop start =
    let _, loop_start =
      Iter.fold_while directions ~init:(start, 0) ~f:(fun (pos, steps) direction ->
        let next =
          Map.find_exn nodes pos
          |>
          match direction with
          | Left -> fst
          | Right -> snd
        in
        should_continue next steps)
    in
    loop_start
  in
  let result = Moonpool.Atomic.make_contended 1 in
  let backoff = Backoff.default in
  let rec atomic_lcm acc lcm_me backoff =
    let acc = lcm acc lcm_me in
    let acc = Moonpool.Atomic.exchange result acc in
    if acc mod lcm_me <> 0
    then (
      let backoff = Backoff.once backoff in
      atomic_lcm acc lcm_me backoff)
  in
  Parallel_iter.from_labelled_iter (List.iter start_positions)
  |> Parallel_iter.map ~f:find_loop
  |> Parallel_iter.iter ~f:(fun i ->
    let acc = Multicore_magic.fenceless_get result in
    atomic_lcm acc i backoff);
  Multicore_magic.fenceless_get result
;;

let%expect_test "sample" =
  run
  @@ fun _ ->
  let part1a () =
    let parsed = parse_file "sample.blob" parser in
    xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 2 |})
  in
  let part1b () =
    let parsed2 = parse_file "sample2.blob" parser in
    xprintf "%d" (part1 parsed2) ~expect:(fun () -> {%expect| 6 |})
  in
  let part2 () =
    xprintf
      "%d"
      (part2 @@ parse_file "sample3.blob" parser)
      ~expect:(fun () -> {%expect| 6 |})
  in
  fork_join_array [| part1a; part1b; part2 |]
;;

let%expect_test "input" =
  run
  @@ fun _ ->
  let parsed = parse_file "input.blob" parser in
  let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 20777 |}) in
  let part2 () =
    xprintf "%d" (part2 parsed) ~expect:(fun () -> {%expect| 13289612809129 |})
  in
  fork_join_array [| part1; part2 |]
;;
