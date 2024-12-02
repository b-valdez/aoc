open! Aoc_std

let parse_prefix =
  let open Angstrom in
  string "seeds: " *> sep_by1 (char ' ') nat <* count 2 end_of_line <* commit
;;

let parser =
  let open Angstrom in
  pair ~sep:(skip_string "-to-") any_word
  *> string " map:\n"
  *> (triple ~sep:space nat |> lines_lazy)
  <* (count 2 end_of_line *> commit <|> end_of_input)
;;

let map maps x =
  List.find_map maps ~f:(fun (dst_start, src_start, length) ->
    if src_start <= x && x < src_start + length
    then Some (dst_start + x - src_start)
    else None)
  |> Option.value ~default:x
;;

let part1 seeds maps =
  let open Moonpool in
  Iter.fold maps ~init:(Fut.return seeds) ~f:(fun sources map_def ->
    Fut.map sources ~f:(List.map ~f:(map map_def)))
  |> Fut.await
  |> List.min_elt ~compare:[%compare: int]
  |> Option.value_exn
;;

type int_range =
  { start_inclusive : int
  ; end_exclusive : int
  }

let split_range int_range at =
  { int_range with end_exclusive = at }, { int_range with start_inclusive = at }
;;

let[@tail_mod_cons] rec split_ranges set boundaries =
  match set, boundaries with
  | [], _ -> []
  | _, [] -> set
  | { start_inclusive; _ } :: _, boundary :: boundaries when boundary <= start_inclusive
    -> split_ranges set boundaries
  | ({ end_exclusive; _ } as range) :: set, boundary :: _ when end_exclusive <= boundary
    -> range :: split_ranges set boundaries
  | range :: set, boundary :: boundaries ->
    let range_1, range_2 = split_range range boundary in
    range_1 :: range_2 :: split_ranges set boundaries
;;

let[@tail_mod_cons] rec normalize = function
  | ([] | [ _ ]) as set -> set
  | range1 :: ({ start_inclusive; _ } :: _ as set)
    when range1.end_exclusive < start_inclusive -> range1 :: normalize set
  | range1 :: { end_exclusive; _ } :: set when range1.end_exclusive >= end_exclusive ->
    range1 :: normalize set
  | range1 :: { end_exclusive; _ } :: set ->
    { range1 with end_exclusive } :: normalize set
;;

let apply_maps maps set =
  let boundaries =
    List.fold maps ~init:[] ~f:(fun acc (_, source_range_start, length) ->
      List.sort ((source_range_start + length) :: source_range_start :: acc) ~compare)
  in
  let split_ranges = split_ranges set boundaries in
  let map_range range (dst_start, src_start, _) =
    let diff = dst_start - src_start in
    { start_inclusive = range.start_inclusive + diff
    ; end_exclusive = range.end_exclusive + diff
    }
  in
  List.map split_ranges ~f:(fun range ->
    let map =
      List.find maps ~f:(fun (_, src_start, len) ->
        src_start <= range.start_inclusive && range.end_exclusive <= src_start + len)
    in
    Option.fold map ~init:range ~f:map_range)
  |> List.sort
       ~compare:
         (Comparable.lift compare ~f:(fun { start_inclusive; _ } -> start_inclusive))
  |> normalize
;;

let min_elt =
  List.min_elt
    ~compare:(Comparable.lift compare ~f:(fun { start_inclusive; _ } -> start_inclusive))
  >> Option.map ~f:(fun { start_inclusive; _ } -> start_inclusive)
;;

let part2 seeds maps =
  let _, seeds =
    List.fold seeds ~init:(None, []) ~f:(fun acc seen ->
      match acc with
      | None, acc -> Some seen, acc
      | Some seed, acc ->
        None, { start_inclusive = seed; end_exclusive = seed + seen } :: acc)
  in
  let seeds =
    seeds
    |> List.sort
         ~compare:
           (Comparable.lift compare ~f:(fun { start_inclusive; _ } -> start_inclusive))
    |> normalize
  in
  let open Moonpool in
  Iter.fold maps ~init:(Fut.return seeds) ~f:(fun fut map ->
    Fut.map fut ~f:(apply_maps map))
  |> await
  |> min_elt
  |> Option.value_exn
;;

type _ Effect.t += Yield : (int * int * int) list -> wrap Effect.t

let%expect_test "sample" =
  run
  @@ fun () ->
  let seeds, fiber = parse_file_prefix "sample.blob" parse_prefix in
  let maps f =
    Effect.Deep.try_with
      (Effect.Shallow.continue_with fiber (Wrap (parser, fun el -> Yield el)))
      { retc = Fun.id; exnc = raise; effc = (fun _ -> None) }
      { effc =
          (fun (type a) -> function
            | (Yield map : a Effect.t) ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  f map;
                  Effect.Deep.continue k (Wrap (parser, fun el -> Yield el)))
            | _ -> None)
      }
  in
  let[@warning "-8"] [| maps1; maps2 |] = tee_iter maps ~n:2 in
  let part1 () = xprintf "%d" (part1 seeds maps1) ~expect:(fun () -> {%expect| 35 |}) in
  let part2 () = xprintf "%d" (part2 seeds maps2) ~expect:(fun () -> {%expect| 46 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  run
  @@ fun () ->
  let seeds, fiber = parse_file_prefix "input.blob" parse_prefix in
  let maps f =
    Effect.Deep.try_with
      (Effect.Shallow.continue_with fiber (Wrap (parser, fun el -> Yield el)))
      { retc = Fun.id; exnc = raise; effc = (fun _ -> None) }
      { effc =
          (fun (type a) -> function
            | (Yield map : a Effect.t) ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  f map;
                  Effect.Deep.continue k (Wrap (parser, fun el -> Yield el)))
            | _ -> None)
      }
  in
  let[@warning "-8"] [| maps1; maps2 |] = tee_iter maps ~n:2 in
  let part1 () =
    xprintf "%d" (part1 seeds maps1) ~expect:(fun () -> {%expect| 462648396 |})
  in
  let part2 () =
    xprintf "%d" (part2 seeds maps2) ~expect:(fun () -> {%expect| 2520479 |})
  in
  fork_join_array [| part1; part2 |]
;;
