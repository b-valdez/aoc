open! Aoc_std

let parser =
  let open Angstrom in
  let%mapn seeds =
    string "seeds: " *> sep_by1 (char ' ') nat <* count 2 end_of_line <* commit
  and maps =
    pair ~sep:(skip_string "-to-") any_word
    *> string " map:\n"
    *> (triple ~sep:space nat |> lines_lazy)
    |> blocks
  in
  seeds, maps
;;

let map maps x =
  List.find_map maps ~f:(fun (dst_start, src_start, length) ->
    if src_start <= x && x < src_start + length
    then Some (dst_start + x - src_start)
    else None)
  |> Option.value ~default:x
;;

let part1 (seeds, maps) =
  List.fold maps ~init:seeds ~f:(fun sources map_def -> List.map ~f:(map map_def) sources)
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

let apply_maps set maps =
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

let part2 (seeds, maps) =
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
  List.fold maps ~init:seeds ~f:apply_maps |> min_elt |> Option.value_exn
;;

let%expect_test "sample" =
  let open Sample in
  let parsed = parse_string parser sample in
  printf "%d" (part1 parsed);
  [%expect {| 35 |}];
  printf "%d" (part2 parsed);
  [%expect {| 46 |}]
;;

let%expect_test "input" =
  let open Input in
  let parsed = parse_string parser input in
  printf "%d" (part1 parsed);
  [%expect {| 462648396 |}];
  printf "%d" (part2 parsed);
  [%expect {| 2520479 |}]
;;
