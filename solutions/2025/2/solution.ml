open! Aoc_std

let parser =
  let open Angstrom in
  interval <* option () (char ',' *> option () (end_of_line <|> end_of_input)) <* commit
;;

let rec repeated n magnitude to_repeat =
  if n = 1
  then to_repeat
  else if n mod 2 = 0
  then repeated (n / 2) (magnitude * magnitude) ((to_repeat * magnitude) + to_repeat)
  else
    to_repeat
    + (magnitude
       * repeated (n / 2) (magnitude * magnitude) ((to_repeat * magnitude) + to_repeat))
;;

let checked_invalid ~repeats ~half ~magnitude ~id1 ~id2 =
  let open Iter in
  singleton (repeated repeats magnitude half)
  |> filter ~f:(Int.between ~low:id1 ~high:id2)
;;

let invalid_between_with_repeats ?low ?high ~digits repeats =
  if digits mod repeats <> 0
  then Iter.empty
  else (
    let magnitude = Int.pow 10 (digits / repeats) in
    let between =
      match low, high with
      | None, None -> Fun.const true
      | Some low, None -> fun id -> id >= low
      | None, Some high -> fun id -> id <= high
      | Some low, Some high -> Int.between ~low ~high
    in
    let start, next =
      match low with
      | None -> Iter.empty, magnitude / 10
      | Some low ->
        let to_repeat = low / Int.pow magnitude (repeats - 1) in
        let start =
          Iter.(singleton (repeated repeats magnitude to_repeat) |> filter ~f:between)
        in
        start, to_repeat + 1
    in
    let end_, last =
      match high with
      | None -> Iter.empty, magnitude - 1
      | Some high ->
        let to_repeat = high / Int.pow magnitude (repeats - 1) in
        let end_ =
          Iter.(singleton (repeated repeats magnitude to_repeat) |> filter ~f:between)
        in
        end_, to_repeat - 1
    in
    let middle =
      let open Iter in
      next -- last |> map ~f:(repeated repeats magnitude)
    in
    Iter.(start <+> middle <+> end_))
;;

let invalid_between part id1 id2 =
  let open Iter in
  let digits id = Float.(iround_down_exn (log10 (float_of_int id))) + 1 in
  let digits1 = digits id1 in
  let digits2 = digits id2 in
  let valid_repeats digits =
    match part with
    | Part1 -> singleton 2
    | Part2 -> 2 -- digits
  in
  if digits1 = digits2
  then
    valid_repeats digits1
    |> flat_map ~f:(invalid_between_with_repeats ~low:id1 ~high:id2 ~digits:digits1)
    |> to_set (module Int)
    |> Set.sum (module Int) ~f:Fun.id
  else (
    let head =
      valid_repeats digits1
      |> flat_map ~f:(invalid_between_with_repeats ~low:id1 ~digits:digits1)
      |> to_set (module Int)
      |> Set.sum (module Int) ~f:Fun.id
    in
    let tail =
      valid_repeats digits2
      |> flat_map ~f:(invalid_between_with_repeats ~high:id2 ~digits:digits2)
      |> to_set (module Int)
      |> Set.sum (module Int) ~f:Fun.id
    in
    let middle =
      digits1 + 1 -- (digits2 - 1)
      |> flat_map ~f:(fun digits ->
        valid_repeats digits |> flat_map ~f:(invalid_between_with_repeats ~digits))
      |> to_set (module Int)
      |> Set.sum (module Int) ~f:Fun.id
    in
    head + tail + middle)
;;

let part1 iter =
  Iter.map iter ~f:(fun (id1, id2) -> invalid_between Part1 id1 id2) |> Iter.sum
;;

(* I dislike that I am recomputing the repetitions from part 1 here *)
let part2 iter =
  Iter.map iter ~f:(fun (id1, id2) -> invalid_between Part2 id1 id2) |> Iter.sum
;;
