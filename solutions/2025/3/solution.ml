open! Aoc_std

let parser =
  let open Angstrom in
  take_while1 Char.is_digit <* (end_of_line <|> end_of_input)
;;

let iter_acc map =
  Map.iter map |> Iter.from_labelled_iter |> Iter.map ~f:Char.get_digit_exn
;;

let iter_to_result = Iter.fold ~init:0 ~f:(fun acc digit -> (10 * acc) + digit)

let insert bank n len (map, i) c : _ Continue_or_stop.t =
  if Map.length map + len - i = n
  then (
    let start = iter_acc map in
    let end_ =
      String.slice bank i 0
      |> String.iter
      |> Iter.from_labelled_iter
      |> Iter.map ~f:Char.get_digit_exn
    in
    let result = iter_to_result Iter.(start <+> end_) in
    Stop result)
  else (
    let map =
      Map.binary_search_subrange
        map
        ~compare:(fun ~key ~data bound ->
          let num_needed_to_fill_map_from_here = n - key in
          let num_banks_still_to_consider = len - i in
          if num_needed_to_fill_map_from_here > num_banks_still_to_consider
          then -1
          else Char.descending data bound)
        ~lower_bound:Unbounded
        ~upper_bound:(Maybe_bound.Incl c)
    in
    let map =
      if Map.length map < n then Map.add_exn map ~key:(Map.length map) ~data:c else map
    in
    Continue (map, i + 1))
;;

let max_joltage n bank =
  let acc = Dynarray.create () in
  Dynarray.set_capacity acc n;
  let len = String.length bank in
  String.fold_until
    bank
    ~init:(Int.Map.empty, 0)
    ~f:(insert bank n len)
    ~finish:(fun (map, _) -> iter_acc map |> iter_to_result)
;;

let part1 banks =
  let open Iter in
  banks |> map ~f:(max_joltage 2) |> sum
;;

let part2 banks =
  let open Iter in
  banks |> map ~f:(max_joltage 12) |> sum
;;
