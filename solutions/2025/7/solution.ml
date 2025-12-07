open! Aoc_std

let prefix_parser =
  let open Angstrom in
  dots *> pos
  <* char 'S'
  <* commit
  <* dots
  <* end_of_line
  <* commit
  <* dots
  <* end_of_line
;;

let parser =
  let open Angstrom in
  dots *> sep_by1 dots (pos <* char '^')
  <* dots
  <* end_of_line
  <* dots
  <* (end_of_line <|> end_of_input)
;;

let rec perform_splits next_tachyons splittings tachyons splitters =
  match next_tachyons, tachyons, splitters with
  | next_tachyons, [], _ -> splittings, List.rev next_tachyons
  | next_tachyons, tachyons, [] -> splittings, List.rev_append next_tachyons tachyons
  | (tachyon' :: _ as next_tachyons), tachyon :: tachyons, splitters
    when tachyon = tachyon' ->
    (perform_splits [@tailcall]) next_tachyons splittings tachyons splitters
  | next_tachyons, tachyon :: tachyons, (splitter :: _ as splitters)
    when tachyon < splitter ->
    (perform_splits [@tailcall]) (tachyon :: next_tachyons) splittings tachyons splitters
  | next_tachyons, (tachyon :: _ as tachyons), splitter :: splitters
    when tachyon > splitter ->
    (perform_splits [@tailcall]) next_tachyons splittings tachyons splitters
  | (tachyon' :: _ as next_tachyons), tachyon :: tachyons, _ :: splitters
    when tachyon' = tachyon - 1 ->
    (perform_splits [@tailcall])
      ((tachyon + 1) :: next_tachyons)
      (splittings + 1)
      tachyons
      splitters
  | next_tachyons, tachyon :: tachyons, _ :: splitters ->
    (perform_splits [@tailcall])
      ((tachyon + 1) :: (tachyon - 1) :: next_tachyons)
      (splittings + 1)
      tachyons
      splitters
;;

let part1 start lines =
  Iter.fold lines ~init:(0, [ start ]) ~f:(fun (splittings, tachyons) splitters ->
    perform_splits [] splittings tachyons splitters)
  |> fst
;;

(* mutual recursion keeps the cases simple *)
let[@tail_mod_cons] rec perform_splits tachyons splitters =
  match tachyons, splitters with
  | [], _ -> []
  | tachyons, [] -> tachyons
  | ((pos, _) :: _ as tachyons), splitter :: splitters when splitter < pos ->
    (perform_splits [@tailcall]) tachyons splitters
  | (pos, count) :: tachyons, (splitter :: _ as splitters) when splitter > pos ->
    (anticipate_merge_with_next_splitter [@tailcall]) pos count tachyons splitters
  | (pos, count) :: tachyons, _ :: splitters (* when splitter = pos *) ->
    (pos - 1, count)
    :: (anticipate_merge_with_unsplitted [@tailcall]) (pos + 1) count tachyons splitters

and[@tail_mod_cons] anticipate_merge_with_unsplitted pos count tachyons splitters =
  match tachyons with
  | (pos', count') :: tachyons when pos = pos' ->
    (anticipate_merge_with_next_splitter [@tailcall])
      pos
      (count + count')
      tachyons
      splitters
  | tachyons ->
    (anticipate_merge_with_next_splitter [@tailcall]) pos count tachyons splitters

and[@tail_mod_cons] anticipate_merge_with_next_splitter pos count tachyons splitters =
  match tachyons, splitters with
  | (pos', count') :: tachyons, splitter :: splitters
    when pos + 1 = pos' && pos' = splitter ->
    (pos, count + count')
    :: (anticipate_merge_with_unsplitted [@tailcall]) (pos + 2) count' tachyons splitters
  | tachyons, splitters -> (pos, count) :: (perform_splits [@tailcall]) tachyons splitters
;;

let part2 start lines =
  Iter.fold lines ~init:[ start, 1 ] ~f:perform_splits |> List.sum (module Int) ~f:snd
;;
