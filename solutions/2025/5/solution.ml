open! Aoc_std

let parse_prefix =
  let open Angstrom in
  many_till (interval <* end_of_line <* commit) (end_of_line *> commit)
;;

let parser =
  let open Angstrom in
  nat <* (end_of_line <|> end_of_input)
;;

let part1 fresh available =
  Iter.filter available ~f:(fun id -> List.exists fresh ~f:(Fun.flip Interval.mem id))
  |> Iter.length
;;

let part2 fresh = Interval.set_of_list fresh |> Interval.length
