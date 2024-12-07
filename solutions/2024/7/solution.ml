open! Aoc_std

let parser =
  let open Angstrom in
  let%mapn test_value = nat <* char ':' <* space <* commit
  and equation = both nat (many (space *> nat)) <* (end_of_line <|> end_of_input) in
  test_value, equation
;;

let rec is_fulfillable test_value operations acc = function
  | [] -> List.mem acc test_value ~equal
  | number :: rest ->
    let acc =
      List.concat_map acc ~f:(fun intermediate_result ->
        Iter.on_list
          (Iter.map ~f:(fun op -> op intermediate_result number)
           >> Iter.filter ~f:(fun intermediate_result ->
             intermediate_result <= test_value))
          operations)
    in
    (is_fulfillable [@tailcall]) test_value operations acc rest
;;

let part1 =
  Parallel_iter.of_cursor
  >> Parallel_iter.filter_map ~f:(fun (test_value, (equation_hd, equation_tl)) ->
    Option.some_if
      (is_fulfillable test_value [ ( + ); ( * ) ] [ equation_hd ] equation_tl)
      test_value)
  >> Parallel_iter.sum
;;

let ( || ) a b = Int.(of_string (to_string a ^ to_string b))

let part2 =
  Parallel_iter.of_cursor
  >> Parallel_iter.filter_map ~f:(fun (test_value, (equation_hd, equation_tl)) ->
    Option.some_if
      (is_fulfillable test_value [ ( + ); ( * ); ( || ) ] [ equation_hd ] equation_tl)
      test_value)
  >> Parallel_iter.sum
;;
