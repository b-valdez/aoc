open! Aoc_std

type token =
  | Mul of int * int
  | EOF
  | Do
  | Dont
[@@deriving sexp]

let parser =
  let open Angstrom in
  let mul =
    string "mul(" *> (pair ~sep:(char ',' *> return ()) nat >>| fun (a, b) -> Mul (a, b))
    <* char ')'
  in
  let eof = end_of_input *> return EOF in
  let do_ = string "do()" *> return Do in
  let dont = string "don't()" *> return Dont in
  fix (fun parser ->
    skip_while (function
      | 'm' | 'd' -> false
      | _ -> true)
    *> choice [ eof; do_; dont; mul; advance 1 *> commit *> parser ])
;;

let part1 =
  Parallel_iter.filter_map ~f:(function
    | Mul (a, b) -> Some (a * b)
    | _ -> None)
  >> Parallel_iter.sum
;;

let part2 =
  Iter.fold_filter_map ~init:true ~f:(fun doing ->
      function
      | Dont -> false, None
      | Do -> true, None
      | EOF -> doing, None
      | Mul (a, b) when doing -> true, Some (a * b)
      | Mul _ -> false, None)
  >> Iter.sum
;;
