open! Aoc_std

let parser =
  let open Angstrom in
  choice [ char 'L' *> return ( ~- ); char 'R' *> return ( ~+ ) ]
  <*> nat
  <* (end_of_line <|> end_of_input)
;;

let part1 iter =
  let _, zeroes =
    Iter.fold iter ~init:(50, 0) ~f:(fun (dial, zeroes) rotation ->
      let dial = (dial + rotation) mod 100 in
      let zeroes = if dial = 0 then zeroes + 1 else zeroes in
      dial, zeroes)
  in
  zeroes
;;

let part2 iter =
  let _, zeroes =
    Iter.fold iter ~init:(50, 0) ~f:(fun (dial, zeroes) rotation ->
      let dial_was_zero = dial = 0 in
      let zeroes = zeroes + abs (rotation / 100) in
      let dial = dial + (rotation mod 100) in
      let dial, zeroes =
        match dial with
        | dial when dial < 0 && dial_was_zero -> dial + 100, zeroes
        | dial when dial < 0 -> dial + 100, zeroes + 1
        | dial when dial > 99 -> dial - 100, zeroes + 1
        | 0 -> 0, zeroes + 1
        | dial -> dial, zeroes
      in
      dial, zeroes)
  in
  zeroes
;;
