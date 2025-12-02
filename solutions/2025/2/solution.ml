open! Aoc_std

let parser =
  let open Angstrom in
  let+ first = nat
  and+ last = char '-' *> nat
  and+ _ = option () (char ',' *> option () (end_of_line <|> end_of_input)) *> commit in
  first, last
;;

let invalid_singleton_iter_between half magnitude id1 id2 =
  let open Iter in
  singleton ((half * magnitude) + half) |> filter ~f:(Int.between ~low:id1 ~high:id2)
;;

let concat_self magnitude half = (half * magnitude) + half

let invalid_between id1 id2 =
  let open Iter in
  let digits id = Float.(iround_down_exn (log10 (float_of_int id))) + 1 in
  let digits1 = digits id1 in
  let digits2 = digits id2 in
  if digits1 = digits2
  then
    if digits1 mod 2 = 1
    then empty
    else (
      let magnitude = Int.pow 10 (digits1 / 2) in
      let upper_half id = id / magnitude in
      let upper_half1 = upper_half id1 in
      let upper_half2 = upper_half id2 in
      if upper_half1 <> upper_half2
      then
        invalid_singleton_iter_between upper_half1 magnitude id1 id2
        <+> (upper_half1 + 1 -- (upper_half2 - 1) |> map ~f:(concat_self magnitude))
        <+> invalid_singleton_iter_between upper_half2 magnitude id1 id2
      else invalid_singleton_iter_between upper_half1 magnitude id1 id2)
  else (
    let magnitude digits = Int.pow 10 (digits / 2) in
    let magnitude1 = magnitude digits1 in
    let magnitude2 = magnitude digits2 in
    let upper_half id magnitude = id / magnitude in
    let upper_half1 = upper_half id1 magnitude1 in
    let upper_half2 = upper_half id2 magnitude2 in
    let head =
      if digits1 mod 2 = 0
      then (
        let head_of_head =
          invalid_singleton_iter_between upper_half1 magnitude1 id1 id2
        in
        let tail_of_head =
          upper_half1 + 1 -- (magnitude1 - 1) |> map ~f:(concat_self magnitude1)
        in
        head_of_head <+> tail_of_head)
      else empty
    in
    let tail =
      if digits2 mod 2 = 0
      then (
        let tail =
          (magnitude2 / 10) -- (upper_half2 - 1) |> map ~f:(concat_self magnitude2)
        in
        let daeh = invalid_singleton_iter_between upper_half2 magnitude2 id1 id2 in
        tail <+> daeh)
      else empty
    in
    let middle =
      digits1 + 1 -- (digits2 - 1)
      |> filter ~f:(fun digits -> digits mod 2 = 0)
      |> flat_map ~f:(fun digits ->
        let magnitude = magnitude digits in
        (magnitude / 10) -- (magnitude - 1) |> map ~f:(concat_self magnitude))
    in
    head <+> middle <+> tail)
;;

let part1 iter =
  Iter.flat_map iter ~f:(fun (id1, id2) -> invalid_between id1 id2) |> Iter.sum
;;

let part2 _ = failwith "TODO"
