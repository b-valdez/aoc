open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  sparse_grid
;;

(* TODO factor out common part *)

let part1 ?(debug = false) (antennas_by_frequencies, width, height) =
  let antinodes =
    Map.iteri antennas_by_frequencies
    |> Iter.of_map_iteri
    |> Parallel_iter.from_iter ~padded:true
    |> Parallel_iter.flat_map ~f:(fun (_, antennas) ->
      Iter.diagonal_l antennas
      |> Parallel_iter.from_iter ~padded:true
      |> Parallel_iter.flat_map_l ~padded:true ~f:(fun (antenna_a, antenna_b) ->
        let diff = Position.(antenna_a - antenna_b) in
        if Position.(diff mod 3 = (0, 0))
        then
          Position.
            [ antenna_a + diff
            ; antenna_b - diff
            ; antenna_a - (diff / 3)
            ; antenna_b + (diff / 3)
            ]
        else Position.[ antenna_a + diff; antenna_b - diff ]))
    |> Parallel_iter.filter ~f:(fun (x, y) ->
      Int.between ~low:0 ~high:(width - 1) x && Int.between ~low:0 ~high:(height - 1) y)
    |> Parallel_iter.batch_fold ~init:Position.Set.empty ~f:(fun resonant batch ->
      Set.union resonant (Position.Set.of_array batch))
  in
  if debug then print_s ([%sexp_of: Position.Set.t] antinodes);
  Set.length antinodes
;;

let part2 (antennas_by_frequencies, width, height) =
  let antinodes =
    Map.iteri antennas_by_frequencies
    |> Iter.of_map_iteri
    |> Parallel_iter.from_iter ~padded:true
    |> Parallel_iter.flat_map ~f:(fun (_, antennas) ->
      Iter.diagonal_l antennas
      |> Parallel_iter.from_iter ~padded:true
      |> Parallel_iter.flat_map ~f:(fun (antenna_a, antenna_b) ->
        let diff = Position.(antenna_a - antenna_b) in
        let diff = Position.(diff / Tuple2.uncurry gcd diff) in
        Parallel_iter.combine
          (Iter.init ~f:(fun i -> Position.(antenna_a + (diff * i)))
           |> Iter.take_while ~f:(fun (x, y) ->
             Int.between ~low:0 ~high:(width - 1) x
             && Int.between ~low:0 ~high:(height - 1) y)
           |> Parallel_iter.from_iter ~padded:true)
          (Iter.init ~f:(fun i -> Position.(antenna_a + (diff * Int.(-1 - i))))
           |> Iter.take_while ~f:(fun (x, y) ->
             Int.between ~low:0 ~high:(width - 1) x
             && Int.between ~low:0 ~high:(height - 1) y)
           |> Parallel_iter.from_iter ~padded:true)))
    |> Parallel_iter.batch_fold ~init:Position.Set.empty ~f:(fun resonant batch ->
      Set.union resonant (Position.Set.of_array batch))
  in
  Set.length antinodes
;;
