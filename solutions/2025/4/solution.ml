open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  sparse_tf_grid ~true_:'@' ()
;;

let is_accessible grid pos =
  List.fold_until
    [%all: Direction.t_with_diagonals]
    ~init:0
    ~f:(fun adjacent_rolls direction ->
      match Set.mem grid (Direction.step pos direction) with
      | true when adjacent_rolls = 3 -> Stop false
      | true -> Continue (adjacent_rolls + 1)
      | false -> Continue adjacent_rolls)
    ~finish:(Fun.negate @@ equal 4)
;;

let part1 grid = Set.count grid ~f:(is_accessible grid)

let remove_accessible grid =
  Set.fold grid ~init:grid ~f:(fun grid pos ->
    if is_accessible grid pos then Set.remove grid pos else grid)
;;

let part2 grid =
  let all_removed =
    until_stable ~equal:Position.Set.equal ~f:remove_accessible ~init:grid
  in
  Set.length grid - Set.length all_removed
;;
