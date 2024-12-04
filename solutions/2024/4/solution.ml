open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  grid Fun.id
;;

let find_from grid pos word =
  let rec find_in direction pos word =
    let pos = Direction.step pos direction in
    in_grid grid pos
    &&
    match word with
    | hd :: _ when Char.(grid.^(pos) <> hd) -> false
    | _ :: [] -> true
    | _ :: word -> find_in direction pos word
    | [] -> assert false
  in
  List.count Direction.all_of_t_with_diagonals ~f:(fun direction ->
    find_in direction pos word)
;;

let part1 grid =
  Grid.iteri grid
  |> Parallel_iter.from_labelled_iter2 ~padded:true
  |> Parallel_iter.filter ~f:(Tuple2.get2 >> Char.equal 'X')
  |> Parallel_iter.map ~f:(fun (pos, _) -> find_from grid pos [ 'M'; 'A'; 'S' ])
  |> Parallel_iter.sum ~padded:true
;;

let is_x_mas_when_a grid pos =
  List.exists Direction.all_of_diagonals ~f:(fun diagonal ->
    let open Direction in
    let ( = ) = Char.( = ) in
    grid.^(step pos diagonal) = 'M'
    && grid.^(step pos (turn_diagonals diagonal Right)) = 'M'
    && grid.^(step pos (opposite_diagonals diagonal)) = 'S'
    && grid.^(step pos (turn_diagonals diagonal Left)) = 'S')
;;

let part2 grid =
  Grid.iteri grid
  |> Parallel_iter.from_labelled_iter2 ~padded:true
  |> Parallel_iter.filter_count ~f:(function
    | (0, _ | _, 0), _ -> false
    | (width', _), _ when width' + 1 = width grid -> false
    | (_, height'), _ when height' + 1 = height grid -> false
    | pos, 'A' -> is_x_mas_when_a grid pos
    | _ -> false)
;;
