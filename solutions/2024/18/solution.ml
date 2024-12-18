open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  pair ~sep:(char ',') nat <* (end_of_line <|> end_of_input)
;;

let part1 fallen_bytes size seq =
  let corrupted =
    Iter.take fallen_bytes seq |> Iter.fold ~init:Position.Set.empty ~f:Set.add
  in
  a_star
    (module Position)
    (module Int)
    ~heuristic:(fun (x, y) -> size - x + size - y)
    ~start_positions:[ (0, 0), 0 ]
    ~is_goal:(fun (x, y) -> x = size && y = size)
    ~step:(fun pos cost ->
      Direction.all
      |> Iter.of_list
      |> Iter.map ~f:(fun direction -> Direction.step pos direction)
      |> Iter.filter ~f:(fun pos ->
        Int.between ~low:0 ~high:size (fst pos)
        && Int.between ~low:0 ~high:size (snd pos)
        && (not @@ Set.mem corrupted pos))
      |> Iter.map ~f:(fun pos -> pos, cost + 1))
  |> snd
;;

let rec add_floating_neighbors_of set floating = function
  | [] -> set, floating
  | [] :: more -> (add_floating_neighbors_of [@tailcall]) set floating more
  | (byte :: more) :: even_more ->
    let floating_neighbors =
      Direction.all_of_t_with_diagonals
      |> List.map ~f:(Direction.step byte)
      |> Position.Set.of_list
      |> Set.inter floating
    in
    (add_floating_neighbors_of [@tailcall])
      (Set.union set floating_neighbors)
      (Set.diff floating floating_neighbors)
      (more :: Set.to_list floating_neighbors :: even_more)
;;

let part2 size seq =
  let south_west =
    Set.union
      (Position.Set.of_increasing_iterator_unchecked ~len:(size + 1) ~f:(fun i -> -1, i))
      (Position.Set.of_increasing_iterator_unchecked ~len:(size + 1) ~f:(fun i ->
         i, size + 1))
  in
  let north_east =
    Set.union
      (Position.Set.of_increasing_iterator_unchecked ~len:(size + 1) ~f:(fun i ->
         size + 1, i))
      (Position.Set.of_increasing_iterator_unchecked ~len:(size + 1) ~f:(fun i -> i, -1))
  in
  let acc, found =
    Iter.fold_while
      seq
      ~init:((south_west, north_east, Position.Set.empty), None)
      ~f:(fun (((south_west, north_east, floating) as acc), _) byte ->
        let neighbors =
          Direction.all_of_t_with_diagonals
          |> List.map ~f:(Direction.step byte)
          |> Position.Set.of_list
        in
        let neighbors_sw = not @@ Set.are_disjoint south_west neighbors in
        let neighbour_ne = not @@ Set.are_disjoint north_east neighbors in
        match neighbors_sw, neighbour_ne with
        | true, true -> (acc, Some byte), `Stop
        | false, false ->
          ((south_west, north_east, Set.add floating byte), None), `Continue
        | true, false ->
          let south_west, floating =
            add_floating_neighbors_of (Set.add south_west byte) floating [ [ byte ] ]
          in
          ((south_west, north_east, floating), None), `Continue
        | false, true ->
          let north_east, floating =
            add_floating_neighbors_of (Set.add north_east byte) floating [ [ byte ] ]
          in
          ((south_west, north_east, floating), None), `Continue)
  in
  match found with
  | Some thing -> thing
  | None ->
    failwiths ~here:[%here] "No result" acc (fun (south_west, north_east, floating) ->
      [%message
        (south_west : Position.Set.t)
          (north_east : Position.Set.t)
          (floating : Position.Set.t)])
;;
