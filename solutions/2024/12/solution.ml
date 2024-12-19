open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  grid Fn.id
;;

let part1 stream grid =
  let union pos crop uf_map current_class map dir =
    match grid.?(Direction.step pos dir) with
    | None -> map
    | Some other_crop when Char.(crop <> other_crop) -> map
    | Some _crop ->
      let neighbor_class = Map.find_exn uf_map (Direction.step pos dir) in
      if Union_find.same_class current_class neighbor_class
      then
        Map.update map (Union_find.get current_class) ~f:(fun data ->
          let area, perimeter = Option.value_exn data in
          area, perimeter - 2)
      else (
        (* This is cumbersome, there should be a better way. I need to know the set of equivalence classes at the end, so I do it this way *)
        let rep = Union_find.get current_class in
        let other_rep = Union_find.get neighbor_class in
        Union_find.union current_class neighbor_class;
        Union_find.set current_class rep;
        let neighbor_area, neighbor_perimeter = Map.find_exn map other_rep in
        Map.update map rep ~f:(fun data ->
          let area, perimeter = Option.value_exn data in
          area + neighbor_area, perimeter + neighbor_perimeter - 2)
        |> Fn.flip Map.remove other_rep)
  in
  let divide group =
    let crop = List.hd_exn group |> snd in
    let map =
      Iter.of_list group
      |> Iter.fold ~init:Position.Map.empty ~f:(fun map (pos, _) ->
        Map.add_exn map ~key:pos ~data:(1, 4))
    in
    let uf_map = Map.mapi map ~f:(fun ~key:pos ~data:_ -> Union_find.create pos) in
    let map =
      Iter.of_list group
      |> Iter.fold ~init:map ~f:(fun map (pos, _) ->
        let current_class = Map.find_exn uf_map pos in
        List.fold ~init:map [ `E; `S ] ~f:(union pos crop uf_map current_class))
    in
    Stream.push stream (uf_map, map);
    Map.sum (module Int) ~f:(fun (area, perimeter) -> area * perimeter) map
  in
  let seq f = iteri grid ~f:(fun pos crop -> f (pos, crop)) in
  let result =
    Iter.group_by ~hash:[%hash: (_[@hash.ignore]) * char] ~eq:[%equal: _ * char] seq
    |> Parallel_iter.from_iter
    |> Parallel_iter.map ~f:divide
    |> Parallel_iter.sum
  in
  Stream.poison stream Parallel_iter.Stream_closed;
  result
;;

let part2 cursor grid =
  let uf_map, map =
    Parallel_iter.fold
      ~init:(Position.Map.empty, Position.Map.empty)
      (Parallel_iter.of_cursor cursor)
      ~f:(fun (uf_map, map) (uf_map', map') ->
        Map.merge_disjoint_exn uf_map uf_map', Map.merge_disjoint_exn map map')
  in
  let fold_count_sides ((last_offset, at_last_pos), sides) dir (x, y) =
    let offset = grid.?(Direction.step (x, y) dir) |> Option.value ~default:'.' in
    let at_pos = grid.?(x, y) |> Option.value ~default:'.' in
    if Char.(offset = at_pos)
    then (offset, at_pos), sides
    else (
      let sides =
        if Char.(last_offset <> at_last_pos && offset = last_offset)
        then sides
        else (
          let area =
            Option.value_map
              ~default:0
              (Map.find uf_map (Direction.step (x, y) dir))
              ~f:(Union_find.get >> Map.find_exn map >> fst)
          in
          sides + area)
      in
      let sides =
        if Char.(last_offset <> at_last_pos && at_pos = at_last_pos)
        then sides
        else (
          let area =
            Option.value_map
              ~default:0
              (Map.find uf_map (x, y))
              ~f:(Union_find.get >> Map.find_exn map >> fst)
          in
          sides + area)
      in
      (offset, at_pos), sides)
  in
  let horizontal =
    Iter.(-1 -- (height grid - 1))
    |> Iter.map ~f:(fun y ->
      Iter.(0 -- (width grid - 1))
      |> Iter.fold ~init:(('.', '.'), 0) ~f:(fun acc x -> fold_count_sides acc `S (x, y))
      |> snd)
    |> Iter.sum
  in
  let vertical =
    Iter.(-1 -- (width grid - 1))
    |> Iter.map ~f:(fun x ->
      Iter.(0 -- (height grid - 1))
      |> Iter.fold ~init:(('.', '.'), 0) ~f:(fun acc y -> fold_count_sides acc `E (x, y))
      |> snd)
    |> Iter.sum
  in
  horizontal + vertical
;;
