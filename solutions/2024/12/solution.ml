open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  grid Fn.id
;;

let part1 grid =
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
    Map.sum (module Int) ~f:(fun (area, perimeter) -> area * perimeter) map
  in
  let seq f = iteri grid ~f:(fun pos crop -> f (pos, crop)) in
  Iter.group_by ~hash:[%hash: (_[@hash.ignore]) * char] ~eq:[%equal: _ * char] seq
  |> Parallel_iter.from_iter
  |> Parallel_iter.map ~f:divide
  |> Parallel_iter.sum
;;

let part2 _ = failwith "TODO"
