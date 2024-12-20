open! Aoc_std
open Grid

type cell =
  | Wall
  | Empty
  | Start
  | End
[@@deriving equal]

let parser =
  let open Angstrom in
  grid_start
    ~equal:[%equal: cell]
    (function
      | 'S' -> Start
      | '#' -> Wall
      | '.' -> Empty
      | 'E' -> End
      | _ -> assert false)
    Start
;;

let honest_path (start, grid) =
  let rec honest_path path pos i =
    if [%equal: cell] End grid.^(pos)
    then path
    else (
      let next =
        Direction.all
        |> Iter.of_list
        |> Iter.map ~f:(Direction.step pos)
        |> Iter.filter ~f:(fun pos ->
          (not ([%equal: cell] Wall grid.^(pos))) && not (Map.mem path pos))
        |> Iter.head_exn
      in
      honest_path (Map.add_exn path ~key:next ~data:i) next (i + 1))
  in
  honest_path (Position.Map.singleton start 0) start 1
;;

let part1 ?(min_diff = 100) (_, grid) honest_path =
  let cheats =
    Map.sumi
      (module Int)
      honest_path
      ~f:(fun ~key:pos ~data:step ->
        Direction.all
        |> List.count ~f:(fun dir ->
          let neighbor = Direction.step pos dir in
          [%equal: cell] Wall grid.^(neighbor)
          && Map.find honest_path (Direction.step neighbor dir)
             |> Option.exists ~f:(fun step_after_cheat ->
               step_after_cheat - (step + 2) >= min_diff)))
  in
  cheats
;;

let part2 ?(min_diff = 100) _ honest_path =
  let cheats =
    Map.iteri honest_path
    |> Iter.of_map_iteri
    |> Parallel_iter.from_iter ~padded:true
    |> Parallel_iter.map ~f:(fun (cheat_start, step) ->
      Map.counti honest_path ~f:(fun ~key:cheat_end ~data:step2 ->
        let dist = Position.dist cheat_start cheat_end in
        dist <= 20 && step2 - (step + dist) >= min_diff))
    |> Parallel_iter.sum ~padded:true
  in
  cheats
;;
