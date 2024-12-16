open! Aoc_std
open Grid

type cell =
  | Wall
  | Empty
  | Goal
  | Start
[@@deriving equal]

let parser =
  let open Angstrom in
  grid_start
    ~equal:[%equal: cell]
    (function
      | '#' -> Wall
      | '.' -> Empty
      | 'S' -> Start
      | 'E' -> Goal
      | _ -> assert false)
    Start
;;

(* TODO candidate for promotion to Aoc_std.Grid *)
module Position_with_direction = struct
  type t = Position.t * Direction.t

  include Tuple.Comparable (Position) (Direction)
end

let part1 (start, grid) =
  let goal, _ =
    Grid.iteri grid
    |> Iter.from_labelled_iter2
    |> Iter.find_pred_exn ~f:(function
      | _, Goal -> true
      | _ -> false)
  in
  let step (position, direction) score =
    let step_forward = Direction.step position direction in
    let turns =
      Iter.of_list [%all: Direction.turn]
      |> Iter.map ~f:(fun turn -> (position, Direction.turn direction turn), score + 1000)
    in
    match grid.^(step_forward) with
    | Wall -> turns
    | _ -> Iter.cons ((step_forward, direction), score + 1) turns
  in
  let is_goal (position, _) = [%equal: Position.t] goal position in
  let heuristic (position, direction) =
    let to_go = Position.(position - goal) in
    let dist = abs (fst to_go) + abs (snd to_go) in
    match direction, horizontal_project to_go, vertical_project to_go with
    | _, None, None -> 0
    | #Direction.horizontal, None, _ | #Direction.vertical, _, None -> dist + 1000
    | (#Direction.horizontal as direction), Some direction', vertical
      when ([%equal: Direction.t] :> Direction.horizontal -> Direction.horizontal -> bool)
             direction
             direction' -> dist + if Option.is_some vertical then 1000 else 0
    | (#Direction.vertical as direction), horizontal, Some direction'
      when ([%equal: Direction.t] :> Direction.vertical -> Direction.vertical -> bool)
             direction
             direction' -> dist + if Option.is_some horizontal then 1000 else 0
    | #Direction.t, _, _ -> dist + 2000
  in
  a_star
    (module Position_with_direction)
    (module Int)
    ~step
    ~start_positions:[ (start, `E), 0 ]
    ~is_goal
    ~heuristic
  |> snd
;;

let part2 _ = failwith "TODO"
