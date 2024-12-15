open! Aoc_std
open Grid

type cell =
  | Box
  | Wall
  | Empty
  | Robot
[@@deriving equal, sexp_of]

let prefix_parser =
  let open Angstrom in
  grid_start
    ~equal:[%equal: cell]
    (function
      | '#' -> Wall
      | 'O' -> Box
      | '.' -> Empty
      | '@' -> Robot
      | _ -> assert false)
    Robot
  <* end_of_line
  <* end_of_line
;;

let parser =
  let open Angstrom in
  skip_while Char.is_whitespace
  *> choice
       [ char '^' *> return `N
       ; char '>' *> return `E
       ; char 'v' *> return `S
       ; char '<' *> return `W
       ]
;;

let part1 robot_pos grid moves =
  grid.^(robot_pos) <- Empty;
  let _ : Position.t =
    Iter.fold moves ~init:robot_pos ~f:(fun robot_pos move ->
      if !is_shutdown
      then (
        grid.^(robot_pos) <- Robot;
        Array.iter grid ~f:(fun row ->
          Array.iter row ~f:(function
            | Box -> print_string "O"
            | Robot -> print_string "@"
            | Empty -> print_string "."
            | Wall -> print_string "#");
          print_string "\n");
        raise_notrace Moonpool.Shutdown);
      let pos, cell =
        iteri_from grid ~pos:robot_pos ~direction:move
        |> Iter.from_labelled_iter2
        |> Iter.find_pred_exn ~f:(function
          | _, Box -> false
          | _ -> true)
      in
      match cell with
      | Wall -> robot_pos
      | Empty ->
        let next_robot_pos = Direction.step robot_pos move in
        grid.^(pos) <- Box;
        grid.^(next_robot_pos) <- Empty;
        next_robot_pos
      | _ -> assert false)
  in
  iteri grid
  |> Iter.from_labelled_iter2
  |> Iter.filter_map ~f:(function
    | (x, y), Box -> Some ((100 * y) + x)
    | _ -> None)
  |> Iter.sum
;;

let rec move_west walls boxes (x, y) =
  if Hash_set.mem walls (x - 1, y)
  then None
  else if Set.mem boxes (x - 2, y)
  then (
    let boxes = Set.remove boxes (x - 2, y) in
    let boxes = Set.add boxes (x - 3, y) in
    (move_west [@tailcall]) walls boxes (x - 2, y))
  else Some boxes
;;

let rec move_east walls boxes (x, y) =
  if Hash_set.mem walls (x + 1, y)
  then None
  else if Set.mem boxes (x + 1, y)
  then (
    let boxes = Set.remove boxes (x + 1, y) in
    let boxes = Set.add boxes (x + 2, y) in
    (move_east [@tailcall]) walls boxes (x + 2, y))
  else Some boxes
;;

let rec move_vertically walls boxes boxes_to_add direction = function
  | [] -> Some (List.fold boxes_to_add ~init:boxes ~f:Set.add)
  | pos :: positions ->
    let stepped = Direction.step pos direction in
    if Hash_set.mem walls stepped
    then None
    else if Set.mem boxes stepped
    then (
      let boxes = Set.remove boxes stepped in
      (move_vertically [@tailcall])
        walls
        boxes
        (Direction.step stepped direction :: boxes_to_add)
        direction
        (stepped :: Direction.step stepped `E :: positions))
    else (
      let offset_position = Direction.step stepped `W in
      if Set.mem boxes offset_position
      then (
        let boxes = Set.remove boxes offset_position in
        (move_vertically [@tailcall])
          walls
          boxes
          (Direction.step offset_position direction :: boxes_to_add)
          direction
          (stepped :: offset_position :: positions))
      else (move_vertically [@tailcall]) walls boxes boxes_to_add direction positions)
;;

let part2 ?debug robot_pos grid moves =
  let walls = Position.Hash_set.create () in
  let boxes =
    Array.foldi grid ~init:Position.Set.empty ~f:(fun y boxes ->
      Array.foldi ~init:boxes ~f:(fun x boxes -> function
        | Box -> Set.add boxes (2 * x, y)
        | Wall ->
          Hash_set.add walls (2 * x, y);
          Hash_set.add walls ((2 * x) + 1, y);
          boxes
        | Robot | Empty -> boxes))
  in
  let robot_pos = Tuple2.map_fst robot_pos ~f:(( * ) 2) in
  let robot_pos, boxes =
    if Option.is_some debug then Mutex.lock xprint_mutex;
    Iter.fold ~init:(robot_pos, boxes) moves ~f:(fun ((robot_pos, boxes) as acc) move ->
      if Option.is_some debug then print_s [%message (move : Direction.t)];
      let maybe_new_boxes =
        match move with
        | `E -> move_east walls boxes robot_pos
        | `W -> move_west walls boxes robot_pos
        | #Direction.vertical -> move_vertically walls boxes [] move [ robot_pos ]
      in
      Option.value_map maybe_new_boxes ~default:acc ~f:(fun boxes ->
        let robot_pos = Direction.step robot_pos move in
        if Option.is_some debug
        then
          for y = 0 to height grid - 1 do
            for x = 0 to (width grid * 2) - 1 do
              if Hash_set.mem walls (x, y)
              then print_string "#"
              else if Set.mem boxes (x, y)
              then print_string "["
              else if Set.mem boxes (x - 1, y)
              then print_string "]"
              else if [%equal: Position.t] robot_pos (x, y)
              then print_string "@"
              else print_string "."
            done;
            print_string "\n"
          done;
        robot_pos, boxes))
  in
  if Option.is_some debug
  then
    for y = 0 to height grid - 1 do
      for x = 0 to (width grid * 2) - 1 do
        if Hash_set.mem walls (x, y)
        then print_string "#"
        else if Set.mem boxes (x, y)
        then print_string "["
        else if Set.mem boxes (x - 1, y)
        then print_string "]"
        else if [%equal: Position.t] robot_pos (x, y)
        then print_string "@"
        else print_string "."
      done;
      print_string "\n"
    done;
  Set.sum (module Int) boxes ~f:(fun (x, y) -> (100 * y) + x)
;;
