open! Aoc_std

let parser =
  let open Angstrom in
  let%mapn pos = string "p=" *> pair ~sep:(char ',') nat
  and vel = string " v=" *> pair ~sep:(char ',') int
  and () = end_of_line <|> end_of_input in
  pos, vel
;;

let part1 width height =
  Parallel_iter.of_cursor
  >> Parallel_iter.filter_map ~f:(fun (pos, vel) ->
    let x, y = Grid.Position.(pos + (vel * 100)) in
    match ((x mod width) + width) mod width, ((y mod height) + height) mod height with
    | x, y when x < width / 2 && y < height / 2 -> Some `NW
    | x, y when x > width / 2 && y < height / 2 -> Some `NE
    | x, y when x > width / 2 && y > height / 2 -> Some `SE
    | x, y when x < width / 2 && y > height / 2 -> Some `SW
    | _ -> None)
  >> Parallel_iter.fold ~init:(0, 0, 0, 0) ~f:(fun (nw, ne, se, sw) -> function
    | `NW -> nw + 1, ne, se, sw
    | `NE -> nw, ne + 1, se, sw
    | `SE -> nw, ne, se + 1, sw
    | `SW -> nw, ne, se, sw + 1)
  >> fun (nw, ne, se, sw) -> nw * ne * se * sw
;;

let is_tree width _height bots num_bots =
  (* searching for a high amount of symmetry *)
  List.count bots ~f:(fun ((x, y), _) ->
    x = width / 2
    || (x < width / 2
        && List.exists bots ~f:(fun ((x', y'), _) ->
          (width / 2) - x = x' - (width / 2) && y = y')))
  > num_bots / 7
;;

let rec step_until_is_tree ~expect time width height bots num_bots =
  if time > lcm width height then failwith "No tree found";
  let bots =
    List.map bots ~f:(fun (pos, vel) ->
      let x, y = Grid.Position.(pos + vel) in
      (((x mod width) + width) mod width, ((y mod height) + height) mod height), vel)
  in
  if is_tree width height bots num_bots
  then
    Mutex.protect xprint_mutex (fun () ->
      printf "After %d steps:\n" (time + 1);
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          let at_pos =
            List.count bots ~f:(fun (pos, _) -> [%equal: Grid.Position.t] (x, y) pos)
          in
          if at_pos = 0 then print_string "." else printf "%d" at_pos
        done;
        print_string "\n"
      done;
      expect ())
  else step_until_is_tree ~expect (time + 1) width height bots num_bots
;;
