open! Aoc_std
open Grid

type cell =
  | Plot
  | Rock
  | Start
[@@deriving equal]

let cell_of_char = function
  | '#' -> Rock
  | '.' -> Plot
  | 'S' -> Start
  | _ -> assert false
;;

let parser =
  let open Angstrom in
  (*TODO: grid_start shouldn't assume a specific Start Variant of cell, the benefit would have been, that we could have mapped 'S' to Plot*)
  grid_start ~equal:equal_cell cell_of_char Start
;;

let ( .^() ) grid (x, y) =
  grid.(((y mod height grid) + height grid) mod height grid).(((x mod width grid)
                                                               + width grid)
                                                              mod width grid)
;;

let part1 (start, grid) iterations =
  let rec aux even_reachable odd_reachable iteration last_reached =
    if iteration = 0 || Set.is_empty last_reached
    then even_reachable
    else (
      let last_reached, even_reachable, odd_reachable =
        List.fold
          Direction.all
          ~init:(Position.Set.empty, even_reachable, odd_reachable)
          ~f:(fun (next, even_reachable, odd_reachable) direction1 ->
            let step1 =
              Position.Set.filter_map last_reached ~f:(fun pos ->
                let next_pos = Direction.step pos direction1 in
                Option.some_if
                  ((not (equal_cell grid.^(next_pos) Rock))
                   && not (Set.mem odd_reachable next_pos))
                  next_pos)
            in
            let odd_reachable = Set.union odd_reachable step1 in
            List.fold
              Direction.all
              ~init:(next, even_reachable, odd_reachable)
              ~f:(fun (next, even_reachable, odd_reachable) direction2 ->
                if Direction.equal direction1 (Direction.opposite direction2)
                then next, even_reachable, odd_reachable
                else (
                  let step2 =
                    Position.Set.filter_map step1 ~f:(fun pos ->
                      let next_pos = Direction.step pos direction2 in
                      Option.some_if
                        ((not (equal_cell grid.^(next_pos) Rock))
                         && not (Set.mem even_reachable next_pos))
                        next_pos)
                  in
                  let next = Set.union next step2 in
                  let even_reachable = Set.union even_reachable step2 in
                  next, even_reachable, odd_reachable)))
      in
      (aux [@tailcall]) even_reachable odd_reachable (iteration - 2) last_reached)
  in
  if iterations < 0
  then 0
  else
    (if iterations mod 2 = 0
     then
       aux
         (Position.Set.singleton start)
         Position.Set.empty
         iterations
         (Position.Set.singleton start)
     else (
       let one_step =
         List.filter_map Direction.all ~f:(fun direction ->
           let pos = Direction.step start direction in
           Option.some_if (equal_cell Plot grid.^(pos)) pos)
         |> Position.Set.of_list
       in
       aux (Position.Set.singleton start) one_step (iterations - 1) one_step))
    |> Set.length
;;

let steps = 26501365

let part2 (start, grid) steps =
  (* The garden is quadratic, the start in the middle *)
  assert (Tuple2.uncurry ( = ) start);
  assert (height grid = width grid);
  assert (height grid = (2 * fst start) + 1);
  (* steps is higher than the height of the grid *)
  assert (steps > height grid);
  let initial_offset = (steps mod (2 * height grid)) + (4 * height grid) in
  let steps_of_k k = initial_offset + (k * 2 * height grid) in
  let k_of_steps = (steps - initial_offset) / (2 * height grid) in
  let size_0 = part1 (start, grid) (steps_of_k 0) in
  let size_1 = part1 (start, grid) (steps_of_k 1) in
  let size_2 = part1 (start, grid) (steps_of_k 2) in
  let c = size_1 in
  let twice_a = size_0 + size_2 - (2 * c) in
  let twice_b = (2 * size_2) - twice_a - (2 * c) in
  let size_of_k k = (((twice_a * (k - 1) * (k - 1)) + (twice_b * (k - 1))) / 2) + c in
  size_of_k k_of_steps
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed 6;
  {%expect| 16 |};
  printf "%d" @@ part2 parsed 50;
  {%expect| 1594 |};
  printf "%d" @@ part2 parsed 100;
  {%expect| 6536 |};
  printf "%d" @@ part2 parsed 5000;
  {%expect| 16733044 |}
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed 64;
  {%expect| 3758 |};
  printf "%d" @@ part2 parsed steps;
  {%expect| 621494544278648 |}
;;
