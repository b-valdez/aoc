open! Aoc_std

let direction_of_char = function
  | 'U' -> `N
  | 'R' -> `E
  | 'D' -> `S
  | 'L' -> `W
  | _ -> assert false
;;

let direction_of_digit = function
  | 3 -> `N
  | 0 -> `E
  | 1 -> `S
  | 2 -> `W
  | _ -> assert false
;;

let parser =
  let open Angstrom in
  let line =
    let%mapn direction = any_char >>| direction_of_char
    and steps = space *> nat
    and steps2 = string " (#" *> take 5
    and direction2 = digit >>| direction_of_digit <* char ')' in
    let steps2 = Int.Hex.of_string ("0x" ^ steps2) in
    (direction, steps), (direction2, steps2)
  in
  lines line
;;

let trench =
  let map = Grid.Position.Map.empty in
  List.fold
    ~init:(map, (0, 0), 0)
    ~f:(fun (corners, pos, id) (direction, steps) ->
      let open Grid.Direction in
      let pos = Fn.apply_n_times ~n:steps (fun pos -> step pos direction) pos in
      Map.add_exn corners ~key:pos ~data:id, pos, id + 1)
  >> Tuple3.get1
;;

let lagoon_size trench =
  let min_x, min_y, max_y =
    Map.fold trench ~init:(0, 0, 0) ~f:(fun ~key:(x, y) ~data:_ (min_x, min_y, max_y) ->
      let min_x = if x < min_x then x else min_x in
      let min_y, max_y =
        if y < min_y then y, max_y else if y > max_y then min_y, y else min_y, max_y
      in
      min_x, min_y, max_y)
  in
  (* inside_ranges is (min_inclusive * max_inclusive) list*)
  let find_inside_range inside_ranges y =
    List.find inside_ranges ~f:(fun (low, high) -> Int.between ~low ~high y)
  in
  let last_index = Map.find_exn trench (0, 0) in
  let rec aux count last_x_pos inside_ranges =
    let closest_key =
      Map.closest_key trench `Greater_or_equal_to (last_x_pos + 1, min_y)
    in
    match closest_key with
    | None -> count
    | Some ((x_pos, y), _) ->
      let count =
        count
        + ((x_pos - last_x_pos)
           * List.sum (module Int) ~f:(fun (a, b) -> b - a + 1) inside_ranges)
      in
      let next_inside_ranges, count, _ =
        Map.fold_range_inclusive
          trench
          ~min:(x_pos, y)
          ~max:(x_pos, max_y)
          ~init:(inside_ranges, count, None)
          ~f:(fun ~key:(_, corner_y) ~data:id -> function
          | inside_ranges, count, Some (other_corner_y, other_id)
            when List.mem [ 1; last_index - 1 ] ~equal (abs (other_id - id)) ->
            let inside_ranges, additional_count =
              match
                ( find_inside_range inside_ranges other_corner_y
                , find_inside_range inside_ranges corner_y )
              with
              | None, None ->
                (other_corner_y, corner_y) :: inside_ranges, corner_y - other_corner_y + 1
              | Some (low, high), None ->
                ( List.update_concat
                    inside_ranges
                    ~equal:[%equal: int * int]
                    (low, high)
                    [ low, corner_y ]
                , corner_y - other_corner_y )
              | None, Some (low, high) ->
                ( List.update_concat
                    inside_ranges
                    ~equal:[%equal: int * int]
                    (low, high)
                    [ other_corner_y, high ]
                , corner_y - other_corner_y )
              | Some (low, high), Some (low', _) when low = low' ->
                ( List.update_concat
                    inside_ranges
                    ~equal:[%equal: int * int]
                    (low, high)
                    (List.filter
                       [ low, other_corner_y; corner_y, high ]
                       ~f:(fun (a, b) -> a < b))
                , 0 )
              | Some (low, low_to), Some (high_from, high) ->
                let intermediate_list =
                  List.update_concat
                    inside_ranges
                    ~equal:[%equal: int * int]
                    (low, low_to)
                    []
                in
                ( List.update_concat
                    intermediate_list
                    ~equal:[%equal: int * int]
                    (high_from, high)
                    [ low, high ]
                , corner_y - other_corner_y - 1 )
            in
            inside_ranges, count + additional_count, None
          | inside_ranges, count, _ -> inside_ranges, count, Some (corner_y, id))
      in
      (aux [@tailcall]) count x_pos next_inside_ranges
  in
  aux 0 (min_x - 1) []
;;

let part1 = List.map ~f:fst >> trench >> lagoon_size
let part2 = List.map ~f:snd >> trench >> lagoon_size

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  {%expect| 62 |};
  printf "%d" @@ part2 parsed;
  {%expect| 952408144115 |}
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  {%expect| 35991 |};
  printf "%d" @@ part2 parsed;
  {%expect| 54058824661845 |}
;;
