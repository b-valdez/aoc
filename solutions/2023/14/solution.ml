open! Aoc_std
open Grid

type rock =
  | (* O *) Round
  | (* # *) Cube
  | (* . *) Empty

let rock_of_char = function
  | 'O' -> Round
  | '#' -> Cube
  | '.' -> Empty
  | _ -> assert false
;;

let parser =
  let open Angstrom in
  grid rock_of_char
;;

let part1 grid =
  let open Iter in
  let grid_height = Array.length grid in
  let grid_width = Array.length grid.(0) in
  0 -- (grid_width - 1)
  |> map ~f:(fun x ->
    0 -- (grid_height - 1)
    |> fold ~init:(0, grid_height) ~f:(fun (sum, next_load) y ->
      match grid.^(x, y) with
      | Empty -> sum, next_load
      | Cube -> sum, grid_height - y - 1
      | Round -> sum + next_load, next_load - 1)
    |> fst)
  |> sum
;;

let part2 grid =
  let grid_height = Array.length grid in
  let grid_width = Array.length grid.(0) in
  let round_pos, cube_pos =
    Array.foldi
      grid
      ~init:(Set.empty (module Position), Set.empty (module Position))
      ~f:(fun y init ->
        Array.foldi ~init ~f:(fun x (round_pos, square_pos) ->
          function
          | Empty -> round_pos, square_pos
          | Cube -> round_pos, Set.add square_pos (x, y)
          | Round -> Set.add round_pos (x, y), square_pos))
  in
  let in_grid (x, y) =
    Int.between ~low:0 ~high:(grid_width - 1) x
    && Int.between ~low:0 ~high:(grid_height - 1) y
  in
  let rec tilt direction cube_pos round_pos =
    let next_round_pos =
      Set.map
        (module Position)
        round_pos
        ~f:(fun pos ->
          let next_pos = Direction.step pos direction in
          if (not (in_grid next_pos))
             || Set.mem cube_pos next_pos
             || Set.mem round_pos next_pos
          then pos
          else next_pos)
    in
    if Set.equal round_pos next_round_pos
    then round_pos
    else (tilt [@tailcall]) direction cube_pos next_round_pos
  in
  let spin_order = [ `N; `W; `S; `E ] in
  let rec spin times cube_pos visited round_pos round =
    (* TODO promote loop detection to Aoc_Std *)
    let after_spin =
      List.fold spin_order ~init:round_pos ~f:(fun round_pos direction ->
        tilt direction cube_pos round_pos)
    in
    match Map.add visited ~key:(Set.to_tree after_spin) ~data:round with
    | `Ok visited -> (spin [@tailcall]) times cube_pos visited after_spin (round + 1)
    | `Duplicate ->
      let visited_at = Map.find_exn visited (Set.to_tree after_spin) in
      let loop_length = round - visited_at in
      let last_equivalent = ((times - round) mod loop_length) + visited_at in
      (* TODO promote: Iter.of_map*)
      Map.iteri visited
      |> fun g ->
      (fun consume -> g ~f:(fun ~key ~data -> consume (key, data)))
      |> Iter.find_pred ~f:(fun (_, round) -> round = last_equivalent)
      |> Option.value_exn
      |> fst
      |> Set.of_tree (module Position)
  in
  let module Tree_comparator = struct
    include Set.Make_tree (Position)
    include (val Comparator.make ~compare ~sexp_of_t)
  end
  in
  let final_round_pos =
    spin 1000000000 cube_pos (Map.empty (module Tree_comparator)) round_pos 1
  in
  Set.sum (module Int) final_round_pos ~f:(fun (_, y) -> grid_height - y)
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 136 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 64 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 111979 |}];
  printf "%d" @@ part2 parsed;
  [%expect{| 102055 |}]
;;
