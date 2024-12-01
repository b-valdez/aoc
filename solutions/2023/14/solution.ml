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
  0 -- (width grid - 1)
  |> map ~f:(fun x ->
    0 -- (height grid - 1)
    |> fold
         ~init:(0, height grid)
         ~f:(fun (sum, next_load) y ->
           match grid.^(x, y) with
           | Empty -> sum, next_load
           | Cube -> sum, height grid - y - 1
           | Round -> sum + next_load, next_load - 1)
    |> fst)
  |> sum
;;

let part2 grid =
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
  (* TODO: don't try to move already settled rocks*)
  let rec tilt direction cube_pos round_pos =
    let next_round_pos =
      Set.map
        (module Position)
        round_pos
        ~f:(fun pos ->
          let next_pos = Direction.step pos direction in
          if (not (in_grid grid next_pos))
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
  (* TODO: look into making Set.Tree.t hashable *)
  let final_round_pos =
    detect_loop_and_skip_to
      (module struct
        type t = Position.Set.t [@@deriving compare, sexp_of]

        include Position.Set.Provide_hash (Position)
      end)
      ~skip_to:1000000000
      ~init:round_pos
      ~f:(fun state ->
        List.fold spin_order ~init:state ~f:(fun state direction ->
          tilt direction cube_pos state))
  in
  Set.sum (module Int) final_round_pos ~f:(fun (_, y) -> height grid - y)
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  {%expect| 136 |};
  printf "%d" @@ part2 parsed;
  {%expect| 64 |}
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  {%expect| 111979 |};
  printf "%d" @@ part2 parsed;
  {%expect| 102055 |}
;;
