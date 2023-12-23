open! Aoc_std
open Grid

type cell =
  | Path
  | Forest
  | Slope of Direction.t
[@@deriving equal]

let cell_of_char = function
  | '.' -> Path
  | '#' -> Forest
  | '^' -> Slope `N
  | '>' -> Slope `E
  | 'v' -> Slope `S
  | '<' -> Slope `W
  | _ -> assert false
;;

let grid_to_paths grid =
  let crossroads =
    let%map_open.Iter x = 1 -- (width grid - 2)
    and y = 1 -- (height grid - 2) in
    if not (equal_cell grid.^(x, y) Path)
    then None
    else (
      let adjacent =
        List.rev_map Direction.all ~f:(fun direction ->
          let adjacent = Direction.step (x, y) direction in
          grid.^(adjacent))
      in
      let adjacent_path = List.count adjacent ~f:(equal_cell Path) in
      Option.some_if (adjacent_path = 0) (x, y))
  in
  let crossroads =
    crossroads
    |> Iter.keep_some
    |> Iter.to_array
    |> Position.Set.of_sorted_array_unchecked
  in
  let start =
    ( Iter.(
        1 -- (width grid - 2) |> find_pred_exn ~f:(fun x -> equal_cell grid.^(x, 0) Path))
    , 0 )
  in
  let finish =
    ( Iter.(
        1 -- (width grid - 2)
        |> find_pred_exn ~f:(fun x -> equal_cell grid.^(x, height grid - 1) Path))
    , height grid - 1 )
  in
  let crossroads = Set.add (Set.add crossroads start) finish in
  let next_direction_fun_iter =
    Iter.of_list [ Fn.id; Fun.flip Direction.turn Left; Fn.flip Direction.turn Right ]
  in
  let adjacent_crossroads crossroad =
    let rec aux length last_direction pos =
      if equal_cell grid.^(pos) (Slope (Direction.opposite last_direction))
      then None
      else (
        let next =
          let%map.Iter f = next_direction_fun_iter in
          let next_direction = f last_direction in
          let next_pos = Direction.step pos next_direction in
          if not (in_grid grid next_pos)
          then None
          else (
            let next_cell = grid.^(next_pos) in
            if equal_cell next_cell Forest then None else Some (next_direction, next_pos))
        in
        match Iter.find_map next ~f:Fn.id with
        | None -> None
        | Some (next_direction, next_pos) ->
          if Set.mem crossroads next_pos
          then Some (next_pos, length + 1)
          else aux (length + 1) next_direction next_pos)
    in
    List.rev_filter_map Direction.all ~f:(fun direction ->
      let pos = Direction.step crossroad direction in
      if in_grid grid pos && not (equal_cell grid.^(pos) Forest)
      then aux 1 direction pos
      else None)
  in
  Position.Map.of_key_set crossroads ~f:adjacent_crossroads, start, finish
;;

let parser =
  let open Angstrom in
  grid cell_of_char >>| grid_to_paths
;;

let topological_order vertices_to_next =
  let rec aux acc unvisited = function
    | [] when Set.is_empty unvisited -> acc
    | [] ->
      let visiting = Set.choose_exn unvisited in
      let next_of_visiting =
        Map.find_exn vertices_to_next visiting |> List.rev_map ~f:fst
      in
      if List.exists next_of_visiting ~f:(Set.mem unvisited)
      then (aux [@tailcall]) acc unvisited (List.rev_append next_of_visiting [ visiting ])
      else (aux [@tailcall]) (visiting :: acc) (Set.remove unvisited visiting) []
    | hd :: tl when not (Set.mem unvisited hd) -> (aux [@tailcall]) acc unvisited tl
    | visiting :: tl ->
      let next_of_visiting =
        Map.find_exn vertices_to_next visiting |> List.rev_map ~f:fst
      in
      if List.exists next_of_visiting ~f:(Set.mem unvisited)
      then
        (aux [@tailcall])
          acc
          unvisited
          (List.rev_append next_of_visiting (visiting :: tl))
      else (aux [@tailcall]) (visiting :: acc) (Set.remove unvisited visiting) tl
  in
  aux [] (Map.key_set vertices_to_next) []
;;

let longest_path vertices_to_next topological_order =
  let[@tail_mod_cons] rec aux future_lengths = function
    | [] -> []
    | hd :: tl ->
      let length, future_lengths =
        match List.Assoc.find ~equal:Position.equal future_lengths hd with
        | None -> 0, List.rev_append future_lengths (Map.find_exn vertices_to_next hd)
        | Some length ->
          let future_lengths =
            List.Assoc.remove ~equal:Position.equal future_lengths hd
          in
          let future_lengths =
            List.fold
              (Map.find_exn vertices_to_next hd)
              ~init:future_lengths
              ~f:(fun future_lengths (neighbor, dist) ->
                List.Assoc.update
                  ~equal:Position.equal
                  future_lengths
                  neighbor
                  ~f:(function
                  | None -> Some (length + dist)
                  | Some future_length -> Some (max future_length (length + dist))))
          in
          length, future_lengths
      in
      (hd, length) :: (aux [@tailcall]) future_lengths tl
  in
  aux [] topological_order
;;

(* We are assuming DAG *)
let part1 (vertices_to_next, _, finish) =
  let topological_order = topological_order vertices_to_next in
  let longest_paths = longest_path vertices_to_next topological_order in
  List.Assoc.find_exn longest_paths ~equal:Position.equal finish
;;

(* We can no longer assume DAG. The cost of walking a path is missing out on the length of all other paths, that are no longer walkable. We want to minimize this cost to maximize the length of the path *)
let part2 (vertices_to_next, start, finish) =
  let full_length =
    Map.fold vertices_to_next ~init:0 ~f:(fun ~key:_ ~data sum ->
      List.fold data ~init:(sum + 1) ~f:(fun sum (_, dist) -> sum + dist - 1))
  in
  let vertices_to_next =
    Map.mapi vertices_to_next ~f:(fun ~key:crossroad ~data ->
      Map.fold vertices_to_next ~init:data ~f:(fun ~key ~data neighbors ->
        let to_maybe_cons =
          let%map.Option dist = List.Assoc.find data ~equal:Position.equal crossroad in
          key, dist
        in
        List.maybe_cons to_maybe_cons neighbors))
  in
  let _path, opportunity_cost =
    a_star
      (List.comparator Position.comparator)
      Int.comparator
      ~step:(fun path cost ->
        let neighbors =
          List.rev_filter
            (Map.find_exn vertices_to_next (List.hd_exn path))
            ~f:(fun (neighbor, _) -> not @@ List.mem path neighbor ~equal:Position.equal)
        in
        let%map.Iter neighbor, dist = Iter.of_list neighbors in
        let opportunities =
          List.sum (module Int) neighbors ~f:(fun (_, dist) -> dist - 1)
        in
        if Position.equal finish neighbor
        then (
          let opportunities =
            opportunities
            + (Map.iteri vertices_to_next
               |> Iter.of_map_iteri
               |> Iter.filter ~f:(fun (crossroad, _) ->
                 not @@ List.mem path crossroad ~equal:Position.equal)
               |> Iter.map ~f:(fun (key, neighbors) ->
                 1
                 + List.sum
                     (module Int)
                     neighbors
                     ~f:(fun (neighbor, dist) ->
                       if Position.(neighbor < key)
                          || List.mem path neighbor ~equal:Position.equal
                       then 0
                       else dist - 1))
               |> Iter.sum)
          in
          neighbor :: path, cost + opportunities - dist + 1)
        else neighbor :: path, cost + opportunities - dist + 1)
      ~sorted_start_positions:[ [ start ], 0 ]
      ~is_goal:(function
        | hd :: _ when Position.equal hd finish -> true
        | _ -> false)
  in
  full_length - opportunity_cost
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 94 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 154 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 2402 |}];
  printf "%d" @@ part2 parsed;
  [%expect {|  6450 |}]
;;
