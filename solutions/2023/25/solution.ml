open! Aoc_std

let parser =
  let open Angstrom in
  let rec line map =
    let%bind key = word <* char ':' <* commit
    and data = many1_till (spaces *> word) (end_of_line <|> end_of_input)
    and at_end = at_end_of_input in
    let map = Map.update map key ~f:(Option.value_map ~default:data ~f:(( @ ) data)) in
    let map =
      List.fold data ~init:map ~f:(fun map datum ->
        Map.add_multi map ~key:datum ~data:key)
    in
    commit *> if at_end then return map else line map
  in
  line String.Map.empty
;;

module Psq = struct
  include
    Psq.Make
      (String)
      (struct
        type t = int

        let compare = Int.descending
      end)

  let[@warning "-32"] sexp_of_t t = to_list t |> [%sexp_of: (string * int) list]
end

let combine_edge edge = function
  | None -> Some edge
  | Some other_edge -> Some (edge + other_edge)
;;

exception Found_solution of int

(* adapted from https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm#Stoer%E2%80%93Wagner_minimum_cut_algorithm *)
let part1 adjacency_lists =
  let total_size = Map.length adjacency_lists in
  let adjacency_lists =
    Map.map adjacency_lists ~f:(List.map ~f:(fun datum -> datum, 1) >> Psq.of_list)
  in
  let shrink vertex_size adjacency_lists s t =
    let t_adjacencies = Psq.remove s (Map.find_exn adjacency_lists t) in
    let adjacency_lists = Map.remove adjacency_lists t in
    let adjacency_lists =
      Map.update adjacency_lists s ~f:(function
        | None -> assert false
        | Some queue ->
          let queue = Psq.remove t queue in
          Psq.fold
            (fun t_adjacent combined_edge queue ->
              Psq.update t_adjacent (combine_edge combined_edge) queue)
            queue
            t_adjacencies)
    in
    let adjacency_lists =
      Psq.fold
        (fun t_adjacent combined_edge adjacency_lists ->
          Map.update adjacency_lists t_adjacent ~f:(function
            | None -> assert false
            | Some queue ->
              let queue = Psq.remove t queue in
              Psq.update s (combine_edge combined_edge) queue))
        adjacency_lists
        t_adjacencies
    in
    let t_vertex_size = Map.find vertex_size t |> Option.value ~default:1 in
    let vertex_size = Map.remove vertex_size t in
    let vertex_size =
      Map.update
        vertex_size
        s
        ~f:(Option.value_map ~default:(1 + t_vertex_size) ~f:(( + ) t_vertex_size))
    in
    vertex_size, adjacency_lists
  in
  let phase vertex_size adjacency_lists v =
    let aux_with_last vertex_size adjacency_lists s =
      let t, weight = Option.value_exn (Psq.min (Map.find_exn adjacency_lists v)) in
      if weight = 3
      then
        raise_notrace (Found_solution (Map.find vertex_size t |> Option.value ~default:1))
      else s, t
    in
    let rec aux vertex_size adjacency_lists =
      let next =
        Psq.min (Map.find_exn adjacency_lists v)
        |> Option.value_map ~f:fst ~default:"doesnt happen"
      in
      if Map.length adjacency_lists > 3
      then (
        let vertex_size, adjacency_lists = shrink vertex_size adjacency_lists v next in
        aux vertex_size adjacency_lists)
      else (
        let vertex_size, adjacency_lists = shrink vertex_size adjacency_lists v next in
        aux_with_last vertex_size adjacency_lists next)
    in
    let s, t = aux vertex_size adjacency_lists in
    shrink vertex_size adjacency_lists s t
  in
  let rec aux vertex_size adjacency_lists v =
    match phase vertex_size adjacency_lists v with
    | exception Found_solution solution -> solution * (total_size - solution)
    | vertex_size, adjacency_lists_new -> aux vertex_size adjacency_lists_new v
  in
  aux
    String.Map.empty
    adjacency_lists
    (Option.value_exn (Map.closest_key adjacency_lists `Greater_or_equal_to "") |> fst)
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  {%expect| 54 |}
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  {%expect| 562912 |}
;;
