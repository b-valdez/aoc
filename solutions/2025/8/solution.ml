open! Aoc_std

let parser =
  let open Angstrom in
  sep_by1 (end_of_line <* commit) (triple ~sep:(char ',') nat) >>| Array.of_list
;;

let sq_dist (a, b, c) (d, e, f) =
  let open Int in
  ((a - d) ** 2) + ((b - e) ** 2) + ((c - f) ** 2)
;;

module Point = struct
  type t = int * int * int [@@deriving sexp_of, compare]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let[@tail_mod_cons] rec add_merge_circuits a b = function
  | [] -> Set.of_list (module Point) [ a; b ] :: []
  | hd :: _ as circuits when Set.mem hd a && Set.mem hd b -> circuits
  | hd :: tl when Set.mem hd a -> add_merge_circuits' hd b tl
  | hd :: tl when Set.mem hd b -> add_merge_circuits' hd a tl
  | hd :: tl -> hd :: (add_merge_circuits [@tailcall]) a b tl

and[@tail_mod_cons] add_merge_circuits' a_set b = function
  | [] -> Set.add a_set b :: []
  | hd :: tl when Set.mem hd b -> Set.union a_set hd :: tl
  | hd :: tl -> hd :: (add_merge_circuits' [@tailcall]) a_set b tl
;;

(* inspired by https://en.wikipedia.org/wiki/Closest_pair_of_points_problem#Linear-time_randomized_algorithms.
   Do not expect this one to have linear complexity however *)
(* There are 499500 pairs of point in the input.
   Sampling 20 pairs means a ~96% probability of not haven chosen one of the 1000 closest pairs *)
let part1 source junction_boxes =
  let samples, connections =
    match source with
    | Sample -> 1, 10
    | Input -> 20, 1000
  in
  let rec sample_sq_dist smallest samples =
    if samples = 0
    then smallest
    else (
      let a = Array.random_element_exn junction_boxes in
      let b = Array.random_element_exn junction_boxes in
      if [%equal: int * int * int] a b
      then sample_sq_dist smallest samples
      else sample_sq_dist (min smallest (sq_dist a b)) (samples - 1))
  in
  let reference_distance =
    sample_sq_dist Int.max_value samples |> Float.of_int |> sqrt |> Int.of_float_unchecked
  in
  let rounded =
    Array.to_sequence_mutable junction_boxes
    |> Sequence.map ~f:(fun point ->
      let key = Tuple3.map ~f:(fun coord -> coord / reference_distance) point in
      key, point)
    |> Map.of_sequence_multi (module Point)
  in
  let module Pqueue =
    Pqueue.MakeMax (struct
      type t = int * Point.t * Point.t [@@deriving compare]
    end)
  in
  let adjacencies (a, b, c) =
    [ a, b, c + 1
    ; a, b + 1, c - 1
    ; a, b + 1, c
    ; a, b + 1, c + 1
    ; a + 1, b - 1, c - 1
    ; a + 1, b - 1, c
    ; a + 1, b - 1, c + 1
    ; a + 1, b, c - 1
    ; a + 1, b, c
    ; a + 1, b, c + 1
    ; a + 1, b + 1, c - 1
    ; a + 1, b + 1, c
    ; a + 1, b + 1, c + 1
    ]
  in
  let pqueue = Pqueue.create () in
  Map.iteri rounded ~f:(fun ~key ~data:fsts ->
    Iter.diagonal_l fsts
    |> Iter.iter ~f:(fun (fst, snd) ->
      Pqueue.add pqueue (sq_dist fst snd, fst, snd);
      if Pqueue.length pqueue > connections then Pqueue.remove_max pqueue);
    List.iter (adjacencies key) ~f:(fun grid_point ->
      let snds = Map.find_multi rounded grid_point in
      let open Iter in
      product (of_list fsts) (of_list snds)
      |> iter ~f:(fun (fst, snd) ->
        Pqueue.add pqueue (sq_dist fst snd, fst, snd);
        if Pqueue.length pqueue > connections then Pqueue.remove_max pqueue)));
  if Pqueue.length pqueue < connections then failwith "Nondeterministic Error";
  let nontrivial_circuits =
    Pqueue.fold_unordered
      (fun circuits (_, a, b) -> add_merge_circuits a b circuits)
      []
      pqueue
  in
  List.fold nontrivial_circuits ~init:[||] ~f:(fun acc circuit ->
    if Array.length acc < 3
    then Array.merge acc [| Set.length circuit |] ~compare:Int.descending
    else if Set.length circuit > Array.last acc
    then
      Array.merge (Array.slice acc 0 2) [| Set.length circuit |] ~compare:Int.descending
    else acc)
  |> Array.fold ~init:1 ~f:( * )
;;

(* Prim's algorithm *)
let part2 junction_boxes =
  let adjust_distances new_point distances =
    (* Why is this so jank? Map.map_keys doesn't have access to the data *)
    Map.of_iteri_exn
      (module Int)
      ~iteri:(fun ~f ->
        Map.iteri distances ~f:(fun ~key ~data:((point, _) as data) ->
          let dist = sq_dist new_point point in
          if dist < key
          then f ~key:dist ~data:(point, fst3 point * fst3 new_point)
          else f ~key ~data))
  in
  let rec aux distances max_distance result =
    match Map.min_elt distances with
    | None -> result
    | Some (distance, (point, result)) when distance > max_distance ->
      let distances = adjust_distances point (Map.remove distances distance) in
      aux distances distance result
    | Some (distance, (point, _)) ->
      let distances = adjust_distances point (Map.remove distances distance) in
      aux distances max_distance result
  in
  let head = junction_boxes.(0) in
  aux
    (Int.Map.of_iteri_exn ~iteri:(fun ~f ->
       Array.iter (Array.slice junction_boxes 1 0) ~f:(fun box ->
         f ~key:(sq_dist head box) ~data:(box, fst3 box * fst3 head))))
    0
    0
;;
