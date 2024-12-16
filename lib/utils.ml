open! Core

let detect_loop_with_affine_accumulator
      ?size
      state_module
      ~f
      ~init_state
      ~init_acc
      ~skip_to
      ~compute_final_accumulator
  =
  let visited = Hashtbl.create ?size state_module in
  let rec aux iteration current current_accumulator =
    let next, next_accumulator = f current current_accumulator in
    if iteration = skip_to
    then next, next_accumulator
    else (
      match Hashtbl.add visited ~key:next ~data:(iteration, next_accumulator) with
      | `Ok -> (aux [@tailcall]) (iteration + 1) next next_accumulator
      | `Duplicate ->
        let visited_at, prev_accumulator = Hashtbl.find_exn visited next in
        let loop_length = iteration - visited_at in
        let state, (visited_at, found_in_loop) =
          Hashtbl.iteri visited
          |> Iter.of_map_iteri
          |> Iter.find_pred_exn ~f:(fun (_, (iteration, _)) ->
            iteration = ((skip_to - visited_at) mod loop_length) + visited_at)
        in
        ( state
        , compute_final_accumulator
            ~found_in_loop
            ~loops:((skip_to - visited_at) / loop_length)
            ~before_loop:prev_accumulator
            ~after_loop:next_accumulator ))
  in
  Hashtbl.add_exn visited ~key:init_state ~data:(0, init_acc);
  aux 1 init_state init_acc
;;

let detect_loop_and_skip_to_mapped ?size ~map ~unmap state_module ~f ~init ~skip_to =
  let visited = Hashtbl.create ?size state_module in
  let rec aux iteration current =
    let next = f current in
    match Hashtbl.add visited ~key:(map next) ~data:iteration with
    | `Ok -> (aux [@tailcall]) (iteration + 1) next
    | `Duplicate ->
      let visited_at = Hashtbl.find_exn visited (map next) in
      let loop_length = iteration - visited_at in
      Hashtbl.iteri visited
      |> Iter.of_map_iteri
      |> Iter.find_pred_exn ~f:(fun (_, iteration) ->
        iteration = ((skip_to - visited_at) mod loop_length) + visited_at)
      |> fst
      |> unmap
  in
  Hashtbl.add_exn visited ~key:(map init) ~data:0;
  aux 1 init
;;

let detect_loop_and_skip_to = detect_loop_and_skip_to_mapped ~map:Fn.id ~unmap:Fn.id

module type Comparable_sexpable = sig
  type t [@@deriving compare, sexp_of]

  include Comparator.S with type t := t
end

module type Comparable_sexpable_summable = sig
  type t [@@deriving compare, sexp_of]

  include Container.Summable with type t := t
end

let dijkstra
      (type key cost)
      ?(verbose : unit option)
      (module Key : Comparable_sexpable with type t = key)
      (module Cost : Comparable_sexpable with type t = cost)
      ~step
      ~sorted_start_positions
      ~is_goal
  =
  let module Queue = Psq.Make (Key) (Cost) in
  let rec aux visited queue =
    if Option.is_some verbose
    then print_s @@ [%sexp_of: (Key.t * Cost.t) list] @@ Queue.to_list queue;
    match Queue.pop queue with
    | None -> assert false
    | Some ((state, priority), _) when is_goal state -> state, priority
    | Some ((state, priority), rest) ->
      step state priority
      |> Iter.filter ~f:(fun (state, _) -> not @@ Set.mem visited state)
      |> Iter.fold ~init:rest ~f:(fun rest (state, priority) ->
        Queue.push state priority rest)
      |> (aux [@tailcall]) (Set.add visited state)
  in
  aux (Set.empty (module Key)) (Queue.of_sorted_list sorted_start_positions)
;;

let a_star
      (type key cost)
      ?(verbose : unit option)
      (module Key : Comparable_sexpable with type t = key)
      (module Cost : Comparable_sexpable_summable with type t = cost)
      ~heuristic
      ~step
      ~start_positions
      ~is_goal
  =
  let module Queue =
    Psq.Make
      (struct
        type t = Key.t * (Cost.t[@compare.ignore]) [@@deriving compare]
      end)
      (Cost)
  in
  let rec aux visited queue =
    if Option.is_some verbose
    then print_s @@ [%sexp_of: ((Key.t * Cost.t) * Cost.t) list] @@ Queue.to_list queue;
    match Queue.pop queue with
    | None -> assert false
    | Some ((((state, _) as state_with_cost), _), _) when is_goal state -> state_with_cost
    | Some (((state, cost), _), rest) ->
      step state cost
      |> Iter.filter ~f:(fun (state, _) -> not @@ Set.mem visited state)
      |> Iter.fold ~init:rest ~f:(fun rest (state, cost) ->
        Queue.push (state, cost) Cost.(cost + heuristic state) rest)
      |> (aux [@tailcall]) (Set.add visited state)
  in
  aux
    (Set.empty (module Key))
    (Queue.of_list
       (List.map start_positions ~f:(fun ((state, cost) as state_with_cost) ->
          state_with_cost, Cost.(cost + heuristic state))))
;;

type 'a branching_paths =
  | Branching of
      { shared_end : 'a list
      ; branches : 'a branching_paths list
      ; shared_rev_start : 'a list
      }
  | Non_branching of 'a list
[@@deriving sexp, compare]

let append_to_branching_paths paths el =
  match paths with
  | Branching paths -> Branching { paths with shared_end = el :: paths.shared_end }
  | Non_branching paths -> Non_branching (el :: paths)
;;

let shared_prefix ~compare a b =
  let rec aux acc = function
    | el :: a, el' :: b when compare el el' = 0 -> (aux [@tailcall]) (el :: acc) (a, b)
    | a, b -> List.rev acc, a, b
  in
  aux [] (a, b)
;;

let end_of = function
  | Branching { shared_end; _ } -> shared_end
  | Non_branching all -> all
;;

let rev_start_of = function
  | Branching { shared_rev_start; _ } -> shared_rev_start
  | Non_branching all -> List.rev all
;;

let branches_of = function
  | Branching { branches; _ } -> branches
  | Non_branching _ -> []
;;

let branching_paths_of shared_end branches shared_rev_start =
  match shared_end, branches, shared_rev_start with
  | shared_end, [], shared_rev_start ->
    Non_branching (List.rev_append shared_end shared_rev_start)
  | shared_end, branches, shared_rev_start ->
    Branching { shared_end; branches; shared_rev_start }
;;

(* TODO could be nicer by having branches of more than 2 elements. I don't have the brainpower right now. *)
let combine_paths (type t) ~compare a b =
  let shared_end, end_a, end_b = shared_prefix ~compare (end_of a) (end_of b) in
  let shared_rev_start, start_a, start_b =
    shared_prefix ~compare (rev_start_of a) (rev_start_of b)
  in
  Branching
    { shared_end
    ; shared_rev_start
    ; branches =
        [ branching_paths_of end_a (branches_of a) start_a
        ; branching_paths_of end_b (branches_of b) start_b
        ]
    }
;;

let a_star_all_paths
      (type key cost)
      ?(verbose : unit option)
      (module Key : Comparable_sexpable with type t = key)
      (module Cost : Comparable_sexpable_summable with type t = cost)
      ~heuristic
      ~step
      ~start_positions
      ~is_goal
  =
  let module Queue =
    Psq.Make
      (struct
        type t = Key.t * (Cost.t[@compare.ignore]) [@@deriving compare]
      end)
      (Cost)
  in
  let rec aux best_paths visited queue =
    if Option.is_some verbose
    then print_s @@ [%sexp_of: ((Key.t * Cost.t) * Cost.t) list] @@ Queue.to_list queue;
    match Queue.pop queue with
    | None -> assert false
    | Some (((state, cost), _), _) when is_goal state ->
      state, cost, Map.find_exn best_paths state
    | Some (((state, cost), _), rest) ->
      let best_paths_to_state = Map.find_exn best_paths state in
      let queue, best_paths =
        step state cost
        |> Iter.fold
             ~init:(rest, best_paths)
             ~f:(fun (rest, best_paths) (next_state, cost) ->
               let priority = Cost.(cost + heuristic next_state) in
               match Queue.find (next_state, cost) rest with
               | Some p when Cost.compare priority p > 0 -> rest, best_paths
               | Some p when Cost.compare priority p < 0 ->
                 ( Queue.add (next_state, cost) priority rest
                 , Map.set
                     best_paths
                     ~key:next_state
                     ~data:(append_to_branching_paths best_paths_to_state next_state) )
               | None when not @@ Set.mem visited next_state ->
                 ( Queue.add (next_state, cost) priority rest
                 , Map.set
                     best_paths
                     ~key:next_state
                     ~data:(append_to_branching_paths best_paths_to_state next_state) )
               | _ ->
                 let known_paths = Map.find_exn best_paths next_state in
                 ( rest
                 , Map.set
                     best_paths
                     ~key:next_state
                     ~data:
                       (combine_paths ~compare:Key.compare known_paths
                        @@ append_to_branching_paths best_paths_to_state next_state) ))
      in
      (aux [@tailcall]) best_paths (Set.add visited state) queue
  in
  aux
    (Map.of_alist_exn
       (module Key)
       (List.map start_positions ~f:(fun (position, _) ->
          position, Non_branching [ position ])))
    (Set.empty (module Key))
    (Queue.of_list
       (List.map start_positions ~f:(fun ((state, cost) as state_with_cost) ->
          state_with_cost, Cost.(cost + heuristic state))))
;;
