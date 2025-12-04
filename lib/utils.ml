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

let a_star_all_paths
      (type key cost)
      ?(verbose : unit option)
      (module Key : Hashtbl.Key_plain with type t = key)
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
  let predecessors = Hashtbl.create (module Key) in
  List.iter start_positions ~f:(fun (state, cost) ->
    Hashtbl.add_exn predecessors ~key:state ~data:(cost, []));
  let rec aux visited queue =
    if Option.is_some verbose
    then print_s @@ [%sexp_of: ((Key.t * Cost.t) * Cost.t) list] @@ Queue.to_list queue;
    match Queue.pop queue with
    | None -> assert false
    | Some (((state, cost), _), _) when is_goal state -> state, cost, predecessors
    | Some (((state, cost), _), rest) ->
      let queue =
        step state cost
        |> Iter.filter ~f:(fun (next_state, cost) ->
          Hashtbl.update predecessors next_state ~f:(function
            | None -> cost, [ state ]
            | Some (greater_cost, _) when Cost.compare greater_cost cost > 0 ->
              cost, [ state ]
            | Some (equal_cost, known_predecessors) when Cost.compare equal_cost cost = 0
              -> cost, state :: known_predecessors
            | Some existing_better -> existing_better);
          not @@ Set.mem visited state)
        |> Iter.fold ~init:rest ~f:(fun rest (next_state, cost) ->
          Queue.push (next_state, cost) Cost.(cost + heuristic next_state) rest)
      in
      (aux [@tailcall]) (Set.add visited state) queue
  in
  aux
    (Set.empty
       (module struct
         type t = Key.t

         include Comparator.Make (Key)
       end))
    (Queue.of_list
       (List.map start_positions ~f:(fun ((state, cost) as state_with_cost) ->
          state_with_cost, Cost.(cost + heuristic state))))
;;

let rec until_stable ~equal ~f ~init =
  let next = f init in
  if equal init next then next else until_stable ~equal ~f ~init:next
;;
