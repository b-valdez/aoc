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

let a_star
  (type key priority)
  ?(verbose : unit option)
  (key_comparator : (key, _) Comparator.t)
  (priority_comparator : (priority, _) Comparator.t)
  ~step
  ~sorted_start_positions
  ~is_goal
  =
  let sexp_of_key = key_comparator.sexp_of_t in
  let sexp_of_priority = priority_comparator.sexp_of_t in
  let module Queue =
    Psq.Make
      (struct
        type t = key

        let compare = key_comparator.compare
      end)
      (struct
        type t = priority

        let compare = priority_comparator.compare
      end)
  in
  let rec aux visited queue =
    if Option.is_some verbose
    then print_s @@ [%sexp_of: (key * priority) list] @@ Queue.to_list queue;
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
  aux
    (Set.Using_comparator.empty ~comparator:key_comparator)
    (Queue.of_sorted_list sorted_start_positions)
;;
