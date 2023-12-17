open! Core

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
