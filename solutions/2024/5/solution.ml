open! Aoc_std

let parse_prefix =
  let open Angstrom in
  let line = pair ~sep:(char '|' *> commit) nat in
  sep_by end_of_line line <* end_of_line <* end_of_line
;;

let parse =
  let open Angstrom in
  sep_by (char ',') nat <* (end_of_line <|> end_of_input)
;;

let parse_file file =
  let rules, fiber = parse_file_prefix file parse_prefix in
  (* dont wait for the conversion to finish before accepting more input *)
  let ordering_map =
    Moonpool.spawn_on_current_runner (fun () -> Int.Map.of_alist_multi rules)
  in
  let updates = continue_parsing_into_stream fiber parse in
  (* set up communication between the parts *)
  let incorrect_updates = Stream.create () in
  ordering_map, tap updates, incorrect_updates, tap incorrect_updates
;;

let get_middle list =
  let length = List.length list in
  List.nth_exn list ((length - 1) / 2)
;;

let part1 (ordering_map, updates, incorrect_update_stream, _) =
  let ordering_map = Moonpool.await ordering_map in
  let return =
    Parallel_iter.of_cursor updates
    |> Parallel_iter.map ~f:(fun update ->
      let is_ordered =
        List.fold_until
          update
          ~init:None
          ~f:(fun allowed current ->
            match allowed with
            | None -> Continue (Option.some @@ Map.find_multi ordering_map current)
            | Some allowed when List.mem ~equal allowed current ->
              Continue (Option.some @@ Map.find_multi ordering_map current)
            | Some _ -> Stop false)
          ~finish:(Fun.const true)
      in
      if is_ordered
      then get_middle update
      else (
        Stream.push incorrect_update_stream update;
        0))
    |> Parallel_iter.sum ~padded:true
  in
  Stream.poison incorrect_update_stream Parallel_iter.Stream_closed;
  return
;;

(** part1 has to run before this one can finish *)
let part2 (ordering_map, _, _, incorrect_updates) =
  let ordering_map = Moonpool.await ordering_map in
  Parallel_iter.of_cursor incorrect_updates
  |> Parallel_iter.map ~f:(fun updates ->
    List.sort updates ~compare:(fun a b ->
      if a = b
      then 0
      else (
        let bigger_than_a = Map.find_multi ordering_map a in
        if List.mem bigger_than_a b ~equal then 1 else -1))
    |> get_middle)
  |> Parallel_iter.sum
;;
