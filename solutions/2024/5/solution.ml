open! Aoc_std

(* TODO better parallelity *)
let parser =
  let open Angstrom in
  let+ prefix =
    let line = pair ~sep:(char '|' *> commit) nat in
    sep_by end_of_line line
    <* end_of_line
    <* end_of_line
    <* commit
    >>| Int.Map.of_alist_multi
  and+ suffix =
    let line = sep_by (char ',') nat in
    sep_by (end_of_line *> commit) line <* end_of_input
  in
  let stream : int list Stream.t = Stream.create () in
  prefix, suffix, stream, tap stream
;;

let get_middle list =
  let length = List.length list in
  List.nth_exn list ((length - 1) / 2)
;;

let part1 (ordering_map, updates, incorrect_update_stream, _) =
  let return =
    List.sum
      (module Int)
      updates
      ~f:(fun update ->
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
  in
  Stream.poison incorrect_update_stream Parallel_iter.Stream_closed;
  return
;;

(* part1 has to run before this one can finish *)
let part2 (ordering_map, _, _, incorrect_updates) =
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
