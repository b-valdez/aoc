open! Aoc_std

let parser =
  let open Angstrom in
  bi_multi_map
    (module String)
    (take 2)
    ~sep_pair:(char '-')
    ~sep_pairs:(end_of_line *> commit)
  <* end_of_input
;;

let part1 connections =
  let iter_range f =
    Map.fold_range_inclusive
      connections
      ~min:"ta"
      ~max:"tz"
      ~init:()
      ~f:(fun ~key ~data () -> f (key, data))
  in
  Parallel_iter.from_iter ~padded:true iter_range
  |> Parallel_iter.map ~f:(fun (candidate, connected) ->
    connected
    |> Set.iter
    |> Iter.from_labelled_iter
    |> Iter.filter ~f:(Fn.non @@ String.between ~low:"ta" ~high:candidate)
    |> Iter.diagonal
    |> Iter.filter_count ~f:(fun (connection1, connection2) ->
      let result = Map.find_exn connections connection1 |> Fn.flip Set.mem connection2 in
      result))
  |> Parallel_iter.sum ~padded:true
;;

(* https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm#With_pivoting *)
let part2 connections =
  let computers = Map.key_set connections in
  let maximum_clique = Kcas.Loc.make_contended (0, "") in
  let rec bron_kerbosch clique unchecked_extensions checked_extensions =
    if
      Set.length clique + Set.length unchecked_extensions
      > fst (Kcas.Loc.fenceless_get maximum_clique)
    then
      if Set.is_empty unchecked_extensions && Set.is_empty checked_extensions
      then
        Kcas.Loc.fenceless_modify
          maximum_clique
          (fun ((maximum_clique_size, _) as maximum_clique) ->
             if maximum_clique_size < Set.length clique
             then Set.length clique, Set.to_list clique |> String.concat ~sep:","
             else maximum_clique)
      else if not @@ Set.is_empty unchecked_extensions
      then (
        let pivot = (* just any random computer *) Set.choose_exn unchecked_extensions in
        Set.iter (Set.diff unchecked_extensions (Map.find_exn connections pivot))
        |> Iter.from_labelled_iter
        |> Iter.fold_map
             ~init:(unchecked_extensions, checked_extensions)
             ~f:(fun (unchecked_extensions, checked_extensions) computer ->
               let neighbors = Map.find_exn connections computer in
               let fut =
                 Moonpool.spawn_on_current_runner (fun () ->
                   bron_kerbosch
                     (Set.add clique computer)
                     (Set.inter unchecked_extensions neighbors)
                     (Set.inter checked_extensions neighbors))
               in
               let unchecked_extensions = Set.remove unchecked_extensions computer in
               let checked_extensions = Set.add checked_extensions computer in
               (unchecked_extensions, checked_extensions), fut)
        |> Iter.to_list
        |> Moonpool.Fut.join_list
        |> Moonpool.Fut.await
        |> (ignore : unit list -> unit))
  in
  bron_kerbosch String.Set.empty computers String.Set.empty;
  Kcas.Loc.fenceless_get maximum_clique |> snd
;;
