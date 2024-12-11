open! Aoc_std

let parser =
  let open Angstrom in
  sep_by1 space (both nat (return 1)) <* end_of_input >>| Int.Map.of_alist_exn
;;

module Htbl = Saturn.Htbl

module Reverse_int = struct
  type t = int [@@deriving hash, equal, sexp_of]

  let compare = Comparable.compare_reversed compare

  include (val Comparator.make ~compare ~sexp_of_t)
end

let memo = Htbl.create ~hashed_type:(module Int) ()

let step = function
  | 0 -> Int.Map.singleton 1 1
  | stone ->
    let length = Float.iround_up_exn (Float.log10 @@ Int.to_float (stone + 1)) in
    if length mod 2 = 0
    then
      Int.Map.singleton (stone / Int.(10 ** (length / 2))) 1
      |> Fun.flip
           Map.change
           (stone mod Int.(10 ** (length / 2)))
           ~f:(function
             | None -> Some 1
             | _ -> Some 2)
    else Int.Map.singleton (stone * 2024) 1
;;

let rec atomic_set ~equal ~combine htbl key data backoff =
  let existing_data = Htbl.set_exn htbl key data in
  if not @@ equal existing_data data
  then
    atomic_set
      ~equal
      ~combine
      htbl
      key
      (combine existing_data data)
      (Backoff.once backoff)
  else data
;;

let atomic_add_to ~equal ~combine htbl key data backoff =
  if not @@ Htbl.try_add htbl key data
  then atomic_set ~equal ~combine htbl key data (Backoff.once backoff)
  else data
;;

let blink times stones =
  let rec aux stone stone_count age =
    if age = 0
    then Int.Map.singleton stone stone_count
    else (
      match Htbl.find_opt memo stone with
      | None ->
        let after_one_step = step stone in
        let to_add =
          if age = 1
          then Map.singleton (module Reverse_int) 1 (Fut.return after_one_step)
          else (
            let to_await =
              Fut.spawn_on_current_runner (fun () ->
                Map.iteri after_one_step
                |> Iter.of_map_iteri
                |> Parallel_iter.from_iter ~padded:true
                |> Parallel_iter.map ~f:(fun (stone, count) -> aux stone count (age - 1))
                |> Parallel_iter.batch_fold ~init:Int.Map.empty ~f:(fun acc batch ->
                  Array.fold
                    batch
                    ~init:acc
                    ~f:(Map.merge_skewed ~combine:(fun ~key:_ -> ( + )))))
            in
            Map.empty (module Reverse_int)
            |> Map.add_exn ~key:1 ~data:(Fut.return after_one_step)
            |> Map.add_exn ~key:age ~data:to_await)
        in
        atomic_add_to
          ~equal:[%equal: _ Fut.t Map.M(Reverse_int).t]
          ~combine:(fun existing new_ ->
            Map.merge_skewed existing new_ ~combine:(fun ~key:_ a b ->
              if Fut.is_resolved a then a else b))
          memo
          stone
          to_add
          Backoff.default
        |> Fun.flip Map.find_exn age
        |> Fut.await
        |> Map.map ~f:(fun count -> count * stone_count)
      | Some existing_futures ->
        let largest_smaller_steps, fut =
          Map.iteri existing_futures
          |> Iter.of_map_iteri
          |> Iter.find_pred_exn ~f:(fun (steps, _) -> steps <= age)
        in
        if largest_smaller_steps = age
        then Fut.await fut |> Map.map ~f:(fun count -> count * stone_count)
        else (
          let to_await =
            Fut.spawn_on_current_runner (fun () ->
              Map.iteri (Fut.await fut)
              |> Iter.of_map_iteri
              |> Parallel_iter.from_iter ~padded:true
              |> Parallel_iter.map ~f:(fun (stone, count) ->
                aux stone count (age - largest_smaller_steps))
              |> Parallel_iter.batch_fold ~padded:true ~init:Int.Map.empty ~f:(fun acc batch ->
                Array.fold
                  batch
                  ~init:acc
                  ~f:(Map.merge_skewed ~combine:(fun ~key:_ -> ( + )))))
          in
          atomic_set
            ~equal:[%equal: _ Fut.t Map.M(Reverse_int).t]
            ~combine:(fun existing new_ ->
              Map.merge_skewed existing new_ ~combine:(fun ~key:_ a b ->
                if Fut.is_resolved b then b else a))
            memo
            stone
            (Map.add_exn existing_futures ~key:age ~data:to_await)
            Backoff.default
          |> Fun.flip Map.find_exn age
          |> Fut.await
          |> Map.map ~f:(fun count -> count * stone_count)))
  in
  Map.iteri stones
  |> Iter.of_map_iteri
  |> Parallel_iter.from_iter ~padded:true
  |> Parallel_iter.map ~f:(fun (stone, count) -> aux stone count times)
  |> Parallel_iter.batch_fold ~init:Int.Map.empty ~f:(fun acc batch ->
    Array.fold batch ~init:acc ~f:(Map.merge_skewed ~combine:(fun ~key:_ -> ( + ))))
;;
