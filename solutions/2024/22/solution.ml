open! Aoc_std

let parser =
  let open Angstrom in
  nat <* (end_of_line <|> end_of_input)
;;

let prune number = number land ((1 lsl 24) - 1)

let next_secret last =
  let step1 = prune @@ (last lxor (last lsl 6)) in
  let step2 = step1 lxor (step1 lsr 5) in
  prune @@ (step2 lxor (step2 lsl 11))
;;

let part1 =
  Parallel_iter.of_cursor
  >> Parallel_iter.map ~f:(Fn.apply_n_times ~n:2000 next_secret)
  >> Parallel_iter.sum ~padded:true
;;

type last_four =
  | T0
  | T1 of int
  | T2 of int * int
  | T3 of int * int * int
  | T4 of (int * int * int * int)

let remember latest = function
  | T0 -> T1 latest
  | T1 previous -> T2 (previous, latest)
  | T2 (previous2, previous1) -> T3 (previous2, previous1, latest)
  | T3 (previous3, previous2, previous1) | T4 (_, previous3, previous2, previous1) ->
    T4 (previous3, previous2, previous1, latest)
;;

module Int4 = struct
  type t = int * int * int * int [@@deriving sexp_of, hash, equal, compare]
end

let profit_of_sequence (last_four_changes, last, profit_of_sequence) =
  let current = next_secret last in
  let current_prize = current mod 10 in
  let last_four_changes = remember (current_prize - (last mod 10)) last_four_changes in
  (match last_four_changes with
   | T4 sequence ->
     Hashtbl.update profit_of_sequence sequence ~f:(function
       | None -> current_prize
       | Some existing -> existing)
   | _ -> ());
  last_four_changes, current, profit_of_sequence
;;

let part2 cursor =
  let tbl = Kcas_data.Hashtbl.create ~hashed_type:(module Int4) () in
  cursor
  |> Parallel_iter.of_cursor
  |> Parallel_iter.map ~f:(fun secret ->
    Fn.apply_n_times ~n:2000 profit_of_sequence (T0, secret, Hashtbl.create (module Int4))
    |> trd3)
  |> Parallel_iter.iter
       ~f:
         (Hashtbl.iteri ~f:(fun ~key ~data ->
            let tx ~xt =
              match Kcas_data.Hashtbl.Xt.find_opt ~xt tbl key with
              | Some existing -> Kcas_data.Hashtbl.Xt.replace ~xt tbl key (data + existing)
              | None -> Kcas_data.Hashtbl.Xt.replace ~xt tbl key data
            in
            Kcas.Xt.commit { tx }));
  Kcas_data.Hashtbl.fold
    (fun sequence profit ((_, max_profit) as acc) ->
       if profit > max_profit then sequence, profit else acc)
    tbl
    ((0, 0, 0, 0), 0)
;;
