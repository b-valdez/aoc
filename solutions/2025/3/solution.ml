open! Aoc_std

let parser =
  let open Angstrom in
  take_while1 Char.is_digit <* (end_of_line <|> end_of_input)
;;

(* Disgustingly imperative for efficiency (premature optimisation) *)
let max_joltage n bank =
  let acc = Dynarray.create () in
  Dynarray.set_capacity acc n;
  let len = String.length bank in
  String.iteri bank ~f:(fun i c ->
    if Dynarray.length acc + (len - i) = n
    then Dynarray.add_last acc c
    else
      with_return (fun { return } ->
        for j = max 0 (n - len + i) to Dynarray.length acc - 1 do
          if Char.(Dynarray.get acc j < c)
          then (
            Dynarray.truncate acc j;
            Dynarray.add_last acc c;
            return ())
        done;
        if Dynarray.length acc < n then Dynarray.add_last acc c));
  Dynarray.fold_left (fun joltage c -> (joltage * 10) + Char.get_digit_exn c) 0 acc
;;

let part1 banks =
  let open Iter in
  banks |> map ~f:(max_joltage 2) |> sum
;;

let part2 banks =
  let open Iter in
  banks |> map ~f:(max_joltage 12) |> sum
;;
