open! Aoc_std

type lr =
  | Left
  | Right
[@@deriving sexp]

let parser =
  let open Angstrom in
  let open Let_syntax in
  let node =
    let%mapn id = take 3 <* take 4
    and left = take 3 <* take 2
    and right = take 3 <* take 1 in
    id, (left, right)
  in
  let%mapn directions =
    many1 (char 'L' *> return Left <|> char 'R' *> return Right <* commit)
    <* end_of_line
    <* end_of_line
    <* commit
  and nodes = lines node in
  directions, nodes |> String.Map.of_alist_exn
;;

let part1 (directions, nodes) =
  let directions = Iter.of_list directions |> Iter.cycle in
  let should_continue = function
    | "ZZZ" -> `Stop
    | _ -> `Continue
  in
  Iter.fold_while directions ~init:("AAA", 0) ~f:(fun (pos, steps) ->
    let left, right = Map.find_exn nodes pos in
    function
    | Left -> (left, steps + 1), should_continue left
    | Right -> (right, steps + 1), should_continue right)
  |> snd
;;

(** Assumption, checked for the given inputs: each start leads to only one end, an end encountered after n steps will also be encountered every k * n steps*)
let part2 (directions, nodes) =
  let directions = Iter.of_list directions |> Iter.cycle in
  let should_continue next steps =
    if not @@ String.is_suffix next ~suffix:"Z"
    then (next, steps + 1), `Continue
    else (next, steps + 1), `Stop
  in
  let start_positions = Map.keys nodes |> List.filter ~f:(String.is_suffix ~suffix:"A") in
  let find_loop start =
    let _, loop_start =
      Iter.fold_while directions ~init:(start, 0) ~f:(fun (pos, steps) direction ->
        let next =
          Map.find_exn nodes pos
          |>
          match direction with
          | Left -> fst
          | Right -> snd
        in
        should_continue next steps)
    in
    loop_start
  in
  List.map start_positions ~f:find_loop |> List.fold ~init:1 ~f:lcm
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  let parsed2 = parse_string parser Sample2.sample2 in
  printf "%d" @@ part1 parsed;
  [%expect {| 2 |}];
  printf "%d" @@ part1 parsed2;
  [%expect {| 6 |}];
  printf "%d" @@ part2 @@ parse_string parser Sample3.sample3;
  [%expect {| 6 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 20777 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 13289612809129 |}]
;;
