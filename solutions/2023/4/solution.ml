open! Aoc_std

let parser =
  let open Angstrom in
  many1
  @@
  let%mapn winning =
    string "Card"
    *> spaces
    *> nat
    *> char ':'
    *> commit
    *> many_unique_till (module String) (take 3) (string " |" <* commit)
  and gotten = many_till (take 3) (end_of_line <|> end_of_input <* commit) in
  List.count gotten ~f:(Set.mem winning)
;;

let part1 =
  List.sum
    (module Int)
    ~f:(function
      | 0 -> 0
      | n -> 1 lsl (n - 1))
;;

let part2 =
  let[@tail_mod_cons] rec add_future_winnings so_far to_be_added times =
    if to_be_added = 0
    then so_far
    else (
      match so_far with
      | [] -> List.init to_be_added ~f:(Fn.const times)
      | hd :: tl ->
        (hd + times) :: (add_future_winnings [@tailcall]) tl (to_be_added - 1) times)
  in
  let f = function
    | sum, [] -> fun winnings -> sum + 1, add_future_winnings [] winnings 1
    | sum, hd :: tl ->
      fun winnings -> sum + 1 + hd, add_future_winnings tl winnings (1 + hd)
  in
  List.fold ~init:(0, []) ~f >> fst
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" (part1 parsed);
  [%expect {| 13 |}];
  printf "%d" (part2 parsed);
  [%expect {| 30 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" (part1 parsed);
  [%expect {| 21213 |}];
  printf "%d" (part2 parsed);
  [%expect {| 8549735 |}]
;;
