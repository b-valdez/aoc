open! Aoc_std

type card =
  | Number of int
  | T
  | J
  | Q
  | K
  | A
[@@deriving compare, equal, hash, sexp]

type hand_type =
  | High_card
  | Pair
  | Two_pair
  | Triple
  | Full_house
  | Quadruple
  | Quintuple
[@@deriving compare, sexp]

type 'a quintuple = 'a * 'a * 'a * 'a * 'a [@@deriving compare]

let iter_quintuple (a1, a2, a3, a4, a5) k =
  k a1;
  k a2;
  k a3;
  k a4;
  k a5
;;

let quintuple_of_list = function
  | [ a1; a2; a3; a4; a5 ] -> a1, a2, a3, a4, a5
  | _ -> invalid_arg "not a quintuple"
;;

let parser =
  let open Angstrom in
  let card =
    let card_of_char = function
      | '0' .. '9' as c -> Number (Char.get_digit_exn c)
      | 'T' -> T
      | 'J' -> J
      | 'Q' -> Q
      | 'K' -> K
      | 'A' -> A
      | _ -> assert false
    in
    satisfy (function
      | '0' .. '9' | 'T' | 'J' | 'Q' | 'K' | 'A' -> true
      | _ -> false)
    >>| card_of_char
    <?> "card"
  in
  let hand = count 5 card <* char ' ' >>| quintuple_of_list <* commit in
  let bet = nat in
  both hand bet <* (end_of_line <* commit <|> end_of_input)
;;

let part1 =
  let hand_type =
    iter_quintuple
    >> Iter.count ~hash:[%hash: card] ~eq:[%equal: card]
    >> Iter.sort ~cmp:(Comparable.compare_reversed @@ [%compare: _ * int])
    >> Iter.to_list
    >> function
    | [ (_, 5) ] -> Quintuple
    | [ (_, 4); _ ] -> Quadruple
    | [ (_, 3); (_, 2) ] -> Full_house
    | [ (_, 3); _; _ ] -> Triple
    | [ (_, 2); (_, 2); _ ] -> Two_pair
    | [ (_, 2); _; _; _ ] -> Pair
    | _ -> High_card
  in
  Parallel_iter.of_cursor
  >> Parallel_iter.map ~f:(fun (hand, bet) -> hand_type hand, hand, bet)
  >> Parallel_iter.sort ~padded:true ~compare:[%compare: hand_type * card quintuple * _]
  >> Array.foldi ~init:0 ~f:(fun i sum (_, _, bet) -> sum + ((i + 1) * bet))
;;

let part2 =
  let compare_card card1 card2 =
    match card1, card2 with
    | J, J -> 0
    | J, _ -> -1
    | _, J -> 1
    | card1, card2 -> compare_card card1 card2
  in
  let hand_type =
    iter_quintuple
    >> Iter.count ~hash:[%hash: card] ~eq:[%equal: card]
    >> Iter.sort
         ~cmp:
           (Comparable.compare_reversed
            @@ Comparable.lift [%compare: int * card] ~f:Tuple2.swap)
    >> Iter.to_list
    >> function
    | [ (_, 5) ] | [ (J, _); _ ] | [ _; (J, _) ] -> Quintuple
    | [ (_, 4); _ ] | [ (J, _); _; _ ] | [ _; (J, _); _ ] | [ (_, 3); _; (J, _) ] ->
      Quadruple
    | [ (_, 3); (_, 2) ] | [ _; _; (J, _) ] -> Full_house
    | [ (_, 3); _; _ ] | [ (_, 2); _; _; (J, _) ] | [ (J, 2); _; _; _ ] -> Triple
    | [ (_, 2); (_, 2); _ ] -> Two_pair
    | [ (_, 2); _; _; _ ] | [ _; _; _; _; (J, _) ] -> Pair
    | _ -> High_card
  in
  Parallel_iter.of_cursor
  >> Parallel_iter.map ~f:(fun (hand, bet) -> hand_type hand, hand, bet)
  >> Parallel_iter.sort ~padded:true ~compare:[%compare: hand_type * card quintuple * _]
  >> Array.foldi ~init:0 ~f:(fun i sum (_, _, bet) -> sum + ((i + 1) * bet))
;;

let%expect_test "sample" =
  run
  @@ fun _ ->
  let cursor = parse_file_into_stream "sample.blob" parser |> tap in
  let part1 () = xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 6440 |}) in
  let part2 () = xprintf "%d" (part2 cursor) ~expect:(fun () -> {%expect| 5905 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  run
  @@ fun _ ->
  let cursor = parse_file_into_stream "input.blob" parser |> tap in
  let part1 () = xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 250347426 |}) in
  let part2 () = xprintf "%d" (part2 cursor) ~expect:(fun () -> {%expect| 251224870 |}) in
  fork_join_array [| part1; part2 |]
;;
