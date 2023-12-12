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
  let line =
    let%map hand = count 5 card <* char ' '
    and bet = nat in
    hand, bet
  in
  lines line
;;

let part1 =
  let hand_type =
    Iter.of_list
    >> Iter.count ~hash:[%hash: card] ~eq:[%equal: card]
    >> Iter.sort ~cmp:(Comparable.compare_reversed @@ Comparable.lift compare_int ~f:snd)
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
  List.map ~f:(fun (hand, bet) -> hand_type hand, hand, bet)
  >> List.sort ~compare:[%compare: hand_type * card list * _]
  >> List.foldi ~init:0 ~f:(fun i sum (_, _, bet) -> sum + ((i + 1) * bet))
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
    Iter.of_list
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
  List.map ~f:(fun (hand, bet) -> hand_type hand, hand, bet)
  >> List.sort ~compare:[%compare: hand_type * card list * _]
  >> List.foldi ~init:0 ~f:(fun i sum (_, _, bet) -> sum + ((i + 1) * bet))
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" (part1 parsed);
  [%expect {| 6440 |}];
  printf "%d" (part2 parsed);
  [%expect {| 5905 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" (part1 parsed);
  [%expect {| 250347426 |}];
  printf "%d" (part2 parsed);
  [%expect {| 251224870 |}]
;;
