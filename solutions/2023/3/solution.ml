open! Aoc_std
open Grid

type symbol =
  | Symbol of char
  | Digit of int
  | Empty
[@@deriving variants]

let parser =
  let symbol_of_char = function
    | '0' .. '9' as d -> Digit (Char.get_digit_exn d)
    | '.' -> Empty
    | c -> Symbol c
  in
  Angstrom.grid symbol_of_char
;;

let is_some_symbol = Option.exists ~f:is_symbol

let is_symbol_above_below grid i j =
  is_some_symbol grid.?(i, j - 1) || is_some_symbol grid.?(i, j + 1)
;;

let find_symbol_adjacent_numbers grid =
  Array.foldi grid ~init:[] ~f:(fun j acc line ->
    let last_acc =
      Array.foldi line ~init:(None, false, acc) ~f:(fun i ->
          function
          | None, is_adjacent, acc ->
            (function
              | Symbol _ -> None, true, acc
              | Digit d ->
                let is_adjacent = is_adjacent || is_symbol_above_below grid i j in
                Some d, is_adjacent, acc
              | Empty -> None, is_symbol_above_below grid i j, acc)
          | Some n, is_adjacent, acc ->
            (function
              | Symbol _ -> None, true, n :: acc
              | Empty ->
                let next_adjacent = is_symbol_above_below grid i j in
                ( None
                , next_adjacent
                , if is_adjacent || next_adjacent then n :: acc else acc )
              | Digit d ->
                Some ((10 * n) + d), is_adjacent || is_symbol_above_below grid i j, acc))
    in
    match last_acc with
    | Some n, true, acc -> n :: acc
    | _, _, acc -> acc)
;;

let part1 parsed =
  List.sum (module Core.Int) (find_symbol_adjacent_numbers parsed) ~f:Fn.id
;;

let is_some_digit grid i j = Option.exists grid.?(i, j) ~f:is_digit

let gear_ratio ?(verbose = false) grid i j =
  let search_number i j =
    if is_some_digit grid i j
    then (
      let stop =
        Iter.init ~f:(( + ) i)
        |> Iter.find_pred_exn ~f:(fun i -> not (is_some_digit grid (i + 1) j))
      in
      let start =
        Iter.init ~f:(( - ) i)
        |> Iter.find_pred_exn ~f:(fun i -> not (is_some_digit grid (i - 1) j))
      in
      Iter.int_range ~start ~stop
      |> Iter.fold
           ~f:(fun number i ->
             (number * 10) + (grid.^(i, j) |> digit_val |> Option.value_exn))
           ~init:0
      |> fun number -> Some (number, stop))
    else None
  in
  let search_number_list i j =
    search_number i j |> Option.fold ~init:[] ~f:(fun _ (number, _) -> [ number ])
  in
  let numbers_in_line dj =
    match search_number (i - 1) (j + dj) with
    | None ->
      (match search_number i (j + dj) with
       | None -> search_number_list (i + 1) (j + dj)
       | Some (number, _) -> [ number ])
    | Some (number, stop) when stop = i - 1 ->
      number :: search_number_list (i + 1) (j + dj)
    | Some (number, _) -> [ number ]
  in
  (* Could be optimized (if we already have 3 numbers, we don't need to find a 4th) *)
  let adjacent_numbers =
    List.concat_no_order
      [ search_number_list (i - 1) j
      ; search_number_list (i + 1) j
      ; numbers_in_line ~-1
      ; numbers_in_line 1
      ]
  in
  if verbose
  then
    print_s
      [%message "Adjacent numbers" (i : int) (j : int) (adjacent_numbers : int list)];
  match adjacent_numbers with
  | [ num1; num2 ] -> num1 * num2
  | _ -> 0
;;

let part2 ?verbose grid =
  Array.foldi grid ~init:0 ~f:(fun j sum ->
    Array.foldi ~init:sum ~f:(fun i sum ->
      function
      | Symbol '*' -> sum + gear_ratio ?verbose grid i j
      | _ -> sum))
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  let symbol_adjacent_numbers = find_symbol_adjacent_numbers parsed in
  printf !"%{sexp:int list}" (List.rev symbol_adjacent_numbers);
  [%expect {| (467 35 633 617 592 755 664 598) |}];
  printf "%d" (part1 parsed);
  [%expect {| 4361 |}];
  printf "%d" (part2 ~verbose:true parsed);
  [%expect
    {|
    ("Adjacent numbers" (i 3) (j 1) (adjacent_numbers (35 467)))
    ("Adjacent numbers" (i 3) (j 4) (adjacent_numbers (617)))
    ("Adjacent numbers" (i 5) (j 8) (adjacent_numbers (598 755)))
    467835 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" (part1 parsed);
  [%expect {| 536202 |}];
  printf "%d" (part2 parsed);
  [%expect {| 78272573 |}]
;;
