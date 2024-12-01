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
  let open Parallel_iter in
  Array.iteri grid
  |> Parallel_iter.from_labelled_iter2
  |> flat_map_l ~f:(fun (j, line) ->
    let last_acc =
      Array.foldi line ~init:(None, false, []) ~f:(fun i ->
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

let part1 parsed = Parallel_iter.sum (find_symbol_adjacent_numbers parsed)
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
  let search_number_iter i j k =
    let iter =
      Parallel_iter.singleton (search_number i j)
      |> Parallel_iter.keep_some
      |> Parallel_iter.map ~f:(fun (number, _) -> number)
    in
    iter k
  in
  let numbers_in_line dj k =
    match search_number (i - 1) (j + dj) with
    | None ->
      (match search_number i (j + dj) with
       | None -> search_number_iter (i + 1) (j + dj) k
       | Some (number, _) -> Parallel_iter.singleton number k)
    | Some (number, stop) when stop = i - 1 ->
      Parallel_iter.also number (search_number_iter (i + 1) (j + dj)) k
    | Some (number, _) -> Parallel_iter.singleton number k
  in
  let adjacent_numbers =
    Parallel_iter.combine_l
      [ search_number_iter (i - 1) j
      ; search_number_iter (i + 1) j
      ; numbers_in_line ~-1
      ; numbers_in_line 1
      ]
    |> Parallel_iter.take 3
    |> Parallel_iter.to_list
  in
  if verbose
  then
    print_s
      [%message
        "Adjacent numbers"
          (i : int)
          (j : int)
          ~adjacent_numbers:(adjacent_numbers |> List.sort ~compare : int list)];
  match adjacent_numbers with
  | [ num1; num2 ] -> num1 * num2
  | _ -> 0
;;

let part2 ?verbose grid =
  let seq k =
    Array.iteri grid ~f:(fun j -> Array.iteri ~f:(fun i symbol -> k (i, j, symbol)))
  in
  Parallel_iter.(
    from_iter seq
    |> map ~f:(function
      | i, j, Symbol '*' -> gear_ratio ?verbose grid i j
      | _ -> 0)
    |> sum)
;;

let%expect_test "sample" =
  run
  @@ fun () ->
  let parsed = parse_file "sample.blob" parser in
  let symbol_adjacent_numbers = find_symbol_adjacent_numbers parsed in
  let part0 () =
    xprintf
      !"%{sexp:int list}"
      (Parallel_iter.to_list symbol_adjacent_numbers |> List.sort ~compare)
      ~expect:(fun () -> {%expect| (35 467 592 598 617 633 664 755) |})
  in
  let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 4361 |}) in
  let part2 () =
    let part2_result, verbose_output =
      Mutex.protect xprint_mutex (fun () ->
        let part2_result = part2 ~verbose:true parsed in
        part2_result, [%expect.output])
    in
    let verbose_output =
      verbose_output |> String.split_lines |> List.sort ~compare:String.compare
    in
    Mutex.lock xprint_mutex;
    (List.iter [@inlined never]) ~f:print_endline verbose_output;
    printf "%d" part2_result;
    {%expect|
      ("Adjacent numbers" (i 3) (j 1) (adjacent_numbers (35 467)))
      ("Adjacent numbers" (i 3) (j 4) (adjacent_numbers (617)))
      ("Adjacent numbers" (i 5) (j 8) (adjacent_numbers (598 755)))
      467835
      |};
    Mutex.unlock xprint_mutex
  in
  fork_join_array [| part0; part1; part2 |]
;;

let%expect_test "input" =
  run
  @@ fun _ ->
  let parsed = parse_file "input.blob" parser in
  let part1 () = xprintf "%d" (part1 parsed) ~expect:(fun () -> {%expect| 536202 |}) in
  let part2 () = xprintf "%d" (part2 parsed) ~expect:(fun () -> {%expect| 78272573 |}) in
  fork_join_array [| part1; part2 |]
;;
