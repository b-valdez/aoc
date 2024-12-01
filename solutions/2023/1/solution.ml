open! Aoc_std

let parser =
  let open Angstrom in
  scan (None, 0) (fun acc c ->
    match c, acc with
    | '\n', _ -> None
    | ('0' .. '9' as digit), (None, _) ->
      Some (Some (Char.get_digit_exn digit), Char.get_digit_exn digit)
    | ('0' .. '9' as digit), (first, _) -> Some (first, Char.get_digit_exn digit)
    | _, state -> Some state)
  <* (end_of_line <|> end_of_input)
;;

let part1 cursor =
  let open Parallel_iter in
  cursor
  |> of_cursor
  |> map ~f:(function
    | _, (Some first, last) -> (first * 10) + last
    | _ -> 0)
  |> sum ~padded:true
;;

let string_to_digit = function
  | "1" | "one" -> 1
  | "2" | "two" -> 2
  | "3" | "three" -> 3
  | "4" | "four" -> 4
  | "5" | "five" -> 5
  | "6" | "six" -> 6
  | "7" | "seven" -> 7
  | "8" | "eight" -> 8
  | "9" | "nine" -> 9
  | other -> raise (Invalid_argument ("string_to_digit" ^ other))
;;

let regex =
  List.map
    ~f:Re.str
    [ "1"
    ; "2"
    ; "3"
    ; "4"
    ; "5"
    ; "6"
    ; "7"
    ; "8"
    ; "9"
    ; "one"
    ; "two"
    ; "three"
    ; "four"
    ; "five"
    ; "six"
    ; "seven"
    ; "eight"
    ; "nine"
    ]
  |> Re.alt
  |> Re.compile
;;

let part2 cursor =
  let open Parallel_iter in
  let mutex = Mutex.create ~padded:true () in
  cursor
  |> of_cursor
  |> map ~f:(fun (line, _) ->
    Mutex.lock ~checked:false mutex;
    let matches = Re.matches regex line in
    Mutex.unlock ~checked:false mutex;
    (10 * string_to_digit (List.hd_exn matches)) + string_to_digit (List.last_exn matches))
  |> sum ~padded:true
;;

let%expect_test "sample" =
  run (fun () ->
    let do_part1 () =
      let cursor = parse_file_into_stream "example.blob" parser |> tap in
      xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 142 |})
    in
    let do_part2 () =
      let cursor = parse_file_into_stream "example2.blob" parser |> tap in
      xprintf "%d" (part2 cursor) ~expect:(fun () -> {%expect| 281 |})
    in
    fork_join_array [| do_part1; do_part2 |])
;;

let%expect_test "part2" =
  run (fun () ->
    let cursor = parse_file_into_stream "input.blob" parser |> tap in
    let do_part1 () =
      xprintf "%d" (part1 cursor) ~expect:(fun () -> {%expect| 54561 |})
    in
    let do_part2 () =
      xprintf "%d" (part2 cursor) ~expect:(fun () -> {%expect| 54076 |})
    in
    fork_join_array [| do_part1; do_part2 |])
;;
