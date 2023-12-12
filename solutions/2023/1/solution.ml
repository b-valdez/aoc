open! Aoc_std
open Example
open Example2
open Input

let part1 input =
  let open String in
  split_lines input
  |> List.sum
       (module Int)
       ~f:(fun line ->
         let iterator = iter line in
         let open Iter in
         let filtered = filter_map ~f:Char.get_digit (from_labelled_iter iterator) in
         let first = head_exn filtered in
         let last = fold ~f:(fun _ -> Fn.id) ~init:first filtered in
         (first * 10) + last)
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

let part2 input =
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
  in
  String.split_lines input
  |> List.sum
       (module Int)
       ~f:(fun line ->
         let matches = Re.matches regex line in
         (10 * string_to_digit (List.hd_exn matches))
         + string_to_digit (List.last_exn matches))
;;

let%expect_test "part1" =
  printf "%d" @@ part1 example;
  [%expect {| 142 |}];
  printf "%d" @@ part1 input;
  [%expect {| 54561 |}]
;;

let%expect_test "part2" =
  printf "%d" @@ part2 example2;
  [%expect {| 281 |}];
  printf "%d" @@ part2 input;
  [%expect {| 54076 |}]
;;
