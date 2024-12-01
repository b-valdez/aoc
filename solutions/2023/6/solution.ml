open! Aoc_std

let parse1 =
  let open Angstrom in
  let%mapn times = string "Time:" *> spaces *> sep_by spaces nat <* end_of_line
  and distances = string "Distance:" *> spaces *> sep_by spaces nat in
  List.zip_exn times distances
;;

let parse2 =
  let open Angstrom in
  let%mapn time =
    string "Time:" *> many1 (spaces *> take_while1 Char.is_digit)
    <* end_of_line
    >>| String.concat
    >>| int_of_string
  and distance =
    string "Distance:" *> many1 (spaces *> take_while1 Char.is_digit)
    >>| String.concat
    >>| int_of_string
  in
  [ time, distance ]
;;

let solve =
  let module Mul = struct
    type t = int

    let zero = 1
    let ( + ) = ( * )
  end
  in
  List.sum
    (module Mul)
    ~f:(fun (duration, record) ->
      let lower_bound =
        let open Iter in
        1 -- (duration / 2)
        |> find_pred_exn ~f:(fun press -> press * (duration - press) > record)
      in
      duration - lower_bound + 1 - lower_bound)
;;

let%expect_test "sample" =
  let sample = Stdio.In_channel.read_all "sample.blob" in
  let part1 () =
    xprintf "%d" (solve (parse_string parse1 sample)) ~expect:(fun () -> {%expect| 288 |})
  in
  let part2 () =
    xprintf
      "%d"
      (solve (parse_string parse2 sample))
      ~expect:(fun () -> {%expect| 71503 |})
  in
  run @@ fun () -> fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  let input = Stdio.In_channel.read_all "input.blob" in
  let part1 () =
    xprintf
      "%d"
      (solve (parse_string parse1 input))
      ~expect:(fun () -> {%expect| 588588 |})
  in
  let part2 () =
    xprintf
      "%d"
      (solve (parse_string parse2 input))
      ~expect:(fun () -> {%expect| 34655848 |})
  in
  run @@ fun () -> fork_join_array [| part1; part2 |]
;;
