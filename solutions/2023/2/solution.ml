open! Aoc_std

type draw =
  { red : int
  ; green : int
  ; blue : int
  }
[@@deriving sexp, fields]

let parser =
  let open Angstrom in
  let open Let_syntax in
  let line =
    let draw =
      let any_order2 k s p1 p2 =
        s *> lift2 k p1 (option 0 (s *> p2))
        <|> s *> lift2 (Fn.flip k) p2 (option 0 (s *> p1))
        <|> return (k 0 0)
      in
      let any_order3 k s p1 p2 p3 =
        p1
        >>= (fun a -> any_order2 (fun b c -> k a b c) s p2 p3)
        <|> (p2 >>= fun b -> any_order2 (fun a c -> k a b c) s p1 p3)
        <|> (p3 >>= fun c -> any_order2 (fun a b -> k a b c) s p1 p2)
      in
      any_order3
        (fun red green blue -> { red; green; blue })
        (string ", ")
        (nat <* string " red" <* commit)
        (nat <* string " green" <* commit)
        (nat <* string " blue" <* commit)
    in
    let%mapn id = string "Game" *> spaces *> nat <* string ": " <* commit
    and draws = sep_by1 (string "; " <* commit) draw in
    id, draws
  in
  line <* (end_of_line <|> end_of_input)
;;

let part1 cursor =
  let open Parallel_iter in
  cursor
  |> of_cursor
  |> map ~f:(fun (id, draws) ->
    if
      List.for_all draws ~f:(fun { red; green; blue } ->
        red <= 12 && green <= 13 && blue <= 14)
    then id
    else 0)
  |> sum ~padded:true
;;

let power { red; green; blue } = red * green * blue

let part2 cursor =
  let open Parallel_iter in
  cursor
  |> of_cursor
  |> map ~f:(fun (_, draws) ->
    List.fold_left
      draws
      ~init:{ red = 0; green = 0; blue = 0 }
      ~f:(fun acc { red; green; blue } ->
        { red = max red acc.red; green = max green acc.green; blue = max blue acc.blue })
    |> power)
  |> sum
;;

let%expect_test "example" =
  run
  @@ fun () ->
  let cursor = parse_file_into_stream "sample.blob" parser |> tap in
  let part1 () = xprintf !"%{Int}" (part1 cursor) ~expect:(fun () -> {%expect| 8 |}) in
  let part2 () = xprintf !"%{Int}" (part2 cursor) ~expect:(fun () -> {%expect| 2286 |}) in
  fork_join_array [| part1; part2 |]
;;

let%expect_test "input" =
  run
  @@ fun () ->
  let cursor = parse_file_into_stream "input.blob" parser |> tap in
  let do_part1 () =
    xprintf !"%{Int}" (part1 cursor) ~expect:(fun () -> {%expect| 2285 |})
  in
  let do_part2 () =
    xprintf !"%{Int}" (part2 cursor) ~expect:(fun () -> {%expect| 77021 |})
  in
  fork_join_array [| do_part1; do_part2 |]
;;
