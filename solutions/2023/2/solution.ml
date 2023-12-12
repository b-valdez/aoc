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
  lines line
;;

let part1 =
  List.sum
    (module Int)
    ~f:(fun (id, draws) ->
      if List.for_all draws ~f:(fun { red; green; blue } ->
           red <= 12 && green <= 13 && blue <= 14)
      then id
      else 0)
;;

let power { red; green; blue } = red * green * blue

let part2 parsed =
  List.sum
    (module Int)
    ~f:(fun (_, draws) ->
      List.fold_left
        draws
        ~init:{ red = 0; green = 0; blue = 0 }
        ~f:(fun acc { red; green; blue } ->
          { red = max red acc.red; green = max green acc.green; blue = max blue acc.blue })
      |> power)
    parsed
;;

let%expect_test "example" =
  let parsed =
    Angstrom.parse_string ~consume:All parser Sample.sample |> Result.ok_or_failwith
  in
  printf !"%{Int}" @@ part1 parsed;
  [%expect {| 8 |}];
  printf !"%{Int}" @@ part2 parsed;
  [%expect {| 2286 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf !"%{Int}" @@ part1 parsed;
  [%expect {| 2285 |}];
  printf !"%{Int}" @@ part2 parsed;
  [%expect {| 77021 |}]
;;
