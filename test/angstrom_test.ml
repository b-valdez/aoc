let%expect_test "angstrom" =
  let parser = Angstrom.(both (string "ab" <|> string "a") (string "bc")) in
  Angstrom.parse_string ~consume:Prefix parser "abcd"
  |> Result.fold ~ok:(fun (a, b) -> Printf.sprintf {|Got "%s" "%s"|} a b) ~error:Fun.id
  |> print_endline;
  {%expect| : string |};
  let parser =
    Angstrom.(both (string "ab") (string "bc") <|> both (string "a") (string "bc"))
  in
  Angstrom.parse_string ~consume:Prefix parser "abcd"
  |> Result.fold ~ok:(fun (a, b) -> Printf.sprintf {|Got "%s" "%s"|} a b) ~error:Fun.id
  |> print_endline;
  {%expect| Got "a" "bc" |}
;;
