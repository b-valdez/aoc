open! Aoc_std

type status =
  | (* # *) Damaged
  | (* . *) Operational
  | (* ? *) Unknown
[@@deriving equal]

let parser =
  let open Angstrom in
  let line =
    let%mapn springs =
      many_till
        (char '#' *> return Damaged
         <|> char '.' *> return Operational
         <|> char '?' *> return Unknown)
        space
    and groups = sep_by1 (char ',') nat in
    springs, groups
  in
  lines line
;;

let part1 =
  let open struct
    type traversal_state =
      { remaining_groups : int list
      ; in_group : bool
      }
    [@@deriving compare]
  end in
  (* given the possible leftover groups, and if we are in the middle of a block right now,
     and how many ways we can accomplish each of those possibilities, how many possibilities are there? *)
  let rec aux state_counts = function
    | Operational :: row ->
      let rest = List.drop_while row ~f:([%equal: status] Operational) in
      let next_state_counts =
        List.filter_map state_counts ~f:(function
          | { remaining_groups = 0 :: groups; in_group = true }, factor ->
            Some ({ remaining_groups = groups; in_group = false }, factor)
          | { in_group = true; _ }, _ -> None
          | state_count -> Some state_count)
      in
      (aux [@tailcall]) next_state_counts rest
    | Damaged :: row ->
      let damaged_count, rest = List.count_drop_while row ~f:([%equal: status] Damaged) in
      let damaged_count = damaged_count + 1 in
      let next_state_counts =
        List.filter_map state_counts ~f:(function
          | { remaining_groups = size :: _; _ }, _ when size < damaged_count -> None
          | { remaining_groups = size :: groups; _ }, factor ->
            Some
              ( { remaining_groups = (size - damaged_count) :: groups; in_group = true }
              , factor )
          | { remaining_groups = []; _ }, _ -> None)
      in
      (aux [@tailcall]) next_state_counts rest
    | Unknown :: rest ->
      let next_state_counts =
        List.concat_map state_counts ~f:(function
          | { remaining_groups = 0 :: groups; in_group = true }, factors ->
            [ { remaining_groups = groups; in_group = false }, factors ]
          | { remaining_groups = size :: groups; in_group = true }, factors ->
            [ { remaining_groups = (size - 1) :: groups; in_group = true }, factors ]
          | { remaining_groups = []; _ }, factors ->
            [ { remaining_groups = []; in_group = false }, factors ]
          | { remaining_groups = size :: groups; in_group = false }, factors ->
            [ { remaining_groups = size :: groups; in_group = false }, factors
            ; { remaining_groups = (size - 1) :: groups; in_group = true }, factors
            ])
        |> List.Assoc.sort_and_group ~compare:[%compare: traversal_state]
        |> List.Assoc.map ~f:(List.sum (module Int) ~f:Fn.id)
      in
      (aux [@tailcall]) next_state_counts rest
    | [] ->
      List.sum
        (module Int)
        state_counts
        ~f:(function
          | { remaining_groups = []; _ }, factors -> factors
          | { remaining_groups = [ 0 ]; in_group = true }, factors -> factors
          | _ -> 0)
  in
  let f (row, groups) = aux [ { remaining_groups = groups; in_group = false }, 1 ] row in
  List.sum (module Int) ~f
;;

let part2 =
  List.map ~f:(fun (springs, groups) ->
    ( List.concat
        [ springs
        ; [ Unknown ]
        ; springs
        ; [ Unknown ]
        ; springs
        ; [ Unknown ]
        ; springs
        ; [ Unknown ]
        ; springs
        ]
    , List.concat [ groups; groups; groups; groups; groups ] ))
  >> part1
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 21 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 525152 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 7718 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 128741994134728 |}]
;;
