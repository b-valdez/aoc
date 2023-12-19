open! Aoc_std

type target =
  | Accept
  | Reject
  | Workflow of string
[@@deriving variants]

type rating_field =
  | X
  | M
  | A
  | S

type rating =
  { x : int
  ; m : int
  ; a : int
  ; s : int
  }
[@@deriving fields]

type op =
  | Gt
  | Lt

type rule =
  { category : rating_field
  ; op : op
  ; threshhold : int
  ; target : target
  }

let parser =
  let open Angstrom in
  let target =
    choice [ char 'A' *> return accept; char 'R' *> return reject; word >>| workflow ]
  in
  let rule =
    let%mapn category =
      choice
        [ char 'x' *> return X
        ; char 'm' *> return M
        ; char 'a' *> return A
        ; char 's' *> return S
        ]
    and op = char '<' *> return Lt <|> char '>' *> return Gt
    and threshhold = nat
    and target = char ':' *> target in
    { category; op; threshhold; target }
  in
  let workflow =
    (let%mapn id = word <* char '{'
     and rules = sep_by1 (char ',') rule
     and default = char ',' *> target <* char '}' in
     id, (rules, default))
    <* commit
  in
  let workflows = lines_lazy workflow >>| String_dict.of_alist_exn in
  let part =
    let%mapn x = string "{x=" *> nat
    and m = string ",m=" *> nat
    and a = string ",a=" *> nat
    and s = string ",s=" *> nat <* char '}' in
    { x; m; a; s }
  in
  let%mapn workflows = workflows <* end_of_line <* end_of_line <* commit
  and parts = lines part in
  workflows, parts
;;

let get = function
  | X -> x
  | M -> m
  | A -> a
  | S -> s
;;

let find_target ratings rule =
  let op =
    match rule.op with
    | Gt -> ( > )
    | Lt -> ( < )
  in
  Option.some_if (op (get rule.category ratings) rule.threshhold) rule.target
;;

let rating_sum { x; m; a; s } = x + m + a + s

let part1 (workflows, parts) =
  let rec aux workflow_id part =
    let rules, default = String_dict.find_exn workflows workflow_id in
    match List.find_map rules ~f:(find_target part) |> Option.value ~default with
    | Accept -> true
    | Reject -> false
    | Workflow workflow_id -> (aux [@tailcall]) workflow_id part
  in
  List.filter parts ~f:(aux "in") |> List.sum (module Int) ~f:rating_sum
;;

type rating_range =
  { x : int * int
  ; m : int * int
  ; a : int * int
  ; s : int * int
  }
[@@deriving fields]

let get = function
  | X -> x
  | M -> m
  | A -> a
  | S -> s
;;

let set field =
  let open Fields_of_rating_range in
  (match field with
   | X -> x
   | M -> m
   | A -> a
   | S -> s)
  |> Field.fset
;;

(** return is handled, unhandled*)
let split_by_op_threshhold op threshhold (low, high) =
  match op with
  | Gt ->
    if threshhold < low
    then Some (low, high), None
    else if threshhold >= high
    then None, Some (low, high)
    else Some (threshhold + 1, high), Some (low, threshhold)
  | Lt ->
    if threshhold > high
    then Some (low, high), None
    else if threshhold <= low
    then None, Some (low, high)
    else Some (low, threshhold - 1), Some (threshhold, high)
;;

let split_by_rule rule rating_range =
  let handled, unhandled =
    split_by_op_threshhold rule.op rule.threshhold (get rule.category rating_range)
  in
  ( Option.map handled ~f:(set rule.category rating_range)
  , Option.map unhandled ~f:(set rule.category rating_range) )
;;

let size_of_int_range (a, b) = b - a + 1

let size_of_range { x; m; a; s } =
  size_of_int_range x * size_of_int_range m * size_of_int_range a * size_of_int_range s
;;

let part2 (workflows, _) =
  let rec split accepted = function
    | [] -> List.sum (module Int) accepted ~f:size_of_range
    | (_, []) :: _ -> assert false
    | (workflow_id, ranges) :: pending ->
      let rules, default = String_dict.find_exn workflows workflow_id in
      let unhandled, handled, accepted_in_step =
        List.fold
          rules
          ~init:(ranges, [], [])
          ~f:(fun (unhandled, handled, accepted) rule ->
            let affected, unaffected =
              List.rev_map unhandled ~f:(split_by_rule rule)
              |> List.unzip
              |> Tuple2.map ~f:List.filter_opt
            in
            let handled, accepted =
              match rule.target with
              | Accept -> handled, List.rev_append affected accepted
              | Reject -> handled, accepted
              | Workflow workflow_id -> (workflow_id, affected) :: handled, accepted
            in
            unaffected, handled, accepted)
      in
      let handled, accepted =
        match default with
        | Accept ->
          handled, List.concat_no_order [ unhandled; accepted; accepted_in_step ]
        | Reject -> handled, List.rev_append accepted_in_step accepted
        | Workflow workflow_id ->
          (workflow_id, unhandled) :: handled, List.rev_append accepted_in_step accepted
      in
      let pending =
        Iter.of_list handled
        |> Iter.fold ~init:pending ~f:(fun pending (workflow_id, ranges) ->
          List.Assoc.update ~equal:equal_string pending workflow_id ~f:(function
            | None -> Some ranges
            | Some ranges' -> Some (List.rev_append ranges ranges')))
      in
      (split [@tailcall]) accepted pending
  in
  split [] [ "in", [ { x = 1, 4000; m = 1, 4000; a = 1, 4000; s = 1, 4000 } ] ]
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 19114 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 167409079868000 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 342650 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 130303473508222 |}]
;;
