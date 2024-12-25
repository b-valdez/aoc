open! Aoc_std

type input =
  | Lock of int * int * int * int * int
  | Key of int * int * int * int * int
[@@deriving variants]

let parser =
  let open Angstrom in
  let default = Option.value ~default:5 in
  let maybe_set char row = function
    | None -> Angstrom.char char *> return None <|> advance 1 *> return (Some row)
    | some -> advance 1 *> return some
  in
  let rec read char row variant acc1 acc2 acc3 acc4 acc5 =
    if row = 5
    then
      return
      @@ variant
           (default acc1)
           (default acc2)
           (default acc3)
           (default acc4)
           (default acc5)
    else
      end_of_line
      *>
      let* acc1 = maybe_set char row acc1
      and+ acc2 = maybe_set char row acc2
      and+ acc3 = maybe_set char row acc3
      and+ acc4 = maybe_set char row acc4
      and+ acc5 = maybe_set char row acc5 in
      read char (row + 1) variant acc1 acc2 acc3 acc4 acc5
  in
  let lock = read '#' 0 lock None None None None None in
  let key = read '.' 0 key None None None None None in
  choice
    [ string "#####" *> lock <* end_of_line <* string "....."
    ; string "....." *> key <* end_of_line <* string "#####"
    ]
  <* (end_of_line *> end_of_line <|> end_of_input)
;;

module T5 = struct
  type t = int * int * int * int * int [@@deriving compare, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let part1 =
  Iter.fold_map ~init:([], []) ~f:(fun (keys, locks) -> function
    | Lock (l1, l2, l3, l4, l5) ->
      ( (keys, (l1, l2, l3, l4, l5) :: locks)
      , List.count keys ~f:(fun (k1, k2, k3, k4, k5) ->
          k1 >= l1 && k2 >= l2 && k3 >= l3 && k4 >= l4 && k5 >= l5) )
    | Key (k1, k2, k3, k4, k5) ->
      ( ((k1, k2, k3, k4, k5) :: keys, locks)
      , List.count locks ~f:(fun (l1, l2, l3, l4, l5) ->
          k1 >= l1 && k2 >= l2 && k3 >= l3 && k4 >= l4 && k5 >= l5) ))
  >> Iter.sum
;;
