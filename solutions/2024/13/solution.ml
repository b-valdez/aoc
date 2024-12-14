open! Aoc_std

let parser =
  let open Angstrom in
  let button =
    advance (String.length "Button _: X+")
    *> pair nat ~sep:(advance (String.length ", Y+"))
    <* end_of_line
    <* commit
  in
  let%mapn a = button
  and b = button
  and prize =
    advance (String.length "Prize: X=") *> pair nat ~sep:(advance (String.length ", Y="))
    <* (end_of_line *> end_of_line <|> end_of_input)
  in
  a, b, prize
;;

let part1 cursor =
  Parallel_iter.of_cursor cursor
  |> Parallel_iter.filter_map ~f:(fun ((a_x, a_y), (b_x, b_y), (prize_x, prize_y)) ->
    let open Lp in
    let a = var ~integer:true ~ub:100. "A" in
    let b = var ~integer:true ~ub:100. "B" in
    let int i = c (Float.of_int i) in
    let x_constraint = (int a_x *~ a) ++ (int b_x *~ b) =~ int prize_x in
    let y_constraint = (int a_y *~ a) ++ (int b_y *~ b) =~ int prize_y in
    let cost = (int 3 *~ a) ++ b in
    let problem = make (minimize cost) [ x_constraint; y_constraint ] in
    let%map.Option cost, _vars =
      Lp_glpk.Milp.solve ~term_output:false problem |> Result.ok
    in
    cost |> Float.iround_nearest_exn)
  |> Parallel_iter.sum
;;

let part2 cursor =
  Parallel_iter.of_cursor cursor
  |> Parallel_iter.filter_map ~f:(fun ((a_x, a_y), (b_x, b_y), (prize_x, prize_y)) ->
    let open Lp in
    let a = var ~integer:true "A" in
    let b = var ~integer:true "B" in
    let int i = c (Float.of_int i) in
    let x_constraint =
      (int a_x *~ a) ++ (int b_x *~ b) =~ int (prize_x + 10000000000000)
    in
    let y_constraint =
      (int a_y *~ a) ++ (int b_y *~ b) =~ int (prize_y + 10000000000000)
    in
    let cost = (int 3 *~ a) ++ b in
    let problem = make (minimize cost) [ x_constraint; y_constraint ] in
    let%map.Option cost, _vars =
      Lp_glpk.Milp.solve ~term_output:false problem |> Result.ok
    in
    cost |> Float.iround_nearest_exn)
  |> Parallel_iter.sum
;;
