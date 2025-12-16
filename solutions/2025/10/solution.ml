open! Aoc_std

let parser =
  let open Angstrom in
  let+ _, indicator_lights =
    char '['
    *> scan_state (0, 0) (fun (pos, state) -> function
      | '.' -> Some (pos + 1, state)
      | '#' -> Some (pos + 1, state + (1 lsl pos))
      | _ -> None)
    <* char ']'
    <* char ' '
  and+ button_wiring =
    char '(' *> sep_by1 (string ") (") (sep_by1 (char ',') nat) <* char ')' <* char ' '
  and+ joltage_requirements = char '{' *> sep_by1 (char ',') nat <* char '}'
  and+ _ = end_of_line <|> end_of_input in
  indicator_lights, button_wiring, joltage_requirements
;;

let min_presses target buttons =
  let _, result =
    dijkstra
      (module Int)
      (module Int)
      ~step:(fun state presses ->
        Iter.(map (of_list buttons)) ~f:(fun button ->
          ( List.fold button ~init:state ~f:(fun state wire -> state lxor (1 lsl wire))
          , presses + 1 )))
      ~sorted_start_positions:[ 0, 0 ]
      ~is_goal:(equal target)
  in
  result
;;

let part1 machines =
  let open Iter in
  machines
  |> map ~f:(fun (indicator_lights, button_wiring, _) ->
    min_presses indicator_lights button_wiring)
  |> sum
;;

let min_presses buttons joltage =
  let open Lp in
  let vars =
    List.mapi buttons ~f:(fun i button ->
      button, var ~integer:true ~lb:0. ("button" ^ Int.to_string i))
  in
  let constraints =
    List.mapi joltage ~f:(fun i target ->
      List.fold vars ~init:zero ~f:(fun acc (button, var) ->
        if List.mem button i ~equal then acc ++ var else acc)
      =~ c (Float.of_int target))
  in
  let presses = List.fold vars ~init:zero ~f:(fun acc (_, var) -> acc ++ var) in
  let problem = make (minimize presses) constraints in
  Lp_glpk.Milp.solve ~term_output:false problem
  |> Result.ok_or_failwith
  |> fst
  |> Float.iround_nearest_exn
;;

let part2 machines =
  let open Iter in
  machines
  |> map ~f:(fun (_, button_wiring, joltage_requirements) ->
    min_presses button_wiring joltage_requirements)
  |> sum
;;
