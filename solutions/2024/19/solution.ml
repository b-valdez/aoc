open! Aoc_std

let parse_available =
  let open Angstrom in
  sep_by1 (char ',' *> space) (take_while1 Char.is_lowercase)
  <* end_of_line
  <* end_of_line
;;

let parser =
  let open Angstrom in
  take_while1 Char.is_lowercase <* (end_of_line <|> end_of_input)
;;

let part1 available designs =
  (* re is not threadsafe because of internal lazy state https://github.com/ocaml/ocaml-re/issues/287 *)
  let re =
    let open Re in
    whole_string
      (rep
         (alt
            (available
             |> List.sort ~compare:Comparable.(reverse (lift ~f:String.length compare))
             |> List.map ~f:str)))
    |> compile
  in
  Iter.filter_count designs ~f:(Re.execp re)
;;

let rec num_arrangements available acc =
  match Map.max_elt_exn acc with
  | 0, [ ("", count) ] -> count
  | max, designs_with_count ->
    let acc = Map.remove acc max in
    let acc =
      List.fold designs_with_count ~init:acc ~f:(fun acc (design, count) ->
        let subdesigns =
          List.filter_map available ~f:(fun prefix -> String.chop_prefix design ~prefix)
        in
        List.fold ~init:acc subdesigns ~f:(fun acc subdesign ->
          Map.update
            acc
            (String.length subdesign)
            ~f:
              (Option.value_map
                 ~default:[ subdesign, count ]
                 ~f:
                   (Fun.flip
                      List.Assoc.update
                      subdesign
                      ~f:(function
                        | None -> Some count
                        | Some count' -> Some (count + count'))
                      ~equal:String.equal))))
    in
    (num_arrangements [@tailcall]) available acc
;;

let part2 available designs =
  let designs =
    Int.Map.of_list_with_key_multi
      (List.map (Iter.to_list designs) ~f:(fun design -> design, 1))
      ~get_key:(fun (design, _) -> String.length design)
  in
  num_arrangements available designs
;;
