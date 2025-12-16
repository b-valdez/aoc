open! Aoc_std

let parser =
  let open Angstrom in
  sep_by1 (end_of_line <* commit) (pair ~sep:(char ',') nat)
;;

let part1 points =
  let open Iter in
  points
  |> diagonal_l
  |> map ~f:(fun ((a, b), (c, d)) -> (abs (a - c) + 1) * (abs (b - d) + 1))
  |> max_exn
;;

type axis =
  | X
  | Y

let get_axis = function
  | X -> fst
  | Y -> snd
;;

let other_axis = function
  | X -> Y
  | Y -> X
;;

(* This has become something of a mess *)
let part2 red_tiles =
  let borders red_tiles =
    let border_axis =
      match red_tiles with
      | (x, _) :: (x', _) :: _ when x = x' -> Y
      | _ -> X
    in
    let[@tail_mod_cons] rec aux = function
      | [] -> []
      | a :: b :: tl ->
        let constant = get_axis (other_axis border_axis) a in
        (constant, min_max ~compare (get_axis border_axis a) (get_axis border_axis b))
        :: (aux [@tailcall]) tl
      | [ _ ] -> assert false
    in
    border_axis, aux red_tiles
  in
  let borders_axis_1, borders_1 = borders red_tiles in
  let borders_axis_2, borders_2 =
    borders (List.tl_exn red_tiles @ [ List.hd_exn red_tiles ])
  in
  let border_intersects_rectangle axis a b (border_constant, border_bounds) =
    let aligned_off_axis =
      let low, high =
        min_max ~compare (get_axis (other_axis axis) a) (get_axis (other_axis axis) b)
      in
      Int.between ~low:(low + 1) ~high:(high - 1) border_constant
    in
    let intersects =
      let low, high = min_max ~compare (get_axis axis a) (get_axis axis b) in
      not @@ Interval.is_disjoint (low + 1, high - 1) border_bounds
    in
    aligned_off_axis && intersects
  in
  let rectangle_inside_borders axis a b =
    let border_axis_coord = (get_axis axis a + get_axis axis b) / 2 in
    let other_axis_coord =
      (get_axis (other_axis axis) a + get_axis (other_axis axis) b) / 2
    in
    let crossings =
      List.count borders_1 ~f:(fun (constant, (border_min, border_max)) ->
        constant <= other_axis_coord
        && Int.between ~low:border_min ~high:(border_max - 1) border_axis_coord)
    in
    crossings mod 2 = 1
  in
  let open Iter in
  red_tiles
  |> diagonal_l
  |> map ~f:(fun (((a, b) as corner_a), ((c, d) as corner_b)) ->
    -(abs (a - c) + 1) * (abs (b - d) + 1), corner_a, corner_b)
  |> sort ~cmp:[%compare: int * _ * _]
  |> filter ~f:(fun (_, corner_a, corner_b) ->
    (not
     @@ List.exists
          borders_1
          ~f:(border_intersects_rectangle borders_axis_1 corner_a corner_b))
    && (not
        @@ List.exists
             borders_2
             ~f:(border_intersects_rectangle borders_axis_2 corner_a corner_b))
    && rectangle_inside_borders borders_axis_1 corner_a corner_b)
  |> head_exn
  |> fst3
  |> ( ~- )
;;
