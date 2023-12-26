open! Aoc_std

let parser =
  let open Angstrom in
  let line =
    let%mapn pos =
      triple ~sep:(char ',' *> commit *> spaces) nat <* string " @" <* commit <* spaces
    and vel = triple ~sep:(char ',' *> commit *> spaces) int in
    pos, vel
  in
  lines line
;;

let part1 hailstones (area_min, area_max) =
  let hailstones = List.sort hailstones ~compare:[%compare: (int * int * int) * _] in
  let are_crossing (pos_a, vel_a) (pos_b, vel_b) =
    let open Owl_dense_matrix_d in
    let mat =
      of_array
        (Array.map ~f:float_of_int [| fst3 vel_a; -fst3 vel_b; snd3 vel_a; -snd3 vel_b |])
        2
        2
    in
    let vect =
      of_array
        (Array.map ~f:float_of_int [| fst3 pos_b - fst3 pos_a; snd3 pos_b - snd3 pos_a |])
        2
        1
    in
    match Owl_linalg_d.linsolve mat vect with
    | exception Failure _ -> false
    | times when not @@ is_nonnegative times -> false
    | times ->
      let time_a = get times 0 0 in
      Float.between
        ~low:(float_of_int area_min)
        ~high:(float_of_int area_max)
        ((float_of_int (fst3 vel_a) *. time_a) +. float_of_int (fst3 pos_a))
      && Float.between
           ~low:(float_of_int area_min)
           ~high:(float_of_int area_max)
           ((float_of_int (snd3 vel_a) *. time_a) +. float_of_int (snd3 pos_a))
  in
  let rec aux crossings = function
    | [] -> crossings
    | hailstone :: hailstones ->
      let crossings = crossings + List.count hailstones ~f:(are_crossing hailstone) in
      aux crossings hailstones
  in
  aux 0 hailstones
;;

let pos_at pos vel t = Tuple3.map2 pos vel ~f:(fun pos vel -> pos + (t * vel))

let part2_verify hailstones stone_pos stone_vel times =
  List.for_all2_exn hailstones times ~f:(fun (hail_pos, hail_vel) time ->
    [%equal: int * int * int]
      (pos_at hail_pos hail_vel time)
      (pos_at stone_pos stone_vel time))
;;

(* The idea how to obtain linear equations from the immediate quadratic equations taken from https://www.reddit.com/r/adventofcode/comments/18q40he/2023_day_24_part_2_a_straightforward_nonsolver/ *)

let part2 hailstones =
  let open Owl_dense_matrix_d in
  let hailstones = Array.of_list hailstones in
  let a_i i =
    let (px, py, _), (vx, vy, _) = hailstones.(i - 1) in
    of_array (Array.map ~f:float_of_int [| -vy; vx; py; -px |]) 1 4
  in
  let b_i i =
    let (px, py, _), (vx, vy, _) = hailstones.(i - 1) in
    float_of_int ((-vx * py) + (vy * px))
  in
  let a =
    of_rows
      [| sub (a_i 1) (a_i 2)
       ; sub (a_i 1) (a_i 3)
       ; sub (a_i 1) (a_i 4)
       ; sub (a_i 1) (a_i 5)
      |]
  in
  let b =
    of_array [| b_i 2 -. b_i 1; b_i 3 -. b_i 1; b_i 4 -. b_i 1; b_i 5 -. b_i 1 |] 4 1
  in
  let stone_x_y = Owl_linalg_d.linsolve a b in
  let[@warning "-8"] [| px_s; py_s; vx_s; vy_s |] = to_array stone_x_y in
  let (px_1, _, pz_1), (vx_1, _, vz_1) = hailstones.(0) in
  let t_1 = (float_of_int px_1 -. px_s) /. (vx_s -. float_of_int vx_1) in
  let (px_2, _, pz_2), (vx_2, _, vz_2) = hailstones.(1) in
  let t_2 = (float_of_int px_2 -. px_s) /. (vx_s -. float_of_int vx_2) in
  print_s [%message (t_1 : float) (t_2 : float)];
  let vz_s =
    (float_of_int pz_1
     +. (t_1 *. float_of_int vz_1)
     -. float_of_int pz_2
     -. (t_2 *. float_of_int vz_2))
    /. (t_1 -. t_2)
  in
  let pz_s = float_of_int pz_1 +. (t_1 *. float_of_int vz_1) -. (t_1 *. vz_s) in
  px_s, py_s, pz_s, vx_s, vy_s, vz_s
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed (7, 27);
  [%expect {| 2 |}];
  print_s @@ [%sexp_of: float * float * float * float * float * float] @@ part2 parsed;
  [%expect
    {|
    ((t_1 5.0000000000000036) (t_2 3.0000000000000004))
    (23.999999999999996 13.000000000000007 10.000000000000018 -2.9999999999999987
     0.99999999999999822 1.9999999999999938) |}];
  printf "%b" @@ part2_verify parsed (24, 13, 10) (-3, 1, 2) [ 5; 3; 4; 6; 1 ];
  [%expect {| true |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed (200000000000000, 400000000000000);
  [%expect {| 12783 |}];
  print_s @@ [%sexp_of: float * float * float * float * float * float] @@ part2 parsed;
  [%expect
    {|
    ((t_1 962702686957.99988) (t_2 65649375952.00074))
    (454587375941126.25 244764814652483.91 249133632375808.81 -330.00000000000028
     63.000000000000171 94.0000000000002) |}];
  print_s
  @@ [%sexp_of: int * int * int]
  @@ pos_at (fst (List.hd_exn parsed)) (snd (List.hd_exn parsed)) 962702686958;
  [%expect {| (136895489244986 305415083930838 339627684949861) |}];
  print_s
  @@ [%sexp_of: int * int * int]
  @@ pos_at
       (454587375941126, 244764814652484, 249133632375809)
       (-330, 63, 94)
       962702686958;
  [%expect {| (136895489244986 305415083930838 339627684949861) |}];
  printf "%d" @@ (454587375941126 + 244764814652484 + 249133632375809);
  [%expect {| 948485822969419 |}]
;;
