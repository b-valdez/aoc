open! Aoc_std

type pos3d = int * int * int [@@deriving sexp, equal]

let compare_pos3d (x, y, z) (x', y', z') =
  [%compare: int * int * int] (z, y, x) (z', y', x')
;;

type brick =
  | Cube of pos3d
  | X of pos3d * int
  | Y of pos3d * int
  | Z of pos3d * int
[@@deriving sexp, equal]

let pos3d_of_brick = function
  | Cube pos3d | X (pos3d, _) | Y (pos3d, _) | Z (pos3d, _) -> pos3d
;;

let compare_brick = Comparable.lift [%compare: pos3d] ~f:pos3d_of_brick

let parser =
  let open Angstrom in
  let string_of_int i = string (string_of_int i) in
  let brick_start = triple ~sep:(char ',' *> commit) nat in
  let line =
    let%bind ((x, y, z) as pos) = brick_start <* char '~' in
    choice
      [ string_of_int x
        *> char ','
        *> string_of_int y
        *> char ','
        *> string_of_int z
        *> return (Cube pos)
      ; (nat
         <* char ','
         <* string_of_int y
         <* char ','
         <* string_of_int z
         >>| fun x' -> X (pos, x' - x))
      ; (string_of_int x *> char ',' *> nat
         <* char ','
         <* string_of_int z
         >>| fun y' -> Y (pos, y' - y))
      ; (string_of_int x *> char ',' *> string_of_int y *> char ',' *> nat
         >>| fun z' -> Z (pos, z' - z))
      ]
  in
  lines line >>| List.sort ~compare:[%compare: brick]
;;

let max_x_of_brick = function
  | Cube (x, _, _) | Y ((x, _, _), _) | Z ((x, _, _), _) -> x
  | X ((x, _, _), dx) when dx < 0 -> x
  | X ((x, _, _), dx) -> x + dx
;;

let max_y_of_brick = function
  | Cube (_, y, _) | X ((_, y, _), _) | Z ((_, y, _), _) -> y
  | Y ((_, y, _), dy) when dy < 0 -> y
  | Y ((_, y, _), dy) -> y + dy
;;

let min_z_pos3d_of_brick = function
  | Cube pos3d -> [ pos3d ]
  | X (pos3d, dx) -> List.init (dx + 1) ~f:(fun dx -> Tuple3.map_fst pos3d ~f:(( + ) dx))
  | Y (pos3d, dy) -> List.init (dy + 1) ~f:(fun dy -> Tuple3.map_snd pos3d ~f:(( + ) dy))
  | Z (pos3d, dz) when dz > 0 -> [ pos3d ]
  | Z (pos3d, dz) -> [ Tuple3.map_trd pos3d ~f:(( + ) dz) ]
;;

let max_z_pos3d_of_brick = function
  | Cube pos3d -> [ pos3d ]
  | X (pos3d, dx) -> List.init (dx + 1) ~f:(fun dx -> Tuple3.map_fst pos3d ~f:(( + ) dx))
  | Y (pos3d, dy) -> List.init (dy + 1) ~f:(fun dy -> Tuple3.map_snd pos3d ~f:(( + ) dy))
  | Z (pos3d, dz) when dz < 0 -> [ pos3d ]
  | Z (pos3d, dz) -> [ Tuple3.map_trd pos3d ~f:(( + ) dz) ]
;;

let fall brick dz =
  let adjust_pos (x, y, z) = x, y, z - dz in
  match brick with
  | Cube pos -> Cube (adjust_pos pos)
  | X (pos, dx) -> X (adjust_pos pos, dx)
  | Y (pos, dy) -> Y (adjust_pos pos, dy)
  | Z (pos, dz) -> Z (adjust_pos pos, dz)
;;

(** precondition: the list of blocks is sorted by z such that they can just fall in order *)
let settle dimx dimy ?on_fall =
  (* aux consumes height_map *)
  let[@tail_mod_cons] rec aux height_map = function
    | [] -> []
    | hd :: tl ->
      let dz =
        Iter.of_list (min_z_pos3d_of_brick hd)
        |> Iter.map ~f:(fun (x, y, z) -> z - height_map.(x).(y))
        |> Iter.min_exn ~lt:( < )
      in
      Option.iter on_fall ~f:(fun eff -> if dz > 1 then Effect.perform eff);
      let fallen = fall hd (dz - 1) in
      (* this didn't need to be imperative, but an array is a good datastructure for
         the heightmap and has efficient imperative features and the imparativity is local *)
      List.iter (max_z_pos3d_of_brick fallen) ~f:(fun (x, y, z) ->
        height_map.(x).(y) <- z);
      fallen :: aux height_map tl
  in
  aux (Array.make_matrix ~dimx ~dimy 0)
;;

let part1 bricks =
  let module Fallen = struct
    type _ Effect.t += Eff : unit -> unit Effect.t
  end
  in
  let max_x, max_y =
    List.fold bricks ~init:(0, 0) ~f:(fun (max_x, max_y) brick ->
      max max_x (max_x_of_brick brick), max max_y (max_y_of_brick brick))
  in
  let settle = settle (max_x + 1) (max_y + 1) in
  let settled = settle bricks |> List.sort ~compare:compare_brick in
  List.count settled ~f:(fun brick ->
    let without_brick = List.update_concat settled brick [] ~equal:equal_brick in
    Effect.Deep.match_with
      (settle ~on_fall:(Fallen.Eff ()))
      without_brick
      { retc = Fn.const true
      ; exnc = Fn.const false
      ; effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Fallen.Eff () ->
              Some
                (fun (k : (b, _) Effect.Deep.continuation) ->
                  Effect.Deep.discontinue k Exit)
            | _ -> None)
      })
;;

let part2 bricks =
  let module Fallen = struct
    type _ Effect.t += Eff : unit -> unit Effect.t
  end
  in
  let max_x, max_y =
    List.fold bricks ~init:(0, 0) ~f:(fun (max_x, max_y) brick ->
      max max_x (max_x_of_brick brick), max max_y (max_y_of_brick brick))
  in
  let settle = settle (max_x + 1) (max_y + 1) in
  let settled = settle bricks |> List.sort ~compare:compare_brick in
  let rec count_fallen : type a. int -> (a, _) Effect.Shallow.continuation -> a -> int =
    fun count k arg ->
    Effect.Shallow.continue_with
      k
      arg
      { retc = Fn.const count
      ; exnc = raise_notrace
      ; effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Fallen.Eff () ->
              Some
                (fun (k : (b, _) Effect.Shallow.continuation) ->
                  count_fallen (count + 1) k ())
            | _ -> None)
      }
  in
  Iter.of_list settled
  |> Iter.map ~f:(fun brick ->
    let without_brick = List.update_concat settled brick [] ~equal:equal_brick in
    count_fallen 0 (Effect.Shallow.fiber (settle ~on_fall:(Fallen.Eff ()))) without_brick)
  |> Iter.sum
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 5 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 7 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 495 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 76158 |}]
;;
