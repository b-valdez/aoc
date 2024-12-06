open! Aoc_std
open Grid

let parser =
  let open Angstrom in
  let+ start, obstacles, width, height = sparse_tf_grid_start '^' in
  let stream
    : (( Position.t * Direction.t
         , ( Position.comparator_witness
             , Direction.comparator_witness )
             Tuple2.comparator_witness )
         Set_intf.Set.t
      * Position.t
      * Direction.t)
        Stream.t
    =
    Stream.create ()
  in
  start, obstacles, width, height, stream, tap stream
;;

type _ Effect.t +=
  | Visiting : (Position.t * Direction.t) -> unit Effect.t
  | Turning : (Position.t * Direction.t) -> unit Effect.t

let rec walk obstacles width height pos facing =
  match Direction.step pos facing with
  | outside_x, _ when not (Int.between ~low:0 ~high:(width - 1) outside_x) -> ()
  | _, outside_y when not (Int.between ~low:0 ~high:(height - 1) outside_y) -> ()
  | blocked when Set.mem obstacles blocked ->
    Effect.perform (Turning (pos, facing));
    (walk [@tailcall]) obstacles width height pos (Direction.turn facing Right)
  | next ->
    Effect.perform (Visiting (next, facing));
    (walk [@tailcall]) obstacles width height next facing
;;

let part1 (start, obstacles, width, height, to_part2, _) =
  let visited = Set.singleton (module Position) start in
  let turning_points =
    Set.Using_comparator.empty
      ~comparator:(Tuple2.comparator Position.comparator Direction.comparator)
  in
  let rec handle_effects visited turning_points fiber =
    Effect.Shallow.continue_with
      fiber
      ()
      { retc =
          (fun () ->
            Stream.poison to_part2 Parallel_iter.Stream_closed;
            Set.length visited)
      ; exnc =
          (fun exn ->
            Stream.poison to_part2 exn;
            raise exn)
      ; effc =
          (fun (type a) -> function
             | (Visiting (next, facing) : a Effect.t) ->
               Some
                 (fun (k : (a, _) Effect.Shallow.continuation) ->
                   if not (Set.mem visited next)
                   then Stream.push to_part2 (turning_points, next, facing);
                   (handle_effects [@tailcall]) (Set.add visited next) turning_points k)
             | Turning turning_point ->
               Some
                 (fun k ->
                   (handle_effects [@tailcall])
                     visited
                     (Set.add turning_points turning_point)
                     k)
             | _ -> None)
      }
  in
  handle_effects visited turning_points
  @@ Effect.Shallow.fiber (fun () -> walk obstacles width height start `N)
;;

exception Looped

let part2 (_, obstacles, width, height, _, from_part1) =
  Parallel_iter.of_cursor from_part1
  |> Parallel_iter.filter_count ~f:(fun (turning_points, candidate, facing) ->
    let start = Direction.step candidate (Direction.opposite facing) in
    let turning_points = Set.add turning_points (start, facing) in
    let obstacles = Set.add obstacles candidate in
    let rec handle_effects turning_points fiber =
      Effect.Shallow.continue_with
        fiber
        ()
        { retc = (fun () -> false)
        ; exnc =
            (function
              | Looped -> true
              | others -> raise others)
        ; effc =
            (fun (type a) -> function
               | (Visiting _ : a Effect.t) ->
                 Some
                   (fun (k : (a, _) Effect.Shallow.continuation) ->
                     (handle_effects [@tailcall]) turning_points k)
               | Turning turning_point ->
                 Some
                   (fun k ->
                     if Set.mem turning_points turning_point
                     then
                       Effect.Shallow.discontinue_with
                         k
                         Looped
                         { retc = (fun _ -> assert false)
                         ; exnc = Fun.const true
                         ; effc =
                             (function
                               | _ -> None)
                         }
                     else
                       (handle_effects [@tailcall])
                         (Set.add turning_points turning_point)
                         k)
               | _ -> None)
        }
    in
    handle_effects
      turning_points
      (Effect.Shallow.fiber (fun () ->
         walk obstacles width height start (Direction.turn facing Right))))
;;
