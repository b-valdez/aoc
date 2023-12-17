open! Aoc_std
open Grid

type cell =
  | Empty (** . *)
  | Mirror_a (** / *)
  | Mirror_b (** \ *)
  | Splitter_horizontal (** - *)
  | Splitter_vertical (** | *)

let parser =
  let open Angstrom in
  grid (function
    | '.' -> Empty
    | '/' -> Mirror_a
    | '\\' -> Mirror_b
    | '-' -> Splitter_horizontal
    | '|' -> Splitter_vertical
    | _ -> assert false)
;;

(*TODO: derive equal on Direction.t, lift beam_path and some derivations*)

let opposite = function
  | `N -> `S
  | `S -> `N
  | `E -> `W
  | `W -> `E
;;

let mirrored_a = function
  | `N -> `W
  | `W -> `N
  | `E -> `S
  | `S -> `E
;;

let mirrored_b = function
  | `N -> `E
  | `E -> `N
  | `W -> `S
  | `S -> `W
;;

module Partial_cell_state = struct
  type t =
    { entry : Direction.t
    ; cell : int * int
    }
  [@@deriving equal, compare, sexp_of, fields]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let energized_by grid start =
  let grid_height = Array.length grid in
  let grid_width = Array.length grid.(0) in
  let in_grid (x, y) =
    Int.between ~low:0 ~high:(grid_height - 1) y
    && Int.between ~low:0 ~high:(grid_width - 1) x
  in
  let open Direction in
  let filter_adjacent visited adjacent =
    let adjacent_filtered =
      List.filter adjacent ~f:(fun Partial_cell_state.({ cell; _ } as state) ->
        in_grid cell && not (Set.mem visited state))
    in
    adjacent_filtered
  in
  (* TODO lift breadth_first *)
  let rec breadth_first visited edge =
    if List.is_empty edge
    then visited
    else
      edge
      |> Iter.of_list
      |> Iter.fold
           ~init:(visited, [])
           ~f:(fun (visited, next) (cell_state : Partial_cell_state.t) ->
             let cell = grid.^(cell_state.cell) in
             let to_add_to_next =
               let adjacent =
                 match cell, cell_state.entry with
                 | Empty, direction
                 | Splitter_horizontal, (#horizontal as direction)
                 | Splitter_vertical, (#vertical as direction) ->
                   [ Partial_cell_state.
                       { cell = step cell_state.cell (opposite direction)
                       ; entry = direction
                       }
                   ]
                 | Mirror_a, direction ->
                   [ { cell = step cell_state.cell (mirrored_a direction)
                     ; entry = opposite (mirrored_a direction)
                     }
                   ]
                 | Mirror_b, direction ->
                   [ { cell = step cell_state.cell (mirrored_b direction)
                     ; entry = opposite (mirrored_b direction)
                     }
                   ]
                 | Splitter_horizontal, _ ->
                   List.map [%all: horizontal] ~f:(fun direction ->
                     Partial_cell_state.
                       { cell = step cell_state.cell (opposite direction)
                       ; entry = (direction :> [ `E | `N | `S | `W ])
                       })
                 | Splitter_vertical, _ ->
                   List.map [%all: vertical] ~f:(fun direction ->
                     Partial_cell_state.
                       { cell = step cell_state.cell (opposite direction)
                       ; entry = (direction :> [ `E | `N | `S | `W ])
                       })
               in
               filter_adjacent visited adjacent
             in
             Set.add visited cell_state, List.rev_append to_add_to_next next)
      |> fun (visited, next) -> (breadth_first [@tailcall]) visited next
  in
  breadth_first (Set.empty (module Partial_cell_state)) [ start ]
;;

(* TODO lift *)
let gen_of_iter (type a) (iter : a Iter.t) : _ -> a =
  let module M = struct
    type _ Effect.t += Yield : a -> unit Effect.t
  end
  in
  let yield v = Effect.perform (M.Yield v) in
  let rec next =
    ref
    @@ fun () ->
    Effect.Deep.match_with
      iter
      yield
      { retc = (fun () -> raise_notrace @@ Not_found_s [%message "No further values"])
      ; exnc = raise
      ; effc =
          (fun (type b) : (b Effect.t -> _) -> function
            | M.Yield (v : a) ->
              Some
                (fun (k : (b, _) Effect.Deep.continuation) ->
                  (next := fun () -> Effect.Deep.continue k ());
                  v)
            | _ -> None)
      }
  in
  fun _ -> !next ()
;;

let cells_in set =
  set
  |> Set.iter
  |> Iter.from_labelled_iter
  |> Iter.map ~f:Partial_cell_state.cell
  |> Iter.sort_uniq ~cmp:Position.compare
  |> Iter.length
;;

(* TODO lift in_grid*)
let part1 grid = cells_in @@ energized_by grid { cell = 0, 0; entry = `W }

let part2 grid =
  let grid_height = Array.length grid in
  let grid_width = Array.length grid.(0) in
  let module Comparator = struct
    type t = Partial_cell_state.t * (Partial_cell_state.t list[@opaque])
    [@@deriving sexp_of, compare]

    include (val Comparator.make ~sexp_of_t ~compare)
  end
  in
  let start_positions =
    let iterator =
      let open Iter in
      append_l
        [ map
            (0 -- (grid_width - 1))
            ~f:(fun x -> Partial_cell_state.{ cell = x, 0; entry = `N })
        ; map
            (0 -- (grid_height - 1))
            ~f:(fun y -> Partial_cell_state.{ cell = grid_width - 1, y; entry = `E })
        ; map
            (0 -- (grid_height - 1))
            ~f:(fun y -> Partial_cell_state.{ cell = 0, y; entry = `W })
        ; 0 -- (grid_width - 1)
          |> map ~f:(fun x ->
            Partial_cell_state.{ cell = x, grid_height - 1; entry = `S })
        ]
      |> map ~f:(fun entry ->
        let open Direction in
        let equivalent_exits =
          match grid.^(entry.Partial_cell_state.cell), entry.entry with
          | Empty, direction -> [ { entry with entry = opposite direction } ]
          | Mirror_a, direction -> [ { entry with entry = mirrored_a direction } ]
          | Mirror_b, direction -> [ { entry with entry = mirrored_b direction } ]
          | Splitter_horizontal, (#horizontal as direction)
          | Splitter_vertical, (#vertical as direction) ->
            List.filter_map [%all: Direction.t] ~f:(function
              | direction' when [%equal: [ `N | `E | `W | `S ]] direction' direction ->
                None
              | direction' -> Some { entry with entry = direction' })
          | Splitter_horizontal, #vertical ->
            List.map [%all: horizontal] ~f:(fun direction' ->
              { entry with entry = (direction' :> Direction.t) })
          | Splitter_vertical, #horizontal ->
            List.map [%all: vertical] ~f:(fun direction' ->
              { entry with entry = (direction' :> Direction.t) })
        in
        entry, equivalent_exits)
      |> gen_of_iter
    in
    Set.of_increasing_iterator_unchecked
      (module Comparator)
      ~len:(2 * (grid_height + grid_width))
      ~f:iterator
  in
  let rec aux max_found to_consider =
    match Set.choose to_consider with
    | None -> max_found
    | Some ((start_position, _) as chosen) ->
      let energized = energized_by grid start_position in
      let to_consider =
        Set.remove to_consider chosen
        |> Set.filter ~f:(fun (_, equivalent_exits) ->
          not @@ List.exists equivalent_exits ~f:(Set.mem energized))
      in
      let max_found = max max_found (cells_in energized) in
      (aux [@tailcall]) max_found to_consider
  in
  aux 0 start_positions
;;

let%expect_test "gen_of_iter" =
  let f = gen_of_iter (Iter.doubleton 1 2) in
  printf "%d" (f 0);
  [%expect {| 1 |}];
  printf "%d" (f 0);
  [%expect {| 2 |}]
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  [%expect {| 46 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 51 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  [%expect {| 6816 |}];
  printf "%d" @@ part2 parsed;
  [%expect {| 8163 |}]
;;
