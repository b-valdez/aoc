open! Aoc_std

type node =
  | Broadcast of string list
  | Flip_flop of bool * string list
  | Conjunction of (string * bool) list * string list
  | Rx of bool (** for part 2 *)
[@@deriving hash, compare, sexp]

let dests_of = function
  | Broadcast dests | Flip_flop (_, dests) | Conjunction (_, dests) -> dests
  | Rx _ -> []
;;

let parsed_to_map parsed =
  let without_conjunction_states = String.Map.of_alist_exn parsed in
  Map.fold
    without_conjunction_states
    ~init:without_conjunction_states
    ~f:(fun ~key ~data map ->
      List.fold
        (dests_of data)
        ~init:map
        ~f:
          (Map.change ~f:(function
            | Some (Conjunction (states, dests)) ->
              Some (Conjunction ((key, false) :: states, dests))
            | other -> other)))
;;

let parser =
  let open Angstrom in
  let line =
    let%mapn f =
      choice
        [ char '%' *> return (fun name dests -> name, Flip_flop (false, dests))
        ; char '&' *> return (fun name dests -> name, Conjunction ([], dests))
        ; return (fun name dests -> name, Broadcast dests)
        ]
    and name = word
    and dests = string " -> " *> sep_by1 (string ", ") word in
    f name dests
  in
  lines line >>| parsed_to_map
;;

let rec work_off_queue high_count low_count map queue =
  match Fqueue.dequeue queue with
  | None -> map, (high_count, low_count)
  | Some ((from, is_high, name), queue) ->
    let map, dests, is_high =
      match Map.find map name with
      | None -> map, [], false
      | Some (Broadcast dests) -> map, dests, is_high
      | Some (Conjunction (state, dests)) ->
        let new_state = List.Assoc.add ~equal:equal_string state from is_high in
        ( Map.update map name ~f:(Fn.const (Conjunction (new_state, dests)))
        , dests
        , not (List.for_all new_state ~f:snd) )
      | Some (Flip_flop (state, dests)) ->
        if is_high
        then map, [], false
        else
          ( Map.update map name ~f:(Fn.const (Flip_flop (not state, dests)))
          , dests
          , not state )
      | Some (Rx _) ->
        print_s [%message "Signal to Rx" (is_high : bool)];
        Map.update map name ~f:(Fn.const (Rx (not is_high))), [], false
    in
    let queue =
      List.fold dests ~init:queue ~f:(fun queue dest ->
        Fqueue.enqueue queue (name, is_high, dest))
    in
    let high_count, low_count =
      if is_high
      then high_count + List.length dests, low_count
      else high_count, low_count + List.length dests
    in
    (work_off_queue [@tailcall]) high_count low_count map queue
;;

let part1 map =
  let f map (high_count, low_count) =
    work_off_queue
      high_count
      (low_count + 1)
      map
      (Fqueue.return ("button", false, "broadcaster"))
  in
  let _, (high, low) =
    detect_loop_with_affine_accumulator
      (module struct
        type t = node Map.M(String).t [@@deriving hash, compare, sexp]
      end)
      ~f
      ~init_state:map
      ~init_acc:(0, 0)
      ~skip_to:1000
      ~compute_final_accumulator:
        (fun
          ~found_in_loop:(base_high, base_low)
          ~loops
          ~before_loop:(subtrahent_high, subtrahent_low)
          ~after_loop:(minuent_high, minuent_low)
        ->
        ( base_high + (loops * (minuent_high - subtrahent_high))
        , base_low + (loops * (minuent_low - subtrahent_low)) ))
  in
  high * low
;;

let find_seperations map =
  let broadcast_dests = Map.find_exn map "broadcaster" |> dests_of in
  let rec aux reachable = function
    | [] -> reachable
    | hd :: tl when Set.mem reachable hd -> aux reachable tl
    | hd :: tl ->
      let reachable = Set.add reachable hd in
      let to_consider =
        Map.find map hd |> Option.map ~f:dests_of |> Option.value ~default:[]
      in
      aux reachable (List.rev_append to_consider tl)
  in
  List.map broadcast_dests ~f:(fun start -> start, aux String.Set.empty [ start ])
;;

let[@warning "-32"] print_state iteration map =
  let states =
    Map.filter_map map ~f:(function
      | Conjunction (state, _) ->
        Option.some
        @@ [%sexp_of: (string * bool) list]
             (List.sort ~compare:[%compare: string * _] state)
      | Flip_flop (state, _) | Rx state -> Option.some @@ [%sexp_of: bool] state
      | _ -> None)
  in
  print_s [%message (iteration : int) (states : Sexp.t String.Map.t)]
;;

let part2 map target =
  let exception Found of (int * node String.Map.t) * (int * node String.Map.t) in
  let map = Map.update map target ~f:(Fn.const (Rx false)) in
  match
    Iter.(1 -- 12000)
    |> Iter.fold ~init:(map, None) ~f:(fun (state, end_states) iteration ->
      let state, _ =
        work_off_queue 0 0 state (Fqueue.return ("button", false, "broadcaster"))
      in
      print_state iteration state;
      let[@warning "-8"] (Rx success) = Map.find_exn state target in
      if not success
      then (
        match end_states with
        | None -> Map.update state target ~f:(Fn.const (Rx false)), Some (iteration, state)
        | Some end_state -> raise_notrace (Found (end_state, (iteration, state))))
      else state, end_states)
  with
  | exception Found (i, j) -> i, j
  | _ -> assert false
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  printf "%d" @@ part1 parsed;
  {%expect| 32000000 |};
  let parsed2 = parse_string parser Sample2.sample2 in
  printf "%d" @@ part1 parsed2;
  {%expect| 11687500 |}
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ part1 parsed;
  {%expect| 818649769 |};
  printf "%d" @@ List.fold ~init:1 ~f:lcm [ 4057; 3943; 3917; 3931 ];
  (* 246313604784976 too low *)
  {%expect| 246313604784977 |}
;;

let%expect_test ("input_exploration" [@tags "disabled"]) =
  let parsed = parse_string parser Input.input in
  let parsed = Map.add_exn parsed ~key:"rx" ~data:(Rx false) in
  let seperations = find_seperations parsed in
  print_s @@ [%sexp_of: (string * String.Set.t) list] @@ seperations;
  [%expect
    {|
    ((gh (dc gb gh jd mh nh pb qq rs rv rx sg tj ts vd xc))
     (dl (ch cz dl hl hm ht kx nf pn rx sv tj vt xf zf zm))
     (xg (gd gv lg lq mb mt pm rx sk sr st tb tj xg zc zv))
     (fb (bf bh dr fb fc hh kk lc nx pv qm rx sm tj vv xb))) |}]
  (* There are 4 distinct components, only sharing tj and rx.
     tj is the only input to rx.
     tj itself has 4 inputs: xc (from gh), vt (from dl), sk (from xg) and kk (from fb) *);
  let adjusted_maps =
    List.map seperations ~f:(fun (from, reachable) ->
      let map =
        Map.filter_keys parsed ~f:(Set.mem reachable)
        |> Fn.flip Map.remove "rx"
        |> Fn.flip Map.update "tj" ~f:(Fn.const (Rx false))
        |> Map.add_exn ~key:"broadcaster" ~data:(Broadcast [ from ])
      in
      from, map)
  in
  let partial_solutions =
    List.map (List.drop adjusted_maps 3) ~f:(fun (_from, map) -> part2 map "tj")
  in
  (* gh: 4056, 8113, 12170, 16227, 20284, ...*)
  (* dl: 3942, 7885, 11828, ... *)
  (* xg: 3916, 7833, 11750, ...*)
  (* fb: 3930, 7861, 11792, ... *)
  (*... modulo an off-by-1-error *)
  print_s
  @@ [%sexp_of:
       ((int * node Aoc_std.String.Map.t) * (int * node Aoc_std.String.Map.t)) list]
  @@ partial_solutions;
  [%expect.unreachable]
;;
