open! Aoc_std

type gate =
  | And
  | Or
  | Xor
[@@deriving sexp_of]

let apply_gate = function
  | And -> ( && )
  | Or -> ( || )
  | Xor -> Bool.( <> )
;;

let value_of initial productions =
  Par_memo.fix
    (module String)
    (fun value_of wire ->
       match Map.find initial wire with
       | Some value -> value
       | None ->
         let gate, wire1, wire2 = Map.find_exn productions wire in
         let open Moonpool.Fut.Infix in
         Moonpool.await
         @@
         let+ value1 = Par_memo.moonpool_fut_of_kcas_promise (value_of wire1)
         and+ value2 = Par_memo.moonpool_fut_of_kcas_promise (value_of wire2) in
         (* In a third of the cases we don't need both values. Let's still wait for both of them *)
         apply_gate gate value1 value2)
;;

let parser =
  let open Angstrom in
  let%mapn initial_values =
    let entry =
      both (take 3 <* advance 2) (char '1' *> return true <|> char '0' *> return false)
      <* end_of_line
      <* commit
    in
    many_till entry end_of_line >>| String.Map.of_alist_exn
  and productions =
    let gate =
      let%mapn input1 = take 3 <* space
      and gate =
        choice
          [ string "AND " *> return And
          ; string "OR " *> return Or
          ; string "XOR " *> return Xor
          ]
      and input2 = take 3 in
      gate, input1, input2
    in
    let entry = both gate (advance 4 *> take 3) >>| Tuple2.swap in
    sep_by (end_of_line *> commit) entry <* end_of_input >>| String.Map.of_alist_exn
  in
  initial_values, productions
;;

let part1 (initial, productions) =
  let value_of = value_of initial productions in
  Map.fold_range_inclusive
    productions
    ~min:"z00"
    ~max:"z62"
    ~init:(0, 0)
    ~f:(fun ~key ~data:_ (acc, i) -> acc + (Bool.to_int (value_of key) lsl i), i + 1)
  |> fst
;;

let trace_sexp (initial, productions) =
  let open Par_memo in
  fix
    (module String)
    (fun trace_sexp output ->
       match Map.find initial output with
       | Some value -> [%message output (value : bool)]
       | None ->
         let gate, wire1, wire2 = Map.find_exn productions output in
         let open Moonpool.Fut.Infix in
         Moonpool.await
         @@
         let+ input1 = moonpool_fut_of_kcas_promise (trace_sexp wire1)
         and+ input2 = moonpool_fut_of_kcas_promise (trace_sexp wire2) in
         let input1, input2 =
           if fst (Sexp.size input1) < fst (Sexp.size input2)
           then input1, input2
           else input2, input1
         in
         [%message
           output
             ~_:
               ([%message "" ~_:(gate : gate) ~_:(input1 : Sexp.t) ~_:(input2 : Sexp.t)]
                : Sexp.t)])
;;

let part2 (initial, productions) ~expect swaps =
  let productions = Map.map_keys_exn (module String) productions ~f:swaps in
  let z = part1 (initial, productions) in
  let x =
    Map.fold_range_inclusive
      initial
      ~min:"x00"
      ~max:"x62"
      ~init:(0, 0)
      ~f:(fun ~key:_ ~data (acc, i) -> acc + (Bool.to_int data lsl i), i + 1)
    |> fst
  in
  let y =
    Map.fold_range_inclusive
      initial
      ~min:"y00"
      ~max:"y62"
      ~init:(0, 0)
      ~f:(fun ~key:_ ~data (acc, i) -> acc + (Bool.to_int data lsl i), i + 1)
    |> fst
  in
  let correct_z = x + y in
  let errors = z lxor correct_z in
  Mutex.protect xprint_mutex (fun () ->
    if correct_z <> z
    then (
      let trace_sexp = trace_sexp (initial, productions) in
      print_endline "--- Incorrect ---";
      for i = 0 to Int.floor_log2 errors do
        let masked = errors land (1 lsl i) in
        if masked <> 0 then print_s (trace_sexp (sprintf "z%02d" (Int.floor_log2 masked)))
      done)
    else print_endline "no incorrect digits";
    expect ())
;;
