open! Aoc_std

type state =
  { a : int
  ; b : int
  ; c : int
  }
[@@deriving sexp]

let[@inline always] combo_operand { a; b; c } = function
  | (0 | 1 | 2 | 3) as literal -> literal
  | 4 -> a
  | 5 -> b
  | 6 -> c
  | (7 | _) as invalid -> invalid_argf "combo_operand %d" invalid ()
;;

let perform opcode operand state rev_out =
  match opcode with
  | 0 (* adv *) -> { state with a = state.a lsr combo_operand state operand }, rev_out
  | 1 (* bxl *) -> { state with b = state.b lxor operand }, rev_out
  | 2 (* bst *) -> { state with b = combo_operand state operand mod 8 }, rev_out
  | 3 (* jnz *) -> state, rev_out
  | 4 (* bxc *) -> { state with b = state.b lxor state.c }, rev_out
  | 5 (* out *) -> state, (combo_operand state operand mod 8) :: rev_out
  | 6 (* bdv *) -> { state with b = state.a lsr combo_operand state operand }, rev_out
  | 7 (* cdv *) -> { state with c = state.a lsr combo_operand state operand }, rev_out
  | invalid -> invalid_argf "perform %d" invalid ()
;;

let parser =
  let open Angstrom in
  let%mapn a = advance (String.length "Register X: ") *> nat <* end_of_line
  and b = advance (String.length "Register X: ") *> nat <* end_of_line
  and c = advance (String.length "Register X: ") *> nat <* end_of_line
  and program =
    end_of_line *> advance (String.length "Program: ") *> sep_by (char ',') digit
    >>| Array.of_list
  in
  { a; b; c }, program
;;

let part1 ?(verbose : unit option) (state, program) =
  let rec aux state instruction_pointer rev_out =
    if instruction_pointer = Array.length program
    then if state.a = 0 then List.rev rev_out else (aux [@tailcall]) state 0 rev_out
    else (
      let state, rev_out =
        perform
          program.(instruction_pointer)
          program.(instruction_pointer + 1)
          state
          rev_out
      in
      if Option.is_some verbose then print_s [%message "part1" (state : state)];
      (aux [@tailcall]) state (instruction_pointer + 2) rev_out)
  in
  aux state 0 []
;;

let part2 (_, program) =
  let module T = struct
    type register =
      | A
      | B
      | C
    [@@deriving equal, hash, sexp]

    type t =
      { step : int
      ; loop : int
      ; register : register
      ; bit : int
      }
    [@@deriving equal, hash, sexp]
  end
  in
  let module Memo = Fix.Memoize.ForHashedType (T) in
  let combo_operand memoized step loop operand bit =
    match operand with
    | 4 -> memoized T.{ step = step - 2; loop; register = A; bit }
    | 5 -> memoized T.{ step = step - 2; loop; register = B; bit }
    | 6 -> memoized T.{ step = step - 2; loop; register = C; bit }
    | literal -> Blang.constant (literal land (1 lsl bit) <> 0)
  in
  let blang_xor a b =
    Blang.and_ [ Blang.or_ [ a; b ]; Blang.not_ @@ Blang.and_ [ a; b ] ]
  in
  let memoized =
    Memo.fix
    @@ fun memoized -> function
    | { loop; step; bit; _ } as v
      when loop < 0
           || loop >= Array.length program
           || step < 0
           || step > Array.length program
           || bit < 0 ->
      invalid_argf
        "%s"
        (Sexp.to_string_hum @@ [%sexp_of: T.t * int array] (v, program))
        ()
    | { loop = 0; step = 0; register = B | C; bit = _ } -> Blang.false_
    | { loop = 0; step = 0; register = A; bit } -> Blang.base bit
    | { loop; step = 0; register; bit } ->
      memoized { loop = loop - 1; step = Array.length program; register; bit }
    | { loop; step; register; bit } ->
      let operand = program.(step - 1) in
      (match program.(step - 2), register with
       | 0, A ->
         if Int.between ~low:4 ~high:6 operand then invalid_arg "This is not modeled";
         memoized { loop; step = step - 2; register = A; bit = bit + operand }
       | 1, B ->
         blang_xor
           (memoized { loop; step = step - 2; register = B; bit })
           (Blang.constant (operand land (1 lsl bit) <> 0))
       | 2, B ->
         if bit > 2 then Blang.false_ else combo_operand memoized step loop operand bit
       | 4, B ->
         blang_xor
           (memoized { loop; step = step - 2; register = B; bit })
           (memoized { loop; step = step - 2; register = C; bit })
       | 6, B ->
         if Int.between ~low:4 ~high:6 operand then invalid_arg "This is not modeled";
         memoized { loop; step = step - 2; register = A; bit = bit + operand }
       | 7, C ->
         let s1 = combo_operand memoized step loop operand 0 in
         let s2 = combo_operand memoized step loop operand 1 in
         let s4 = combo_operand memoized step loop operand 2 in
         (* in my input these are enough *)
         Blang.if_
           (Blang.and_ [ s1; s2; s4 ])
           (memoized { loop; step = step - 2; register = A; bit = bit + 7 })
           (Blang.if_
              (Blang.and_ [ Blang.not_ s1; s2; s4 ])
              (memoized { loop; step = step - 2; register = A; bit = bit + 6 })
              (Blang.if_
                 (Blang.and_ [ s1; Blang.not_ s2; s4 ])
                 (memoized { loop; step = step - 2; register = A; bit = bit + 5 })
                 (Blang.if_
                    (Blang.and_ [ Blang.not_ s1; Blang.not_ s2; s4 ])
                    (memoized { loop; step = step - 2; register = A; bit = bit + 4 })
                    (Blang.if_
                       (Blang.and_ [ s1; s2; Blang.not_ s4 ])
                       (memoized { loop; step = step - 2; register = A; bit = bit + 3 })
                       (Blang.if_
                          (Blang.and_ [ Blang.not_ s1; s2; Blang.not_ s4 ])
                          (memoized
                             { loop; step = step - 2; register = A; bit = bit + 2 })
                          (Blang.if_
                             (Blang.and_ [ s1; Blang.not_ s2; Blang.not_ s4 ])
                             (memoized
                                { loop; step = step - 2; register = A; bit = bit + 1 })
                             (memoized { loop; step = step - 2; register = A; bit })))))))
       | _ -> memoized { loop; step = step - 2; register; bit })
  in
  let out_index, _ = Array.findi_exn program ~f:(fun i op -> i mod 2 = 0 && op = 5) in
  let register =
    match program.(out_index + 1) with
    | 4 -> T.A
    | 5 -> B
    | 6 -> C
    | _ -> assert false
  in
  let outputs_satisfied =
    List.range 0 (Array.length program)
    |> List.map ~f:(fun depth ->
      Blang.and_
      @@ List.map [ 0; 1; 2 ] ~f:(fun i ->
        if program.(depth) land (1 lsl i) <> 0
        then memoized { loop = depth; step = out_index + 2; register; bit = i }
        else
          Blang.not_ @@ memoized { loop = depth; step = out_index + 2; register; bit = i }))
    |> Blang.and_
  in
  let biggest_index = Blang.max_elt outputs_satisfied ~compare |> Option.value_exn in
  let stop_satisfied =
    List.range ~stop:`inclusive 0 biggest_index
    |> List.map ~f:(fun bit ->
      Blang.not_
      @@ memoized
           { loop = Array.length program - 1
           ; step = Array.length program
           ; register = A
           ; bit
           })
  in
  let is_satisfied = Blang.and_ (outputs_satisfied :: stop_satisfied) in
  (* possibly promote to Aoc_std, because core only has Blang.eval_set which has prohibitive space requirements for this kind of problem *)
  let rec find_smallest blang backtrack current_known =
    match Blang.max_elt blang ~compare with
    | None -> current_known
    | Some max ->
      let state =
        List.range ~stride:(-1) ~stop:`inclusive max 0
        |> List.fold_until
             ~init:(blang, current_known)
             ~f:(fun (blang, current_known) bit ->
               if Map.mem current_known bit
               then Continue (blang, current_known)
               else (
                 let specialized_true =
                   Blang.specialize blang (fun i ->
                     if i = bit then `Known true else `Unknown)
                 in
                 let specialized_false =
                   Blang.specialize blang (fun i ->
                     if i = bit then `Known false else `Unknown)
                 in
                 match
                   ( Blang.constant_value specialized_true
                   , Blang.constant_value specialized_false )
                 with
                 | Some true, _ ->
                   Stop (`Finished (Map.add_exn current_known ~key:bit ~data:true))
                 | _, Some true ->
                   Stop (`Finished (Map.add_exn current_known ~key:bit ~data:false))
                 | Some false, Some false -> Stop `Backtrack
                 | Some false, _ ->
                   Continue
                     (specialized_false, Map.add_exn current_known ~key:bit ~data:false)
                 | _, Some false ->
                   Continue
                     (specialized_true, Map.add_exn current_known ~key:bit ~data:true)
                 | None, None -> Continue (blang, current_known)))
             ~finish:(fun (blang, current_known) -> `Unfinished (blang, current_known))
      in
      (match state with
       | `Finished known -> known
       | `Backtrack ->
         (match backtrack with
          | [] -> failwith "No solution found"
          | (blang, current_known) :: backtrack ->
            (find_smallest [@tailcall]) blang backtrack current_known)
       | `Unfinished (blang', current_known) ->
         if [%equal: int Blang.t] blang blang'
         then (
           let specialized_true =
             Blang.specialize blang (fun i -> if i = max then `Known true else `Unknown)
           in
           let specialized_false =
             Blang.specialize blang (fun i -> if i = max then `Known false else `Unknown)
           in
           (find_smallest [@tailcall])
             specialized_false
             ((specialized_true, Map.add_exn current_known ~key:max ~data:true)
              :: backtrack)
             (Map.add_exn current_known ~key:max ~data:false))
         else (find_smallest [@tailcall]) blang' backtrack current_known)
  in
  find_smallest is_satisfied [] Int.Map.empty
  |> Map.fold ~init:0 ~f:(fun ~key:bit ~data:is_set a ->
    if is_set then a + (1 lsl bit) else a)
;;
