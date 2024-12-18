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

let rec improve_blang = function
  | Blang.And (_, _) as t ->
    let ts = Blang.gather_conjuncts t in
    (match List.find ts ~f:(fun blang -> Blang.length blang = 1) with
     | Some (Base value as singleton) ->
       Blang.and_
         [ singleton
         ; improve_blang
           @@ Blang.specialize t (fun value' ->
             if value = value' then `Known true else `Unknown)
         ]
     | Some (Not (Base value) as singleton) ->
       Blang.and_
         [ singleton
         ; improve_blang
           @@ Blang.specialize t (fun value' ->
             if value = value' then `Known false else `Unknown)
         ]
     | _ -> Blang.and_ (List.map ts ~f:improve_blang))
  | Blang.Or (_, _) as t ->
    let ts = Blang.gather_disjuncts t in
    (match List.find ts ~f:(fun blang -> Blang.length blang = 1) with
     | Some (Base value as singleton) ->
       Blang.or_
         [ singleton
         ; improve_blang
           @@ Blang.specialize t (fun value' ->
             if value = value' then `Known false else `Unknown)
         ]
     | Some (Not (Base value) as singleton) ->
       Blang.or_
         [ singleton
         ; improve_blang
           @@ Blang.specialize t (fun value' ->
             if value = value' then `Known true else `Unknown)
         ]
     | _ -> Blang.or_ (List.map ts ~f:improve_blang))
  | If (condition, then_, else_) ->
    Blang.if_
      (improve_blang condition)
      (improve_blang (Blang.and_ [ condition; then_ ]))
      (improve_blang (Blang.and_ [ Blang.not_ condition; else_ ]))
  | Not blang -> Blang.not_ @@ improve_blang blang
  | untouched -> untouched
;;

(* This solution requires manual effort *)
let part2
      ?(specialize = Fn.const `Unknown)
      ?(depth = 1)
      ?(digits_of_last_a = 5)
      (_, program)
  =
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
  let specialize blang = Blang.specialize blang specialize |> improve_blang in
  let combo_operand memoized step loop operand bit =
    match operand with
    | 4 -> memoized T.{ step = step - 2; loop; register = A; bit }
    | 5 -> memoized T.{ step = step - 2; loop; register = B; bit }
    | 6 -> memoized T.{ step = step - 2; loop; register = C; bit }
    | literal -> Blang.constant (literal land (1 lsl bit) <> 0)
  in
  let blang_xor a b =
    Blang.and_ [ Blang.or_ [ a; b ]; Blang.not_ @@ Blang.and_ [ a; b ] ] |> specialize
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
    | { loop = 0; step = 0; register = A; bit } -> Blang.base bit |> specialize
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
         |> specialize
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
  List.range 0 depth
  |> List.map ~f:(fun depth ->
    if depth < Array.length program
    then
      ( program.(depth)
      , List.map [ 0; 1; 2 ] ~f:(fun i ->
          i, memoized { loop = depth; step = out_index + 2; register; bit = i }) )
    else
      ( 0
      , List.map (List.range 0 digits_of_last_a) ~f:(fun i ->
          ( i
          , memoized
              { loop = depth - 1; step = Array.length program; register = A; bit = i } ))
      ))
;;
