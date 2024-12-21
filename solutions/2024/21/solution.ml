(* This one changed quite a bit between solving part1 and part2. Initially part1 was solved with a_star but it wasn't efficient enough for part2.
   Presumably this was because the heuristic I used was inadequate. I couldn't find a better heuristic without basically trying to derive a closed form but 
   I just wrote some code for a parallel recursive memoizing of functions which came quite handy! *)
open! Aoc_std

type numpad =
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Enter
[@@deriving compare, sexp]

type dpad =
  | Up
  | Down
  | Left
  | Right
  | Activate
[@@deriving equal, compare, sexp, hash]

let parser =
  let open Angstrom in
  many_till
    (choice
       [ char '0' *> return Zero
       ; char '1' *> return One
       ; char '2' *> return Two
       ; char '3' *> return Three
       ; char '4' *> return Four
       ; char '5' *> return Five
       ; char '6' *> return Six
       ; char '7' *> return Seven
       ; char '8' *> return Eight
       ; char '9' *> return Nine
       ; char 'A' *> return Enter
       ])
    (end_of_line <|> end_of_input)
;;

let numpad_to_pos = function
  | Zero -> 1, 3
  | One -> 0, 2
  | Two -> 1, 2
  | Three -> 2, 2
  | Four -> 0, 1
  | Five -> 1, 1
  | Six -> 2, 1
  | Seven -> 0, 0
  | Eight -> 1, 0
  | Nine -> 2, 0
  | Enter -> 2, 3
;;

let pos_to_numpad = function
  | 1, 3 -> Zero
  | 0, 2 -> One
  | 1, 2 -> Two
  | 2, 2 -> Three
  | 0, 1 -> Four
  | 1, 1 -> Five
  | 2, 1 -> Six
  | 0, 0 -> Seven
  | 1, 0 -> Eight
  | 2, 0 -> Nine
  | 2, 3 -> Enter
  | _ -> assert false
;;

module Dpad_tuple = struct
  type t = int * dpad * dpad [@@deriving equal, hash]
end

let navigate_and_press =
  Tuple3.curry
  @@ Par_memo.fix
       (module Dpad_tuple)
       (fun navigate_and_press (layer, from, to_) ->
          if layer = -1
          then 1
          else (
            let navigate_and_press_sequence sequence =
              snd
              @@ List.fold sequence ~init:(Activate, 0) ~f:(fun (last, cost) current ->
                current, cost + navigate_and_press (layer - 1, last, current))
            in
            match from, to_ with
            | Up, Up | Down, Down | Left, Left | Right, Right | Activate, Activate ->
              navigate_and_press_sequence [ Activate ]
            | Down, Up | Right, Activate -> navigate_and_press_sequence [ Up; Activate ]
            | Up, Down | Activate, Right -> navigate_and_press_sequence [ Down; Activate ]
            | Activate, Up | Right, Down | Down, Left ->
              navigate_and_press_sequence [ Left; Activate ]
            | Right, Left -> navigate_and_press_sequence [ Left; Left; Activate ]
            | Up, Activate | Down, Right | Left, Down ->
              navigate_and_press_sequence [ Right; Activate ]
            | Left, Right -> navigate_and_press_sequence [ Right; Right; Activate ]
            | Up, Right ->
              min
                (navigate_and_press_sequence [ Right; Down; Activate ])
                (navigate_and_press_sequence [ Down; Right; Activate ])
            | Right, Up ->
              min
                (navigate_and_press_sequence [ Left; Up; Activate ])
                (navigate_and_press_sequence [ Up; Left; Activate ])
            | Activate, Down ->
              min
                (navigate_and_press_sequence [ Left; Down; Activate ])
                (navigate_and_press_sequence [ Down; Left; Activate ])
            | Down, Activate ->
              min
                (navigate_and_press_sequence [ Right; Up; Activate ])
                (navigate_and_press_sequence [ Up; Right; Activate ])
            | Up, Left -> navigate_and_press_sequence [ Down; Left; Activate ]
            | Left, Up -> navigate_and_press_sequence [ Right; Up; Activate ]
            | Left, Activate ->
              min
                (navigate_and_press_sequence [ Right; Right; Up; Activate ])
                (navigate_and_press_sequence [ Right; Up; Right; Activate ])
            | Activate, Left ->
              min
                (navigate_and_press_sequence [ Down; Left; Left; Activate ])
                (navigate_and_press_sequence [ Left; Down; Left; Activate ])))
;;

let navigate_and_press_sequence layer sequence =
  snd
  @@ List.fold sequence ~init:(Activate, 0) ~f:(fun (last, cost) current ->
    current, cost + navigate_and_press (layer - 1) last current)
;;

let rec keypresses_between from to_ =
  let try_left =
    match from with
    | Zero -> false
    | _ -> fst (numpad_to_pos to_) < fst (numpad_to_pos from)
  in
  let try_right = fst (numpad_to_pos to_) > fst (numpad_to_pos from) in
  let try_up = snd (numpad_to_pos to_) < snd (numpad_to_pos from) in
  let try_down =
    match from with
    | One -> false
    | _ -> snd (numpad_to_pos to_) > snd (numpad_to_pos from)
  in
  match try_left, try_right, try_up, try_down with
  | false, false, false, false -> [ [ Activate ] ]
  | true, false, false, false ->
    List.map
      (keypresses_between
         (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `W))
         to_)
      ~f:(List.cons Left)
  | false, true, false, false ->
    List.map
      (keypresses_between
         (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `E))
         to_)
      ~f:(List.cons Right)
  | false, false, true, false ->
    List.map
      (keypresses_between
         (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `N))
         to_)
      ~f:(List.cons Up)
  | false, false, false, true ->
    List.map
      (keypresses_between
         (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `S))
         to_)
      ~f:(List.cons Down)
  | true, false, true, false ->
    List.map
      (keypresses_between
         (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `W))
         to_)
      ~f:(List.cons Left)
    @ List.map
        (keypresses_between
           (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `N))
           to_)
        ~f:(List.cons Up)
  | true, false, false, true ->
    List.map
      (keypresses_between
         (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `W))
         to_)
      ~f:(List.cons Left)
    @ List.map
        (keypresses_between
           (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `S))
           to_)
        ~f:(List.cons Down)
  | false, true, true, false ->
    List.map
      (keypresses_between
         (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `E))
         to_)
      ~f:(List.cons Right)
    @ List.map
        (keypresses_between
           (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `N))
           to_)
        ~f:(List.cons Up)
  | false, true, false, true ->
    List.map
      (keypresses_between
         (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `E))
         to_)
      ~f:(List.cons Right)
    @ List.map
        (keypresses_between
           (pos_to_numpad (Grid.Direction.step (numpad_to_pos from) `S))
           to_)
        ~f:(List.cons Down)
  | _ -> assert false
;;

let solve n cursor =
  Parallel_iter.of_cursor cursor
  |> Parallel_iter.map ~f:(fun code ->
    let _, num_inputs =
      List.fold ~init:(Enter, 0) code ~f:(fun (last, sum) code ->
        let keypresses = keypresses_between last code in
        ( code
        , sum
          + (Iter.of_list keypresses
             |> Iter.map ~f:(navigate_and_press_sequence n)
             |> Iter.min_exn ~lt:( < )) ))
    in
    let code_num =
      List.fold code ~init:0 ~f:(fun acc -> function
        | Zero -> acc * 10
        | One -> (acc * 10) + 1
        | Two -> (acc * 10) + 2
        | Three -> (acc * 10) + 3
        | Four -> (acc * 10) + 4
        | Five -> (acc * 10) + 5
        | Six -> (acc * 10) + 6
        | Seven -> (acc * 10) + 7
        | Eight -> (acc * 10) + 8
        | Nine -> (acc * 10) + 9
        | Enter -> acc)
    in
    num_inputs * code_num)
  |> Parallel_iter.sum ~padded:true
;;
