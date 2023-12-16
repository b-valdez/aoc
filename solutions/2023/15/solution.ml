open! Aoc_std

let parser =
  let open Angstrom in
  sep_by1
    (char ',' *> commit)
    (sep_by1
       end_of_line
       (take_while (function
         | '\n' | '\r' | ',' -> false
         | _ -> true))
     >>| String.concat)
;;

let hash = String.fold ~init:0 ~f:(fun hash c -> (Char.to_int c + hash) * 17 mod 256)
let part1 = List.map ~f:hash

type operation =
  | Remove
  | Put of int

let put int = Put int

(* We could just use a Hashtbl, but that would be boring*)
let part2 parsed =
  let state = Array.create ~len:256 [] in
  let[@tail_mod_cons] rec remove key = function
    | [] -> []
    | (key', _) :: more when String.equal key' key -> more
    | hd :: more -> hd :: (remove [@tailcall]) key more
  in
  let[@tail_mod_cons] rec insert key to_put = function
    | [] -> [ key, to_put ]
    | (key', _) :: more when String.equal key key' -> (key, to_put) :: more
    | hd :: more -> hd :: (insert [@tailcall]) key to_put more
  in
  let update key = function
    | Remove -> remove key
    | Put to_put -> insert key to_put
  in
  List.iter parsed ~f:(fun string ->
    let key, operation =
      Angstrom.parse_string
        ~consume:All
        Angstrom.(both word (char '-' *> return Remove <|> (char '=' *> nat >>| put)))
        string
      |> Result.ok_or_failwith
    in
    let hash = hash key in
    state.(hash) <- update key operation state.(hash));
  state
;;

let focussing_power =
  Array.foldi ~init:0 ~f:(fun i sum lenses ->
    sum
    + ((i + 1)
       * List.foldi lenses ~init:0 ~f:(fun inner_i inner_sum (_, focal_length) ->
         inner_sum + ((inner_i + 1) * focal_length))))
;;

let%expect_test "sample" =
  let parsed = parse_string parser Sample.sample in
  print_s @@ [%sexp_of: int list] @@ part1 parsed;
  [%expect {| (30 253 97 47 14 180 9 197 48 214 231) |}];
  let part2 = part2 parsed in
  print_s @@ [%sexp_of: (string * int) list array] @@ part2;
  [%expect
    {|
    (((rn 1) (cm 2)) () () ((ot 7) (ab 5) (pc 6)) () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () () () () () () () () () ()
     () () () () () () () () () () () () () () () () ()) |}];
  printf "%d" @@ focussing_power part2;
  [%expect {| 145 |}]
;;

let%expect_test "input" =
  let parsed = parse_string parser Input.input in
  printf "%d" @@ List.sum (module Int) (part1 parsed) ~f:Fn.id;
  [%expect {| 505459 |}];
  printf "%d" @@ focussing_power (part2 parsed);
  [%expect {| 228508 |}]
;;
