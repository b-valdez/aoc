open! Core
include Angstrom
include Let_syntax
open Composition_infix

let skip_string chars = string chars *> return () <?> "skip_string"
let skip_while1 pred = skip pred *> skip_while pred <?> "skip_while1"
let space = char ' ' *> return () <?> "space"
let spaces = skip_many1 (char ' ') <?> "spaces"
let digit = satisfy Char.is_digit >>| Char.get_digit_exn <?> "digit"
let nat = take_while1 Char.is_digit >>| int_of_string <?> "nat"
let nat_eager = nat <* commit <?> "nat_eager"

let int =
  option Fn.id (char '-' *> return ( ~- ) <|> char '+' *> return Fn.id) <*> nat <?> "int"
;;

let word = take_while1 Char.is_alpha <?> "word"
let any_word = skip_while1 Char.is_alpha <?> "any_word"
let lines p = sep_by1 (end_of_line <* commit) p <?> "lines"
let lines_lazy p = sep_by1 end_of_line p <?> "lines no commit"
let blocks p = sep_by1 (end_of_line *> end_of_line <* commit) p <?> "blocks"

let many1_till p t =
  lift2 List.cons (t *> fail "terminated before first" <|> p) @@ many_till p t
  <?> "many1_till"
;;

(* untested *)
let many_unique =
  let rec aux p set =
    match%bind option None (p >>| Option.some) with
    | None -> return set
    | Some el -> (aux [@tailcall]) p (Set.add set el)
  in
  fun comparator p -> aux p (Set.empty comparator) <?> "many_unique"
;;

(* untested *)
let many_unique1 comparator p =
  lift2 (Fn.flip Set.add) p (many_unique comparator p) <?> "many_unique1"
;;

let many_unique_till comparator p e =
  fix (fun fixed ->
    e *> return (Set.empty comparator) <|> lift2 (Fn.flip Set.add) p fixed)
  <?> "many_unique_till"
;;

let pair ?(sep = return ()) p = lift2 Tuple2.create (p <* sep) p <?> "pair"
let triple ?(sep = return ()) p = lift3 Tuple3.create (p <* sep) (p <* sep) p <?> "triple"

let grid f =
  let line =
    many1_till (any_char >>| f) (end_of_line <|> end_of_input) >>| Array.of_list <* commit
  in
  many1 line >>| Array.of_list <?> "grid"
;;

let grid_start ~(equal : 'a -> 'a -> _) f start =
  let%bind grid_start = pos in
  let start_pos = Set_once.create () in
  let rec on_start =
    ref
    @@ fun cell ->
    if equal start cell
    then (
      pos
      >>| fun x ->
      Set_once.set_exn start_pos [%here] (x - grid_start - 1, 0);
      on_start := return;
      cell)
    else return cell
  in
  let%bind first_line =
    many_till (any_char >>= (f >> !on_start)) (end_of_line <|> end_of_input)
    <* commit
    >>| Array.of_list
    <?> "first line"
  in
  let line_length = Array.length first_line in
  (if Set_once.is_none start_pos
   then
     on_start
     := fun cell ->
          if equal start cell
          then (
            pos
            >>| fun x ->
            Set_once.set_exn
              start_pos
              [%here]
              ( (x - grid_start - 1) mod (line_length + 1)
              , (x - grid_start - 1) / (line_length + 1) );
            on_start := return;
            cell)
          else return cell);
  let line =
    count line_length (any_char >>= (f >> !on_start))
    >>| Array.of_list
    <?> "line with known length"
  in
  let%map other_lines = lines line in
  Set_once.get_exn start_pos [%here], first_line :: other_lines |> Array.of_list
;;

let tf_grid =
  let line =
    many1_till
      (char '#' >>| Fn.const true <|> (char '.' >>| Fn.const false))
      (end_of_line <|> end_of_input)
    >>| Array.of_list
    <* commit
  in
  many1_till line end_of_input >>| Array.of_list <?> "tf_grid"
;;

let tf_grid_lazy =
  let line =
    many1 (char '#' >>| Fn.const true <|> (char '.' >>| Fn.const false)) >>| Array.of_list
  in
  lines_lazy line >>| Array.of_list <?> "tf_grid_lazy"
;;

let sparse_tf_grid =
  scan_state
    (Set.empty (module Tuple.Comparator (Int) (Int)), (0, 0))
    (fun (acc, ((x, y) as pos)) -> function
      | '#' -> Some (Set.add acc pos, (x + 1, y))
      | '.' -> Some (acc, (x + 1, y))
      | '\n' -> Some (acc, (0, y + 1))
      | '\r' -> Some (acc, pos)
      | _ -> None)
  >>| fst
  <?> "sparse_tf_grid"
;;
