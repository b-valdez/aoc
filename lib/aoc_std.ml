(** {2 Iter} *)

module Iter = IterLabels

(** {2 Core} *)

include Core (** @inline *)

(** {2 Core_kernel Composition_Infix} *)

include Composition_infix (** @inline *)

module List = struct
  include List (** @inline *)

  (** {2 Added for AOC} *)

  let count_drop_while =
    let rec aux ~f count = function
      | hd :: tl when f hd -> (aux [@tailcall]) ~f (count + 1) tl
      | l -> count, l
    in
    aux 0
  ;;
end

(** {2 Angstrom} *)

module Angstrom = struct
  include Angstrom (** @inline *)

  include Let_syntax (**@inline*)

  (** {2 Added for AOC} *)

  let skip_string chars = string chars *> return () <?> "skip_string"
  let skip_while1 pred = skip pred *> skip_while pred
  let space = char ' ' *> return () <?> "space"
  let spaces = skip_many1 (char ' ') <?> "spaces"
  let digit = satisfy Char.is_digit >>| Char.get_digit_exn <?> "digit"
  let nat = take_while1 Char.is_digit >>| int_of_string <?> "nat"
  let nat_eager = nat <* commit <?> "nat_eager"

  let int =
    option Fn.id (char '-' *> return ( ~- ) <|> char '+' *> return Fn.id)
    <*> nat
    <?> "int"
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

  let many_unique comparator p =
    let empty_set = Set.empty comparator in
    let add_to_set = Fn.flip Set.add in
    fix (fun many_unique_p ->
      match%bind option None (p >>| Option.some) with
      | Some el -> lift (add_to_set el) many_unique_p
      | None -> empty_set |> return)
    <?> "many_unique"
  ;;

  let many_unique1 comparator p =
    lift2 (Fn.flip Set.add) p (many_unique comparator p) <?> "many_unique1"
  ;;

  let many_unique_till comparator p e =
    fix (fun fixed ->
      e *> return (Set.empty comparator) <|> lift2 (Fn.flip Set.add) p fixed)
    <?> "many_unique_till"
  ;;

  let pair ?(sep = return ()) p = lift2 Tuple2.create (p <* sep) p <?> "pair"

  let triple ?(sep = return ()) p =
    lift3 Tuple3.create (p <* sep) (p <* sep) p <?> "triple"
  ;;

  let grid f =
    let line =
      many1_till (any_char >>| f) (end_of_line <|> end_of_input)
      >>| Array.of_list
      <* commit
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
      many1 (char '#' >>| Fn.const true <|> (char '.' >>| Fn.const false))
      >>| Array.of_list
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
  ;;
end

(** {2 Added for AOC} *)

module Grid = struct
  type 'a t = 'a array array [@@deriving sexp]

  module Position = struct
    type t = int * int [@@deriving sexp, compare]

    include (val Comparator.make ~compare ~sexp_of_t)
  end

  module Direction = struct
    type t =
      [ `N
      | `E
      | `S
      | `W
      ]
    [@@deriving enumerate, sexp]

    type horizontal =
      [ `E
      | `W
      ]

    type vertical =
      [ `N
      | `S
      ]

    let step (x, y) = function
      | `N -> x, y - 1
      | `E -> x + 1, y
      | `S -> x, y + 1
      | `W -> x - 1, y
    ;;
  end

  let ( .?() ) : 'a t -> _ -> 'a option =
    fun grid (i, j) -> Option.try_with @@ fun () -> grid.(j).(i)
  ;;

  let ( .^() ) : 'a t -> _ -> 'a = fun grid (i, j) -> grid.(j).(i)
end

let parse_string parser =
  Angstrom.parse_string ~consume:All parser >> Result.ok_or_failwith
;;

let min_max ~compare a b = if compare a b < 0 then a, b else b, a

let gcd a b =
  let rec aux a b =
    match b with
    | 1 -> 1
    | 0 -> a
    | b -> aux b (a mod b)
  in
  let a, b = min_max ~compare a b in
  aux b a
;;

let lcm a b = a * b / gcd a b
