open! Core

type t = int * int [@@deriving sexp, compare]

let mem (low, high) i = Int.between ~low ~high i

(* maybe consider rewriting this as a proper t Set for putposes of binary search *)
type set = t list

let set_of_interval = List.singleton

let[@tail_mod_cons] rec merge a b =
  (* recursive calls try to bias argument order such that the head of a has a smaller start than the head of b *)
  match a, b with
  (* Nothing to do *)
  | [], rest | rest, [] -> rest
  (* not mergeable *)
  | ((_, end_) as head) :: tail, ((start, _) :: _ as other) when end_ + 1 < start ->
    head :: (merge [@tailcall]) tail other
  | ((start, _) :: _ as other), ((_, end_) as head) :: tail when end_ + 1 < start ->
    head :: (merge [@tailcall]) tail other
  (* yes mergeable *)
  | (start_a, end_a) :: a, (start_b, end_b) :: b when end_a < end_b ->
    (merge [@tailcall]) ((min start_a start_b, end_b) :: b) a
  | (start_a, end_a) :: a, (start_b, end_b) :: b when end_a > end_b ->
    (merge [@tailcall]) ((min start_a start_b, end_a) :: a) b
  | (start_a, end_) :: a, (start_b, _) :: b ->
    (min start_a start_b, end_) :: (merge [@tailcall]) a b
;;

let set_of_iter iter = Iter.fold ~init:[] ~f:(fun set t -> merge set [ t ]) iter

let[@tail_mod_cons] rec set_of_ordered_list = function
  | ([] | [ _ ]) as small_list -> small_list
  | ((_, end_) as head) :: ((start, _) :: _ as tail) when end_ + 1 < start ->
    head :: (set_of_ordered_list [@tailcall]) tail
  | (start, end_1) :: (_, end_2) :: tail ->
    (set_of_ordered_list [@tailcall]) ((start, max end_1 end_2) :: tail)
;;

let set_of_list list = list |> List.sort ~compare |> set_of_ordered_list
let length = List.sum (module Int) ~f:(fun (a, b) -> b - a + 1)

let rec set_mem set el =
  let open Int in
  match set with
  | [] -> false
  | (big_start, _) :: _ when big_start > el -> false
  | (_, small_end) :: tail when small_end < el -> (set_mem [@tailcall]) tail el
  | _ -> true
;;
