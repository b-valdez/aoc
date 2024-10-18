(** {2 Iter} *)

module Iter = Iter
module Parallel_iter = Parallel_iter

(** {2 Core} *)

include (Core : module type of Core with module List := Core.List) (** @inline *)

module List = List

(** {2 Core_kernel Composition_Infix} *)

include Composition_infix (** @inline *)

(** {2 Angstrom} *)

module Angstrom = Angstrom_modified

(** {2 Added for AOC} *)

module Grid = Grid

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

include Utils
