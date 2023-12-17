include module type of Angstrom with type 'a t = 'a Angstrom.t
include module type of Let_syntax

val skip_string : string -> unit t
val skip_while1 : (char -> bool) -> unit t
val space : unit t
val spaces : unit t
val digit : int t
val nat : int t
val nat_eager : int t
val int : int t
val word : string t
val any_word : unit t
val lines : 'a t -> 'a list t
val lines_lazy : 'a t -> 'a list t
val blocks : 'a t -> 'a list t
val many1_till : 'a t -> 'b t -> 'a list t

val many_unique
  :  ('a, 'b) Core.Comparator.Module.t
  -> 'a Angstrom.t
  -> ('a, 'b) Base.Set.t t

val many_unique1 : ('a, 'b) Core.Comparator.Module.t -> 'a t -> ('a, 'b) Base.Set.t t

val many_unique_till
  :  ('a, 'b) Core.Comparator.Module.t
  -> 'a t
  -> 'c t
  -> ('a, 'b) Base.Set.t t

val pair : ?sep:unit t -> 'a t -> ('a, 'a) Core.Tuple2.t t
val triple : ?sep:unit t -> 'a t -> ('a, 'a, 'a) Core.Tuple3.t t
val grid : (char -> 'a) -> 'a array array t

val grid_start
  :  equal:('a -> 'a -> bool)
  -> (char -> 'a)
  -> 'a
  -> ((int * int) * 'a array array) t

val tf_grid : bool array array t
val tf_grid_lazy : bool array array t

val sparse_tf_grid
  : ( Core.Tuple.Make(Core.Int)(Core.Int).t
      , Core.Tuple.Comparator(Core.Int)(Core.Int).comparator_witness )
      Base.Set.t
      t
