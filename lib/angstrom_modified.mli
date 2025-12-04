include module type of Angstrom with type 'a t = 'a Angstrom.t
include module type of Let_syntax

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
  -> ('a, 'b) Core.Set.t t

val many_unique1 : ('a, 'b) Core.Comparator.Module.t -> 'a t -> ('a, 'b) Core.Set.t t

val many_unique_till
  :  ('a, 'b) Core.Comparator.Module.t
  -> 'a t
  -> 'c t
  -> ('a, 'b) Core.Set.t t

val bi_multi_map
  :  ?sep_pair:_ t
  -> ?sep_pairs:_ t
  -> ('a, 'b) Core.Comparator.Module.t
  -> 'a t
  -> ('a, ('a, 'b) Core.Set.t, 'b) Core.Map.t t

val pair : ?sep:_ t -> 'a t -> ('a, 'a) Core.Tuple2.t t
val triple : ?sep:_ t -> 'a t -> ('a, 'a, 'a) Core.Tuple3.t t
val grid : (char -> 'a) -> 'a array array t

val grid_start
  :  equal:('a -> 'a -> bool)
  -> (char -> 'a)
  -> 'a
  -> (Grid.Position.t * 'a array array) t

val tf_grid : bool array array t
val tf_grid_lazy : bool array array t
val sparse_tf_grid : ?true_:char -> ?false_:char -> unit -> Grid.Position.Set.t t
val sparse_tf_grid_start : char -> (Grid.Position.t * Grid.Position.Set.t * int * int) t
val sparse_grid : (Grid.Position.t list Core.Char.Map.t * int * int) t
val count_till : bool t -> _ t -> int t
