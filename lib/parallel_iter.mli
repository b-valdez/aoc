type 'a t = ('a -> unit) -> unit Picos.Computation.t

(** This does not work with infinite iterators *)
val from_iter : ?yield_every:int -> ?padded:bool -> 'a Iter.t -> 'a t

(** This does not work with infinite iterators *)
val from_iter2
  :  ?yield_every:int
  -> ?padded:bool
  -> (('a -> 'b -> unit) -> unit)
  -> ('a * 'b) t

(** This does not work with infinite iterators *)
val from_labelled_iter
  :  ?yield_every:int
  -> ?padded:bool
  -> (f:('a -> unit) -> unit)
  -> 'a t

(** This does not work with infinite iterators *)
val from_labelled_iter2
  :  ?yield_every:int
  -> ?padded:bool
  -> (f:('a -> 'b -> unit) -> unit)
  -> ('a * 'b) t

val from_fun : ?padded:bool -> ?yield_every:int -> (unit -> 'a option) -> 'a t
val empty : 'a t
val return : 'a -> 'a t
val singleton : 'a -> 'a t
val pure : 'a -> 'a t
val doubleton : 'a -> 'a -> 'a t
val also : 'a -> 'a t -> 'a t
val repeat : ?yield_every:int -> 'a -> 'a t
val init : ?yield_every:int -> f:(int -> 'a) -> 'a t
val iterate : ?yield_every:int -> ?padded:bool -> ('a -> 'a) -> 'a -> 'a t
val forever : ?yield_every:int -> (unit -> 'a) -> 'a t
val iter : f:('a -> unit) -> 'a t -> unit
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
val for_each : seq:'a t -> ('a -> unit) -> unit
val for_eachi : seq:'a t -> (int -> 'a -> unit) -> unit
val fold : f:('acc -> 'a -> 'acc) -> init:'acc -> 'a t -> 'acc
val foldi : f:('acc -> int -> 'a -> 'acc) -> init:'acc -> 'a t -> 'acc
val fold_map : f:('acc -> 'a -> 'acc * 'b) -> init:'acc -> 'a t -> 'b t
val fold_filter_map : f:('acc -> 'a -> 'acc * 'b option) -> init:'acc -> 'a t -> 'b t
val map : f:('a -> 'b) -> 'a t -> 'b t
val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
val filter : f:('a -> bool) -> 'a t -> 'a t
val combine : 'a t -> 'a t -> 'a t

(** This does not work with infinite lists *)
val combine_l : 'a t list -> 'a t

val concat : 'a t t -> 'a t
val flatten : 'a t t -> 'a t
val flat_map : f:('a -> 'b t) -> 'a t -> 'b t

(** This does not work with infinite lists *)
val flat_map_l : ?padded:bool -> f:('a -> 'b list) -> 'a t -> 'b t

val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
val filter_mapi : f:(int -> 'a -> 'b option) -> 'a t -> 'b t
val filter_count : f:('a -> bool) -> 'a t -> int
val keep_some : 'a option t -> 'a t
val keep_ok : ('a, _) result t -> 'a t
val keep_error : (_, 'e) result t -> 'e t
val unfoldr : ?yield_every:int -> ?padded:bool -> ('b -> ('a * 'b) option) -> 'b -> 'a t
val to_list : ?padded:bool -> 'a t -> 'a list
val take : ?padded:bool -> int -> 'a t -> 'a t
val throttle : ?padded:bool -> int -> 'a t -> 'a t
val for_all : f:('a -> bool) -> 'a t -> bool
val exists : f:('a -> bool) -> 'a t -> bool
val find_pred_exn : f:('a -> bool) -> 'a t -> 'a
val take_while : ?padded:bool -> f:('a -> bool) -> 'a t -> 'a t
val tap : f:('a -> unit) -> 'a t -> 'a t

exception Stream_closed

val stream_on
  :  ?poison:bool
  -> ?callstack:int
  -> 'a Picos_std_sync.Stream.t
  -> 'a t
  -> unit

val of_cursor : ?yield_every:int -> 'a Picos_std_sync.Stream.cursor -> 'a t

val batch_map
  :  ?padded:bool
  -> f:(('a * 'b Picos.Computation.t) array -> unit)
  -> 'a t
  -> 'b t

val batch_iter : ?padded:bool -> f:('a array -> unit) -> 'a t -> unit
val batch_fold : ?padded:bool -> f:('acc -> 'a array -> 'acc) -> init:'acc -> 'a t -> 'acc
val sum : ?padded:bool -> int t -> int

val iter_into_iter_and_stream
  :  ?padded:bool
  -> 'a Iter.t
  -> 'a Iter.t * 'a Picos_std_sync.Stream.t

module type Deriving_enum = sig
  type t [@@deriving enum]
end

val into_buckets
  :  ?padded:bool
  -> (module Deriving_enum with type t = 'bucket)
  -> project:('a -> 'bucket)
  -> 'a t
  -> 'a Dynarray.t array

val sort : ?padded:bool -> compare:('a -> 'a -> int) -> 'a t -> 'a array
