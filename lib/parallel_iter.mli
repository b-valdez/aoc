type 'a t = ('a -> unit) -> unit

val from_iter : ?yield_every:int -> 'a Iter.t -> 'a t
val from_labelled_iter : ?yield_every:int -> (f:('a -> unit) -> unit) -> 'a t
val from_fun : ?yield_every:int -> (unit -> 'a option) -> 'a t
val empty : 'a t
val return : 'a -> 'a t
val singleton : 'a -> 'a t
val pure : 'a -> 'a t
val doubleton : 'a -> 'a -> 'a t
val also : 'a -> 'a t -> 'a t
val repeat : 'a -> 'a t
val init : f:(int -> 'a) -> 'a t
val iterate : ('a -> 'a) -> 'a -> 'a t
val forever : (unit -> 'a) -> 'a t
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
val combine_l : 'a t list -> 'a t
val concat : 'a t t -> 'a t
val flatten : 'a t t -> 'a t
val flat_map : f:('a -> 'b t) -> 'a t -> 'b t
val flat_map_l : f:('a -> 'b list) -> 'a t -> 'b t
val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
val filter_mapi : f:(int -> 'a -> 'b option) -> 'a t -> 'b t
val filter_count : f:('a -> bool) -> 'a t -> int
val keep_some : 'a option t -> 'a t
val keep_ok : ('a, _) result t -> 'a t
val keep_error : (_, 'e) result t -> 'e t
val unfoldr : ('b -> ('a * 'b) option) -> 'b -> 'a t
val to_list : 'a t -> 'a list
val take : int -> 'a t -> 'a t
val throttle : ?padded:bool -> int -> 'a t -> 'a t
val for_all : f:('a -> bool) -> 'a t -> bool
val exists : f:('a -> bool) -> 'a t -> bool
val take_while : f:('a -> bool) -> 'a t -> 'a t
val tap : f:('a -> unit) -> 'a t -> 'a t

val stream_on
  :  ?poison:bool
  -> ?callstack:int
  -> 'a Picos_std_sync.Stream.t
  -> 'a t
  -> unit

val of_cursor : 'a Picos_std_sync.Stream.cursor -> 'a t
