open Picos_std_structured
open Picos_std_sync

module Batched_unordered_list = struct
  type ('a, 'b) op =
    | Insert_no_yield : 'a -> ('a, unit) op
    | Yield : ('a, 'a list) op

  module Service = struct
    type 'a t = 'a list ref
    type cfg = Core.Nothing.t
    type nonrec ('a, 'b) op = ('a, 'b) op
    type 'a wrapped_op = Mk : ('a, 'b) op * 'b Picos.Computation.t -> 'a wrapped_op

    let init ?cfg:_ () = ref []

    let run (type a) list ops =
      let open Picos in
      let f : a wrapped_op -> _ = function
        | Mk (Insert_no_yield el, comp) ->
          list := el :: !list;
          Computation.finish comp
        | Mk (Yield, comp) -> Computation.return comp !list
      in
      Array.iter f ops
    ;;
  end

  include Obatcher.Make_Poly (Service)
end

(* TODO change to unit Promise, see if you can make take work this way *)
type 'a t = ('a -> unit) -> unit

(** Substract the Control.Terminate exception. Only use this in the main Fiber of a iter to prevent iter from returning Terminate *)
let[@inline] ( let- ) k thunk =
  match (thunk [@inlined]) () with
  | res -> (k [@inlined]) res
  | exception Control.Terminate -> ()
;;

(** May best attempt at a fork. Only use this in the main Fiber of a iter to prevent iter from returning Terminate *)
let[@inline] ( let^|^ ) k thunk =
  match Flock.fork thunk with
  | () -> (k [@inlined]) ()
  | exception Control.Terminate -> ()
;;

let[@inline] periodic_yield = function
  | None -> fun [@inline] _ () -> ()
  | Some interval ->
    fun [@inline] phase_ref () ->
      if !phase_ref = interval
      then (
        phase_ref := 1;
        Control.yield ();
        Control.raise_if_canceled ())
      else incr phase_ref
;;

let to_list iter =
  let open Batched_unordered_list in
  let list = init () in
  Flock.join_after (fun () -> iter (fun a -> exec list (Insert_no_yield a)));
  exec list Yield
;;

(* TODO optional arguments for yield frequency in all constructors *)

(* TODO  does yield_every=None get phase compiled away? *)
let from_iter ?yield_every iter f =
  let phase = ref 1 in
  let[@inline] periodic_yield () = periodic_yield yield_every phase () in
  try
    iter (fun el ->
      Flock.fork (fun () ->
        Control.raise_if_canceled ();
        f el);
      periodic_yield ())
  with
  | Control.Terminate -> ()
;;

let from_labelled_iter ?yield_every iter = from_iter ?yield_every (fun f -> iter ~f)

let[@specialise] rec from_fun ?yield_every:_ f k =
  match f () with
  | None | (exception Control.Terminate) -> ()
  | Some x ->
    let^|^ () =
      fun () ->
      Control.raise_if_canceled ();
      k x
    in
    Control.yield ();
    let- () = Control.raise_if_canceled in
    from_fun f k
;;

let[@inline] empty _ = ()

let (return as singleton as pure) =
  fun [@inline] x f ->
  try f x with
  | Control.Terminate -> ()
;;

let doubleton x y k =
  let^|^ () =
    fun () ->
    Control.raise_if_canceled ();
    k y
  in
  try k x with
  | Control.Terminate -> ()
;;

let also x l k =
  let^|^ () =
    fun () ->
    Control.raise_if_canceled ();
    k x
  in
  l k
;;

let[@specialise] rec repeat x k =
  let^|^ () =
    fun () ->
    Control.raise_if_canceled ();
    k x
  in
  Control.yield ();
  let- () = Control.raise_if_canceled in
  repeat x k
;;

let[@specialise] rec init_aux f yield i =
  let^|^ () =
    fun () ->
    Control.raise_if_canceled ();
    yield (f i)
  in
  Control.yield ();
  let- () = Control.raise_if_canceled in
  init_aux f yield (i + 1)
;;

let[@inline] init ~f yield = (init_aux [@inlined]) f yield 0
let iterate f x = Iter.iterate f x |> from_iter

let[@specialise] rec forever f k =
  let^|^ () =
    fun () ->
    Control.raise_if_canceled ();
    k (f ())
  in
  Control.yield ();
  let- () = Control.raise_if_canceled in
  (forever [@tailcall]) f k
;;

let iter ~f seq = Flock.join_after (fun () -> seq f)

(** TODO could be batched *)
let iteri ~f seq =
  let counter = Atomic.make 0 in
  Flock.join_after (fun () -> seq (fun x -> f (Atomic.fetch_and_add counter 1) x))
;;

let for_each ~seq f = Flock.join_after (fun () -> seq f)
let for_eachi ~seq f = iteri ~f seq

(** TODO could be batched *)
let fold ~f ~init seq =
  let mutex = Mutex.create () in
  let r = ref init in
  Flock.join_after (fun () ->
    seq (fun elt -> Mutex.protect mutex (fun () -> r := f !r elt)));
  !r
;;

(** TODO could be batched *)
let foldi ~f ~init seq =
  let mutex = Mutex.create () in
  let i = Atomic.make 0 in
  let r = ref init in
  Flock.join_after (fun () ->
    seq (fun elt ->
      Mutex.protect mutex (fun () -> r := f !r (Atomic.fetch_and_add i 1) elt)));
  !r
;;

(** TODO could be batched *)
let fold_map ~f ~init seq yield =
  let mutex = Mutex.create () in
  let r = ref init in
  seq (fun x ->
    let y =
      Mutex.protect mutex (fun () ->
        let acc', y = f !r x in
        r := acc';
        y)
    in
    yield y)
;;

(** TODO could be batched *)
let fold_filter_map ~f ~init seq yield =
  let mutex = Mutex.create () in
  let r = ref init in
  seq (fun [@inline] x ->
    let y =
      (Mutex.protect [@inlined]) mutex (fun () ->
        let acc', y = f !r x in
        r := acc';
        y)
    in
    match y with
    | None -> ()
    | Some y' -> yield y')
;;

let map ~f seq k = seq (fun x -> k (f x))

let mapi ~f seq k =
  let i = Atomic.make 0 in
  seq (fun x -> k (f (Atomic.fetch_and_add i 1) x))
;;

let filter ~f seq k = seq (fun x -> if f x then k x)

let combine s1 s2 k =
  Flock.fork (fun () -> s2 k);
  s1 k
;;

let combine_l l k = List.iter ~f:(fun seq -> Flock.fork (fun () -> seq k)) l
let (concat as flatten) = fun seq_seq k -> seq_seq (fun seq -> seq k)
let flat_map ~f seq k = seq (fun x -> f x k)

let flat_map_l ~f seq k =
  seq (fun x -> List.iter (f x) ~f:(fun el -> Flock.fork (fun () -> k el)))
;;

let filter_map ~f seq k =
  seq (fun x ->
    match f x with
    | None -> ()
    | Some y -> k y)
;;

let filter_mapi ~f seq k =
  let i = Atomic.make 0 in
  seq (fun x ->
    match f (Atomic.fetch_and_add i 1) x with
    | None -> ()
    | Some y -> k y)
;;

let filter_count ~f seq =
  let open Ds.Batched_counter in
  let i = init () in
  Flock.join_after (fun () -> seq (fun x -> if f x then incr i));
  get i
;;

let keep_some seq k =
  seq (function
    | Some x -> k x
    | None -> ())
;;

let keep_ok seq k =
  seq (function
    | Ok x -> k x
    | Error _ -> ())
;;

let keep_error seq k =
  seq (function
    | Error exn -> k exn
    | Ok _ -> ())
;;

let[@specialise] rec unfoldr f b k =
  match f b with
  | None -> ()
  | Some (x, b) ->
    let^|^ () =
      fun () ->
      Control.raise_if_canceled ();
      k x
    in
    Control.yield ();
    let- () = Control.raise_if_canceled in
    unfoldr f b k
;;

let throttle ?padded size seq k =
  let module Semaphore = Picos_std_sync.Semaphore.Counting in
  let semaphore = Semaphore.make ?padded size in
  seq (fun x ->
    Semaphore.acquire semaphore;
    match k x with
    | () | (exception Control.Terminate) -> Semaphore.release semaphore)
;;

let for_all ~f seq =
  let ret = Ivar.create () in
  Flock.join_after (fun () ->
    seq (fun x ->
      if not (f x)
      then (
        Ivar.fill ret ();
        Flock.terminate ())));
  match Ivar.peek_opt ret with
  | None -> true
  | Some () -> false
;;

let exists ~f seq =
  let ret = Ivar.create () in
  Flock.join_after (fun () ->
    seq (fun x ->
      if f x
      then (
        Ivar.fill ret ();
        Flock.terminate ())));
  match Ivar.peek_opt ret with
  | None -> false
  | Some () -> true
;;

(* TODO audit Terminate-returns: 'a t should never return Terminate, if true: simplify {take}-error-handling *)

let take count seq k =
  let count = Atomic.make count in
  let promise = ref @@ Promise.of_value () in
  promise
  := Flock.fork_as_promise (fun () ->
       seq (fun x ->
         Control.raise_if_canceled ();
         let to_take = Atomic.fetch_and_add count ~-1 in
         if to_take = 1 then Promise.terminate !promise;
         if to_take > 0 then (* TODO check this *) Flock.fork (fun () -> k x)))
;;

(* TODO replace join_after, see {take} *)
let take_while ~f seq k =
  let promise = ref @@ Promise.of_value () in
  promise
  := Flock.fork_as_promise (fun () ->
       seq (fun x ->
         Control.raise_if_canceled ();
         if f x then Flock.fork (fun () -> k x) else Promise.terminate !promise))
;;

(* TODO custom exn *)
let stream_on ?(poison = false) ?(callstack = 0) stream seq =
  Flock.join_after (fun () -> seq (fun x -> Stream.push stream x));
  if poison then Stream.poison stream Control.Terminate (Printexc.get_callstack callstack)
;;

let of_cursor cursor seq =
  (unfoldr [@inlined])
    (fun [@inline] cursor ->
      match Stream.read cursor with
      | read_result -> Some read_result
      | exception Control.Terminate -> None)
    cursor
    seq
;;

let tap ~f seq k =
  seq (fun x ->
    f x;
    k x)
;;
