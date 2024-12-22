open Picos
open Picos_std_sync

type 'a t = ('a -> unit) -> unit Computation.t

(** TODO ?callstack *)
let[@inline] fork outer_computation thunk =
  let fiber = Fiber.create ~forbid:false outer_computation in
  Fiber.spawn fiber
  @@ fun fiber ->
  try thunk fiber with
  | exn -> Computation.cancel outer_computation exn (Printexc.get_raw_backtrace ())
;;

let[@inline] periodic_yield = function
  | None -> fun [@inline] _ -> 1
  | Some interval ->
    fun [@inline] phase ->
      if phase = interval
      then (
        Fiber.yield ();
        1)
      else phase + 1
;;

(* TODO optional arguments for yield frequency in all constructors *)

let from_iter ?yield_every ?padded iter k =
  let latch = Latch.create ?padded 1 in
  (* since we assume iter to be sequential phase doesn't need to be atomic *)
  let phase = ref 1 in
  let computation = Computation.create () in
  fork computation (fun _ ->
    iter (fun x ->
      Latch.incr latch;
      fork computation (fun _ ->
        if Computation.is_running computation
        then (
          k x;
          Latch.decr latch));
      phase := periodic_yield yield_every !phase);
    Latch.decr latch;
    Latch.await latch;
    Computation.finish computation);
  computation
;;

let from_iter2 ?yield_every ?padded iter k =
  let latch = Latch.create ?padded 1 in
  (* since we assume iter to be sequential phase doesn't need to be atomic *)
  let phase = ref 1 in
  let computation = Computation.create () in
  fork computation (fun _ ->
    iter (fun x y ->
      Latch.incr latch;
      fork computation (fun _ ->
        if not @@ Computation.is_canceled computation
        then (
          k (x, y);
          Latch.decr latch));
      phase := periodic_yield yield_every !phase);
    Latch.decr latch;
    Latch.await latch;
    Computation.finish computation);
  computation
;;

let from_labelled_iter ?yield_every ?padded iter =
  from_iter ?yield_every ?padded (fun f -> iter ~f)
;;

let from_labelled_iter2 ?yield_every ?padded iter =
  from_iter2 ?yield_every ?padded (fun f -> iter ~f)
;;

let[@specialise] rec from_fun_aux yield_every outer_computation latch f k phase =
  match f () with
  | None ->
    Latch.decr latch;
    Latch.await latch;
    Computation.finish outer_computation
  | Some x ->
    Latch.incr latch;
    fork outer_computation (fun _ ->
      if not @@ Computation.is_canceled outer_computation
      then (
        k x;
        Latch.decr latch));
    let phase = periodic_yield yield_every phase in
    from_fun_aux yield_every outer_computation latch f k phase
;;

let from_fun ?padded ?yield_every f k =
  let computation = Computation.create () in
  let latch = Latch.create ?padded 1 in
  fork computation (fun _ -> from_fun_aux yield_every computation latch f k 1);
  computation
;;

let[@inline] empty _ = Computation.finished

let (return as singleton as pure) =
  fun [@inline] x f ->
  let computation = Computation.create () in
  fork computation (fun _ ->
    if not @@ Computation.is_canceled computation
    then (
      f x;
      Computation.finish computation));
  computation
;;

let doubleton x y k =
  let computation = Computation.create () in
  let x_trigger = Trigger.create () in
  fork computation (fun _ ->
    if not @@ Computation.is_canceled computation
    then (
      k x;
      Trigger.signal x_trigger));
  fork computation (fun _ ->
    if not @@ Computation.is_canceled computation
    then (
      k y;
      match Trigger.await x_trigger with
      | None -> Computation.finish computation
      | Some (exn, backtrace) -> Computation.cancel computation exn backtrace));
  computation
;;

(* TODO audit attach_canceler *)

let also x iter k =
  let computation = Computation.create () in
  let inner_computation = iter k in
  let cancel_outwards = Computation.canceler ~from:inner_computation ~into:computation in
  Computation.try_attach inner_computation cancel_outwards |> ignore;
  let cancel_inwards =
    Computation.attach_canceler ~from:computation ~into:inner_computation
  in
  let (Packed current_computation) = Fiber.get_computation (Fiber.current ()) in
  fork current_computation (fun _ ->
    Computation.wait inner_computation;
    Computation.detach computation cancel_inwards);
  fork computation (fun _ ->
    k x;
    Computation.detach inner_computation cancel_outwards;
    Computation.await inner_computation;
    Computation.finish computation);
  computation
;;

let[@specialise] rec repeat_aux yield_every outer_computation x k phase =
  fork outer_computation (fun _ -> k x);
  let phase = periodic_yield yield_every phase in
  repeat_aux yield_every outer_computation x k phase
;;

let repeat ?yield_every x k =
  let computation = Computation.create () in
  fork computation (fun _ -> repeat_aux yield_every computation x k 1);
  computation
;;

let[@specialise] rec init_aux yield_every computation f yield i phase =
  fork computation (fun _ -> yield (f i));
  let phase = periodic_yield yield_every phase in
  init_aux yield_every computation f yield (i + 1) phase
;;

let[@inline] init ?yield_every ~f yield =
  let computation = Computation.create () in
  fork computation (fun _ -> (init_aux [@inlined]) yield_every computation f yield 0 1);
  computation
;;

(* TODO implement this without the latch inside from_iter -- it doen't make sense in a known to be infinete iterator *)
let iterate ?yield_every ?padded f x = Iter.iterate f x |> from_iter ?yield_every ?padded

let[@specialise] rec forever_aux yield_every computation f k phase =
  fork computation (fun _ -> k (f ()));
  let phase = periodic_yield yield_every phase in
  (forever_aux [@tailcall]) yield_every computation f k phase
;;

let forever ?yield_every f k =
  let computation = Computation.create () in
  fork computation (fun _ -> forever_aux yield_every computation f k 1);
  computation
;;

let iter ~f seq =
  let computation = seq f in
  let (Packed context) = Fiber.(get_computation (current ())) in
  Computation.check context;
  let canceler = Computation.attach_canceler ~from:context ~into:computation in
  Computation.await computation;
  Computation.detach context canceler
;;

let iteri ~f seq =
  let counter = Atomic.make 0 in
  iter seq ~f:(fun x -> f (Atomic.fetch_and_add counter 1) x)
;;

let for_each ~seq f = iter seq ~f
let for_eachi ~seq f = iteri ~f seq

let fold ?padded ~f ~init seq =
  let mutex = Mutex.create ?padded () in
  let r = ref init in
  iter seq ~f:(fun elt -> Mutex.protect mutex (fun () -> r := f !r elt));
  !r
;;

let foldi ?padded ~f ~init seq =
  let mutex = Mutex.create ?padded () in
  let i = ref 0 in
  let r = ref init in
  iter seq ~f:(fun elt ->
    Mutex.protect mutex (fun () ->
      r := f !r !i elt;
      incr i));
  !r
;;

let fold_map ?padded ~f ~init seq yield =
  let mutex = Mutex.create ?padded () in
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

let fold_filter_map ?padded ~f ~init seq yield =
  let mutex = Mutex.create ?padded () in
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

(* TODO tsan test for raciness, *)
let combine s1 s2 k =
  let computation = Computation.create () in
  let s1_computation = s1 k in
  let s2_computation = s2 k in
  let s1_canceler = Computation.attach_canceler ~from:computation ~into:s1_computation in
  let s2_canceler = Computation.attach_canceler ~from:computation ~into:s2_computation in
  (* The outwards cancelers are implicitly given by the await in the forks *)
  fork computation (fun _ ->
    Computation.await s1_computation;
    if Computation.is_running s2_computation
    then Computation.detach computation s1_canceler
    else Computation.finish computation);
  fork computation (fun _ ->
    Computation.await s2_computation;
    if Computation.is_running s1_computation
    then Computation.detach computation s2_canceler
    else Computation.finish computation);
  computation
;;

let combine_l l k =
  let computation = Computation.create () in
  fork computation (fun _ ->
    List.filter_map l ~f:(fun seq ->
      let inner_computation = seq k in
      if Computation.is_running computation
      then (
        let inwards_canceler =
          Computation.attach_canceler ~from:computation ~into:inner_computation
        in
        fork computation (fun _ ->
          Computation.await inner_computation;
          Computation.detach computation inwards_canceler);
        Some inner_computation)
      else None)
    |> List.iter ~f:Computation.wait;
    Computation.finish computation);
  computation
;;

let (concat as flatten) =
  fun seq_seq k ->
  let computation = Ivar.create () in
  seq_seq (fun seq ->
    let inner_computation = seq k in
    let computation = Ivar.read computation in
    if Computation.is_running computation
    then (
      let inwards_canceler =
        Computation.attach_canceler ~from:computation ~into:inner_computation
      in
      Computation.await inner_computation;
      Computation.detach computation inwards_canceler))
  |> Ivar.fill computation;
  Ivar.read computation
;;

let flat_map ~f seq k =
  let computation = Ivar.create () in
  seq (fun x ->
    let inner_computation = f x k in
    let computation = Ivar.read computation in
    if Computation.is_running computation
    then (
      let inwards_canceler =
        Computation.attach_canceler ~from:computation ~into:inner_computation
      in
      Computation.await inner_computation;
      Computation.detach computation inwards_canceler))
  |> Ivar.fill computation;
  Ivar.read computation
;;

let flat_map_l ?padded ~f seq k =
  let computation = Ivar.create () in
  seq (fun x ->
    let values = f x in
    let computation = Ivar.read computation in
    let latch = Latch.create ?padded 1 in
    List.iter values ~f:(fun y ->
      Latch.incr latch;
      fork computation (fun _ ->
        k y;
        Latch.decr latch));
    Latch.decr latch;
    Latch.await latch)
  |> Ivar.fill computation;
  Ivar.read computation
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
  let i = Atomic.make 0 in
  Computation.await (seq (fun x -> if f x then Atomic.incr i));
  Atomic.get i
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

(* TODO check which other creators may need latches like unfoldr *)

let[@specialise] rec unfoldr_aux yield_every computation latch f b k phase =
  match f b with
  | None ->
    Latch.decr latch;
    Latch.await latch;
    Computation.finish computation
  | Some (x, b) ->
    Latch.incr latch;
    fork computation (fun _ ->
      k x;
      Latch.decr latch);
    let phase = periodic_yield yield_every phase in
    unfoldr_aux yield_every computation latch f b k phase
;;

let unfoldr ?yield_every ?padded f b k =
  let computation = Computation.create () in
  let latch = Latch.create ?padded 1 in
  fork computation (fun _ -> unfoldr_aux yield_every computation latch f b k 1);
  computation
;;

let throttle ?padded size seq k =
  let module Semaphore = Picos_std_sync.Semaphore.Counting in
  let semaphore = Semaphore.make ?padded size in
  seq (fun x ->
    Semaphore.acquire semaphore;
    k x;
    Semaphore.release semaphore)
;;

exception Shortcut

let for_all ~f seq =
  let comp = seq (fun x -> if not (f x) then raise_notrace Shortcut) in
  Computation.wait comp;
  match Computation.canceled comp with
  | Some (Shortcut, _) -> false
  | None -> true
  | Some (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace
;;

let exists ~f seq =
  let comp = seq (fun x -> if f x then raise_notrace Shortcut) in
  Computation.wait comp;
  match Computation.canceled comp with
  | Some (Shortcut, _) -> true
  | None -> false
  | Some (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace
;;

exception All_taken

let take ?padded count seq k =
  if count <= 0
  then Computation.finished
  else (
    let latch = Latch.create ?padded count in
    let count = Atomic.make count in
    let new_comp = Computation.create () in
    let comp =
      seq (fun x ->
        let to_take = Atomic.fetch_and_add count ~-1 in
        if to_take > 0
        then
          fork new_comp (fun _ ->
            if Computation.is_running new_comp
            then (
              k x;
              Latch.decr latch));
        if to_take = 1 then raise_notrace All_taken)
    in
    let canceler = Computation.attach_canceler ~from:new_comp ~into:comp in
    fork new_comp (fun _ ->
      let () =
        match Computation.await comp with
        | () ->
          Computation.detach new_comp canceler;
          Core.Fn.apply_n_times ~n:(Atomic.get count) (fun () -> Latch.decr latch) ()
        | exception All_taken -> Computation.detach new_comp canceler
      in
      Latch.await latch;
      Computation.finish new_comp);
    new_comp)
;;

let take_while ?padded ~f seq k =
  let latch = Latch.create ?padded 1 in
  let new_comp = Computation.create () in
  let comp =
    seq (fun x ->
      if f x
      then (
        Latch.incr latch;
        fork new_comp (fun _ ->
          k x;
          Latch.decr latch))
      else raise_notrace All_taken)
  in
  let canceler = Computation.attach_canceler ~from:new_comp ~into:comp in
  fork new_comp (fun _ ->
    let () =
      match Computation.await comp with
      | () | (exception All_taken) -> Computation.detach new_comp canceler
    in
    Latch.decr latch;
    Latch.await latch;
    Computation.finish new_comp);
  new_comp
;;

exception Stream_closed

let stream_on ?(poison = false) ?callstack stream seq =
  Computation.await (seq (fun x -> Stream.push stream x));
  if poison then Stream.poison stream Stream_closed ?callstack
;;

let _iter_of_cursor cursor seq =
  (Iter.unfoldr [@inlined])
    (fun [@inline] cursor ->
       match Stream.read cursor with
       | read_result -> Some read_result
       | exception Stream_closed -> None)
    cursor
    seq
;;

let of_cursor ?yield_every cursor seq =
  (unfoldr [@inlined])
    ?yield_every
    (fun [@inline] cursor ->
       match Stream.read cursor with
       | read_result -> Some read_result
       | exception Stream_closed -> None)
    cursor
    seq
;;

let tap ~f seq k =
  seq (fun x ->
    f x;
    k x)
;;

let rec peek_iter cursor k =
  match Stream.peek_opt cursor with
  | None | (exception _) -> cursor
  | Some (el, cursor) ->
    k el;
    peek_iter cursor k
;;

(* TODO use in as many places as possible *)
let fork_new_comp ?padded ?(cleanup_hook = ignore) seq k =
  let new_comp = Computation.create () in
  let latch = Latch.create ?padded 1 in
  let old_comp =
    seq (fun x ->
      Latch.incr latch;
      fork new_comp (fun _ ->
        k x;
        Latch.decr latch))
  in
  let canceler = Computation.attach_canceler ~from:new_comp ~into:old_comp in
  fork new_comp (fun _ ->
    Computation.await old_comp;
    Computation.detach new_comp canceler;
    cleanup_hook ();
    Latch.decr latch;
    Latch.await latch;
    Computation.finish new_comp);
  new_comp
;;

let batch_map ?padded ~f seq k =
  let stream = Stream.create ?padded () in
  let cursor = Stream.tap stream in
  let new_comp =
    fork_new_comp
      ?padded
      ~cleanup_hook:(fun () -> Stream.poison stream Stream_closed)
      (filter_map seq ~f:(fun x ->
         let computation = Computation.create () in
         Stream.push stream (x, computation);
         match Computation.await computation with
         | y -> Some y
         | exception _ -> None))
      k
  in
  fork new_comp (fun _ ->
    let vector = Dynarray.create () in
    let rec loop cursor =
      match Stream.read cursor with
      | exception Stream_closed -> ()
      | el, cursor ->
        Dynarray.add_last vector el;
        let cursor = peek_iter cursor (Dynarray.add_last vector) in
        let array = Dynarray.to_array vector in
        Dynarray.clear vector;
        f array;
        loop cursor
    in
    loop cursor);
  new_comp
;;

let batch_iter ?padded ~f seq =
  let stream = Stream.create ?padded () in
  let cursor = Stream.tap stream in
  (* TODO is this the best way? *)
  stream_on ~poison:true stream seq;
  let vector = Dynarray.create () in
  let rec loop cursor =
    match Stream.read cursor with
    | exception Stream_closed -> ()
    | el, cursor ->
      Dynarray.add_last vector el;
      let cursor = peek_iter cursor (Dynarray.add_last vector) in
      let array = Dynarray.to_array vector in
      Dynarray.clear vector;
      f array;
      loop cursor
  in
  loop cursor
;;

let batch_fold ?padded ~f ~init seq =
  let stream = Stream.create ?padded () in
  let cursor = Stream.tap stream in
  stream_on ~poison:true stream seq;
  let vector = Dynarray.create () in
  let rec loop acc cursor =
    match Stream.read cursor with
    | exception Stream_closed -> acc
    | el, cursor ->
      Dynarray.add_last vector el;
      let cursor = peek_iter cursor (Dynarray.add_last vector) in
      let array = Dynarray.to_array vector in
      Dynarray.clear vector;
      let acc = f acc array in
      loop acc cursor
  in
  loop init cursor
;;

let sum ?(padded = false) seq =
  let sum = if padded then Atomic.make_contended 0 else Atomic.make 0 in
  iter seq ~f:(fun n -> if n <> 0 then Atomic.fetch_and_add sum n |> ignore);
  Atomic.get sum
;;

let to_list ?padded iter =
  batch_fold ?padded iter ~init:[] ~f:(fun init array ->
    Core.Array.fold array ~init ~f:(fun list el -> el :: list))
;;

(* TODO benchmark versions of to set using fold+add, of_array+to_list+union_list, of_array+union *)

let find_pred_exn (type a) ~f seq =
  let exception Found of a in
  match iter ~f:(fun el -> if f el then raise_notrace (Found el)) seq with
  | exception Found el -> el
  | () -> raise Not_found
;;

let iter_into_iter_and_stream ?padded seq =
  let stream = Stream.create ?padded () in
  let seq k =
    Iter.map
      seq
      ~f:(fun el ->
        Stream.push stream el;
        el)
      k;
    Stream.poison stream Stream_closed
  in
  seq, stream
;;

module type Deriving_enum = sig
  type t [@@deriving enum]
end

let into_buckets (type t) ?padded (module T : Deriving_enum with type t = t) ~project seq =
  let result = Array.init T.(max - min + 1) (fun _ -> Dynarray.create ()) in
  let mutexes = Array.init T.(max - min + 1) (fun _ -> Mutex.create ?padded ()) in
  iter seq ~f:(fun el ->
    let index = T.to_enum (project el) - T.min in
    Mutex.protect mutexes.(index) (fun () -> Dynarray.add_last result.(index) el));
  result
;;

let sort (type t) ?padded ~compare =
  batch_fold ?padded ~init:[||] ~f:(fun acc unsorted ->
    Core.Array.sort unsorted ~compare;
    Core.Array.merge acc unsorted ~compare)
;;

(* TODO some kind of radix sort *)
