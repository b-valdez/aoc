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

type wrap = Wrap : ('a Angstrom.t * ('a -> wrap Effect.t)) -> wrap [@@unboxed]

let parse_partial file parser eff =
  let open Angstrom_modified.Unbuffered in
  let buffer = Bigstring.create 0x1100 in
  let rec f : type a b. a state -> (a -> wrap Effect.t) -> _ -> _ =
    fun state eff ic buffer off len eof ->
    match state with
    | Done (consumed, v) ->
      let (Wrap (parser, eff)) = Effect.perform (eff v) in
      if (not eof) || len <> consumed
      then f (parse parser) eff ic buffer (off + consumed) (len - consumed) eof
    | Fail (consumed, l, s) ->
      failwiths
        ~here:[%here]
        "Failure during parsing"
        (Bigstring.sub_shared ~pos:(off + consumed) ~len:(len - consumed) buffer, l, s)
        [%sexp_of: Bigstring.t * string list * string]
    | Partial { committed = consumed; continue } ->
      (* Forking the io such that it will be ready when angstrom requests more is tempting, but I don't see a good hook to ensure buffer gets overwritten only after it has been copied without introducing an intermediate copy *)
      let unconsumed_len = len - consumed in
      if unconsumed_len = 0x1100 then failwith "Buffer full and no progress possible";
      let off =
        if off + len > 0x100
        then (
          (* TODO replace with unsafe_blit, once it has shown itself to be safe *)
          Bigstring.blit
            ~src:buffer
            ~dst:buffer
            ~src_pos:(off + consumed)
            ~dst_pos:0
            ~len:unconsumed_len;
          0)
        else off + consumed
      in
      let len =
        if eof
        then unconsumed_len
        else
          unconsumed_len
          + Stdlib.In_channel.input_bigarray
              ic
              buffer
              (off + unconsumed_len)
              (Int.min 0x1000 (0x1100 - unconsumed_len))
      in
      if len = unconsumed_len
      then f (continue buffer ~off ~len Complete) eff ic buffer off len true
      else f (continue buffer ~off ~len Incomplete) eff ic buffer off len false
  in
  In_channel.with_file file ~binary:true ~f:(fun ic ->
    f (parse parser) eff ic buffer 0 0 false)
;;

let parse_file_prefix (type a) file parser =
  let module Eff = struct
    type _ Effect.t += Yield : a -> wrap Effect.t
  end
  in
  let eff a = Eff.Yield a in
  let fiber = Effect.Shallow.fiber (parse_partial file parser) in
  let el, fiber =
    Effect.Shallow.continue_with
      fiber
      eff
      { retc = (fun _ -> invalid_arg "parse_file: Returned without a Yield")
      ; exnc = raise
      ; effc =
          (fun (type a' b) -> function
            | (Eff.Yield el : a' Effect.t) ->
              Some
                (fun (k : (a', _) Effect.Shallow.continuation) ->
                  el, (k : (wrap, _) Effect.Shallow.continuation))
            | _ -> None)
      }
  in
  el, fiber
;;

let parse_file file parser =
  let el, fiber = parse_file_prefix file parser in
  Effect.Shallow.continue_with
    fiber
    (Wrap (Angstrom.fail "Not eof", fun _ -> failwith "Not eof"))
    { retc = ignore; exnc = raise; effc = (fun _ -> None) };
  el
;;

let parse_file_into_iter (type a) file parser k_iter =
  let module Eff = struct
    type _ Effect.t += Yield : a -> wrap Effect.t
  end
  in
  let eff a = Eff.Yield a in
  Effect.Deep.(
    try_with
      (parse_partial file parser)
      eff
      { effc =
          (fun (type a') -> function
            | (Eff.Yield a : a' Effect.t) ->
              k_iter a;
              Some (fun (k : (a', unit) continuation) -> continue k (Wrap (parser, eff)))
            | _ -> None)
      })
;;

let parse_file_into_stream (type a) file parser =
  let stream = Moonpool_sync.Stream.create () in
  Moonpool.Runner.(run_async (get_current_runner () |> Option.value_exn)) (fun () ->
    let module Eff = struct
      type _ Effect.t += Yield : a -> wrap Effect.t
    end
    in
    let eff a = Eff.Yield a in
    let open Effect.Deep in
    match_with
      (parse_partial file parser)
      eff
      { retc = (fun () -> Moonpool_sync.Stream.poison stream Parallel_iter.Stream_closed)
      ; exnc = Moonpool_sync.Stream.poison stream
      ; effc =
          (fun (type a') -> function
            | (Eff.Yield a : a' Effect.t) ->
              Moonpool_sync.Stream.push stream a;
              Some (fun (k : (a', unit) continuation) -> continue k (Wrap (parser, eff)))
            | _ -> None)
      });
  stream
;;

let tap = Picos_std_sync.Stream.tap

let run f =
  Moonpool_fib.main (fun _ ->
    Moonpool.Ws_pool.(
      with_ () (fun pool ->
        let fut, resolve = Moonpool.Fut.make () in
        Moonpool.start_thread_on_some_domain
          (fun () ->
            let open Caml_threads in
            Thread.delay 10.;
            Moonpool.Fut.fulfill resolve (Error (Moonpool.Exn_bt.get Shutdown)))
          ()
        |> ignore;
        let work = Moonpool.spawn ~on:pool f in
        Moonpool_sync.Event.(select [ of_fut fut; of_fut work ]))))
;;

let fork_join_array array =
  let (_ : unit array) = Moonpool_forkjoin.all_array ~chunk_size:1 array in
  ()
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
include Picos_std_sync

let[@inline never] mxprint_s mx sexp ~expect =
  Mutex.lock mx;
  print_s sexp;
  expect ();
  Mutex.unlock mx
;;

let[@inline never] mxprintf ~expect mx =
  Mutex.lock mx;
  Printf.kfprintf
    (fun _ ->
      expect ();
      Mutex.unlock mx)
    stdout
;;

let xprint_mutex = Mutex.create ()
let xprintf = mxprintf xprint_mutex
let xprint_s = mxprint_s xprint_mutex

(* TODO error-handling, otherwise some iter functions won't work *)
let tee_iter seq =
  let open Moonpool in
  let k1, set_k1 = Fut.make () in
  let k2, set_k2 = Fut.make () in
  let run =
    Fut.spawn_on_current_runner (fun () ->
      let k1 = await k1 in
      let k2 = await k2 in
      seq (fun el ->
        k1 el;
        k2 el))
  in
  let fulfill set k =
    Fut.fulfill set (Ok k);
    await run
  in
  fulfill set_k1, fulfill set_k2
;;
