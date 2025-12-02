(** {2 Iter} *)

module Iter = Iter

(** {2 Core} *)

include (Core : module type of Core with module List := Core.List) (** @inline *)

module List = List

(** {2 Core_kernel Composition_Infix} *)

include Composition_infix (** @inline *)

(** {2 Angstrom} *)

module Angstrom = Angstrom_modified

(** {2 Multicore_magic}*)

module Atomic = Multicore_magic.Transparent_atomic

(** {2 Moonpool}*)

module Fut = Fut

(** {2 Added for AOC} *)

type part =
  | Part1
  | Part2

module Parallel_iter = Parallel_iter
module Grid = Grid
module Par_memo = Par_memo

let parse_string parser =
  Angstrom.parse_string ~consume:All parser >> Result.ok_or_failwith
;;

type wrap = Wrap : ('a Angstrom.t * ('a -> wrap Effect.t)) -> wrap [@@unboxed]

let parse_partial file parser eff =
  let open Angstrom_modified.Unbuffered in
  let buffer = Bigstring.create 0x1100 in
  let rec f : type a. a state -> (a -> wrap Effect.t) -> _ -> _ =
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
  In_channel.with_file file ~binary:false ~f:(fun ic ->
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
          (fun (type a') -> function
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

let continue_parsing_into_iter (type a) fiber parser k_iter =
  let module Eff = struct
    type _ Effect.t += Yield : a -> wrap Effect.t
  end
  in
  let eff a = Eff.Yield a in
  Effect.Deep.(
    try_with
      (Effect.Shallow.continue_with fiber (Wrap (parser, eff)))
      { retc = Fun.id
      ; exnc = raise_notrace
      ; effc =
          (function
            | _ -> None)
      }
      { effc =
          (fun (type a') -> function
             | (Eff.Yield a : a' Effect.t) ->
               k_iter a;
               Some (fun (k : (a', unit) continuation) -> continue k (Wrap (parser, eff)))
             | _ -> None)
      })
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
               Some
                 (fun (k : (a', unit) continuation) ->
                   match k_iter a with
                   | () -> continue k (Wrap (parser, eff))
                   | exception exn -> discontinue k exn)
             | _ -> None)
      })
;;

let continue_parsing_into_stream (type a) fiber parser =
  let stream = Picos_std_sync.Stream.create () in
  Moonpool.Runner.(run_async (get_current_runner () |> Option.value_exn)) (fun () ->
    let module Eff = struct
      type _ Effect.t += Yield : a -> wrap Effect.t
    end
    in
    let eff a = Eff.Yield a in
    Effect.Deep.match_with
      (Effect.Shallow.continue_with fiber (Wrap (parser, eff)))
      { retc = Fun.id
      ; exnc = raise_notrace
      ; effc =
          (function
            | _ -> None)
      }
      { effc =
          (fun (type a') -> function
             | (Eff.Yield a : a' Effect.t) ->
               Picos_std_sync.Stream.push stream a;
               Some
                 (fun (k : (a', _) Effect.Deep.continuation) ->
                   Effect.Deep.continue k (Wrap (parser, eff)))
             | _ -> None)
      ; retc = (fun () -> Picos_std_sync.Stream.poison stream Parallel_iter.Stream_closed)
      ; exnc = Picos_std_sync.Stream.poison stream
      });
  stream
;;

let parse_file_into_stream (type a) file parser =
  let stream = Picos_std_sync.Stream.create () in
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
      { retc = (fun () -> Picos_std_sync.Stream.poison stream Parallel_iter.Stream_closed)
      ; exnc = Picos_std_sync.Stream.poison stream
      ; effc =
          (fun (type a') -> function
             | (Eff.Yield a : a' Effect.t) ->
               Picos_std_sync.Stream.push stream a;
               Some (fun (k : (a', unit) continuation) -> continue k (Wrap (parser, eff)))
             | _ -> None)
      });
  stream
;;

let tap = Picos_std_sync.Stream.tap
let is_shutdown = ref true

let run ?(timeout = 10.) f =
  Moonpool.Main.main (fun _ ->
    Moonpool.Ws_pool.(
      with_ () (fun pool ->
        is_shutdown := false;
        let fut, resolve = Moonpool.Fut.make () in
        Moonpool.start_thread_on_some_domain
          (fun () ->
             let open Caml_threads in
             Thread.delay timeout;
             is_shutdown := true;
             Moonpool.Fut.fulfill_idempotent
               resolve
               (Error (Moonpool.Exn_bt.get Shutdown)))
          ()
        |> ignore;
        Moonpool.Fut.on_result
          (Moonpool.spawn ~on:pool f)
          (Moonpool.Fut.fulfill_idempotent resolve);
        Moonpool.await fut)))
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

let tee_iter seq ~n =
  let open Moonpool in
  let k_array = Array.init n ~f:(fun _ -> Fut.make (), Fut.make ()) in
  let _ =
    Fut.spawn_on_current_runner (fun () ->
      await @@ Fut.wait_array (Array.map k_array ~f:(fun ((k, _), _) -> k));
      Iter.fold
        seq
        ~init:
          (Array.map k_array ~f:(fun ((k, _), (_, return)) ->
             Fut.get_or_fail_exn k, return))
        ~f:(fun k_array el ->
          if Array.is_empty k_array then raise_notrace Shutdown;
          Array.filter k_array ~f:(fun (k, return) ->
            match k el with
            | () -> true
            | exception exn ->
              Fut.fulfill return (Error (Exn_bt.get exn));
              false))
      |> Array.iter ~f:(fun (_, return) -> Fut.fulfill return (Ok ())))
  in
  Array.map k_array ~f:(fun ((_, set_k), (result, _)) k ->
    Fut.fulfill set_k (Ok k);
    await result)
;;

let tee_parallel_iter seq ~n =
  let open Moonpool in
  let k_array =
    Array.init n ~f:(fun _ -> Fut.make (), Picos.Computation.create ~mode:`LIFO ())
  in
  let _ =
    Fut.spawn_on_current_runner (fun () ->
      await @@ Fut.wait_array (Array.map k_array ~f:(fun ((k, _), _) -> k));
      Parallel_iter.fold
        seq
        ~init:
          (Array.map k_array ~f:(fun ((k, _), return) -> Fut.get_or_fail_exn k, return))
        ~f:(fun k_array el ->
          if Array.is_empty k_array then raise_notrace Shutdown;
          Array.filter k_array ~f:(fun (k, return) ->
            match k el with
            | () -> true
            | exception exn ->
              Picos.Computation.cancel return exn (Stdlib.Printexc.get_callstack 0);
              false))
      |> Array.iter ~f:(fun (_, return) -> Picos.Computation.finish return))
  in
  Array.map k_array ~f:(fun ((_, set_k), result) k ->
    Fut.fulfill set_k (Ok k);
    result)
;;
