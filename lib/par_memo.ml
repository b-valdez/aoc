open! Core
open Kcas_data

let fix hashed_type ?min_buckets ?max_buckets f =
  let tbl = Hashtbl.create ~hashed_type ?min_buckets ?max_buckets () in
  let rec memo arg =
    let tx ~xt =
      match Hashtbl.Xt.find_opt ~xt tbl arg with
      | Some promise -> fun () -> Promise.await_exn promise
      | None ->
        let promise, resolver = Promise.create () in
        Hashtbl.Xt.replace ~xt tbl arg promise;
        fun () ->
          (match f memo arg with
           | value ->
             Promise.resolve_ok resolver value;
             value
           | exception exn ->
             Promise.resolve_error resolver exn;
             raise exn)
    in
    Kcas.Xt.commit { tx } ()
  in
  memo
;;

let repeat_fix
      (type root)
      (module T : Ppx_hash_lib.Hashable.S with type t = root)
      ?min_buckets
      ?max_buckets
      ~first_step
      f
  =
  let tbl =
    Hashtbl.create
      ~hashed_type:
        (module struct
          type t = T.t * int [@@deriving hash]

          let equal (t1, i1) (t2, i2) = i1 = i2 && T.hash t1 = T.hash t2
        end)
      ?min_buckets
      ?max_buckets
      ()
  in
  let rec memo repetitions root =
    if repetitions <= 0 then invalid_arg "repeat_fix: non-positive amount of repetitions";
    if repetitions = 1
    then first_step root
    else (
      let tx ~xt =
        match Hashtbl.Xt.find_opt ~xt tbl (root, repetitions) with
        | Some promise -> fun () -> Promise.await_exn promise
        | None ->
          let promise, resolve = Promise.create () in
          Hashtbl.Xt.replace ~xt tbl (root, repetitions) promise;
          fun () ->
            (* biasing recursive memo calls towards powers of 2 for more likely reuses *)
            let lesser_pow_2 = Int.floor_pow2 (repetitions - 1) in
            (match f memo (repetitions - lesser_pow_2) (memo lesser_pow_2 root) with
             | value ->
               Promise.resolve_ok resolve value;
               value
             | exception exn ->
               Promise.resolve_error resolve exn;
               raise exn)
      in
      Kcas.Xt.commit { tx } ())
  in
  memo
;;
