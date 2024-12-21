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

let repeat_fix hashed_type ?min_buckets ?max_buckets ~first_step f =
  let tbl = Hashtbl.create ~hashed_type ?min_buckets ?max_buckets () in
  let rec memo repetitions initial =
    if repetitions <= 0 then invalid_arg "repeat_fix: non-positive amount of repetitions";
    let tx ~xt =
      match Hashtbl.Xt.find_opt ~xt tbl initial with
      | Some sub_tbl ->
        let repetition, closest =
          (* TODO replace with binary search to minimize accesses *)
          Iter.(repetitions --^ 1)
          |> Iter.find_map ~f:(fun repetition ->
            Option.map
              (fun result -> repetition, result)
              (Hashtbl.Xt.find_opt ~xt sub_tbl repetition))
          |> Option.get
        in
        if repetition = repetitions
        then fun () -> Promise.await_exn closest
        else (
          let promise, resolver = Promise.create () in
          Hashtbl.Xt.replace ~xt sub_tbl repetitions promise;
          fun () ->
            match f memo (repetitions - repetition) (Promise.await_exn closest) with
            | value ->
              Promise.resolve_ok resolver value;
              value
            | exception exn ->
              Promise.resolve_error resolver exn;
              raise exn)
      | None ->
        let sub_tbl =
          Hashtbl.create ~hashed_type:(module Int) ?min_buckets ?max_buckets ()
        in
        let promise, resolve = Promise.create () in
        Hashtbl.Xt.replace ~xt tbl initial sub_tbl;
        Hashtbl.Xt.replace ~xt sub_tbl 1 promise;
        fun () ->
          (match first_step initial with
           | value ->
             Promise.resolve_ok resolve value;
             value
           | exception exn ->
             Promise.resolve_error resolve exn;
             raise exn)
    in
    Kcas.Xt.commit { tx } ()
  in
  memo
;;
