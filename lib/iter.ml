include IterLabels

let from_labelled_iter2 iter2 f = iter2 ~f:(fun i a -> f (i, a))

let of_map_iteri map_iteri (consume : _ -> unit) : unit =
  map_iteri ~f:(fun ~key ~data -> consume (key, data))
;;

type 'a gen =
  { mutable next : unit -> 'a
  ; mutable free : unit -> unit
  }

let to_gen (type a) (iter : a t) : a gen =
  let open struct
    type _ Effect.t += Yield : a -> unit Effect.t
  end in
  let yield v = Effect.perform (Yield v) in
  let rec gen =
    let next _ =
      Effect.Deep.match_with
        iter
        yield
        { retc =
            (fun () -> raise_notrace @@ Core.Not_found_s [%message "No further values"])
        ; exnc = raise_notrace
        ; effc =
            (fun (type b) : (b Effect.t -> _) -> function
               | Yield (v : a) ->
                 Some
                   (fun (k : (b, _) Effect.Deep.continuation) ->
                     gen.free
                     <- (fun () ->
                          match Effect.Deep.discontinue k Exit with
                          | _ | (exception Exit) -> ());
                     gen.next <- (fun _ -> Effect.Deep.continue k ());
                     v)
               | _ -> None)
        }
    in
    { next; free = (fun () -> ()) }
  in
  gen
;;

type 'a functional_gen =
  { next : unit -> 'a functional_gen
  ; free : unit -> 'a functional_gen
  ; has_next : bool
  ; latest : ('a, exn) result
  }

let to_functional_gen (type a) (iter : a t) : a functional_gen =
  let open struct
    type _ Effect.t += Yield : a -> unit Effect.t
  end in
  let yield v = Effect.perform (Yield v) in
  let no_next_gen latest =
    let rec ret =
      { next = (fun () -> ret); free = (fun () -> ret); has_next = false; latest }
    in
    ret
  in
  let next () =
    Effect.Deep.match_with
      iter
      yield
      { retc =
          (fun () ->
            let exn = Core.Not_found_s [%message "No further values"] in
            no_next_gen (Error exn))
      ; exnc = (fun exn -> no_next_gen (Error exn))
      ; effc =
          (fun (type b) : (b Effect.t -> _) -> function
             | Yield (v : a) ->
               Some
                 (fun (k : (b, _) Effect.Deep.continuation) ->
                   { next = (fun () -> Effect.Deep.continue k ())
                   ; free =
                       (fun () ->
                         match Effect.Deep.discontinue k Exit with
                         | _ | (exception Exit) -> no_next_gen (Error Exit))
                   ; has_next = true
                   ; latest = Ok v
                   })
             | _ -> None)
      }
  in
  { next
  ; free = Fun.const @@ no_next_gen (Error Exit)
  ; has_next = true
  ; latest = Error (Invalid_argument "latest before first next")
  }
;;

module Let_syntax = struct
  let return = return
  let ( >>= ) = ( >>= )
  let ( >>| ) = ( >|= )

  module Let_syntax = struct
    let return = return
    let bind t ~f = flat_map ~f t
    let map t ~f = map ~f t
    let both = product

    module Open_on_rhs = Infix
  end
end

let tap iter ~f g =
  iter (fun x ->
    f x;
    g x)
;;

let to_set elt = fold ~init:(Core.Set.empty elt) ~f:Core.Set.add
