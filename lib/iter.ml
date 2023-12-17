include IterLabels

let of_map_iteri map_iteri (consume : _ -> unit) : unit =
  map_iteri ~f:(fun ~key ~data -> consume (key, data))
;;

type 'a gen =
  { next : 'b. 'b -> 'a
  ; free : unit -> unit
  }

let to_gen (type a) (iter : a t) : a gen =
  let module M = struct
    type _ Effect.t += Yield : a -> unit Effect.t
  end
  in
  let yield v = Effect.perform (M.Yield v) in
  let free = ref @@ fun () -> () in
  let rec next =
    ref
    @@ fun () ->
    Effect.Deep.match_with
      iter
      yield
      { retc =
          (fun () -> raise_notrace @@ Core.Not_found_s [%message "No further values"])
      ; exnc = raise_notrace
      ; effc =
          (fun (type b) : (b Effect.t -> _) -> function
            | M.Yield (v : a) ->
              Some
                (fun (k : (b, _) Effect.Deep.continuation) ->
                  (free
                   := fun () ->
                        match Effect.Deep.discontinue k Exit with
                        | _ | (exception Exit) -> ());
                  (next := fun () -> Effect.Deep.continue k ());
                  v)
            | _ -> None)
      }
  in
  { next = (fun _ -> !next ()); free = (fun () -> !free ()) }
;;
