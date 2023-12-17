include IterLabels

let of_map_iteri map_iteri (consume : _ -> unit) : unit =
  map_iteri ~f:(fun ~key ~data -> consume (key, data))
;;
