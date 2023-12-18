include Core.List

let count_drop_while =
  let rec aux ~f count = function
    | hd :: tl when f hd -> (aux [@tailcall]) ~f (count + 1) tl
    | l -> count, l
  in
  aux 0
;;

let[@tail_mod_cons] rec update_concat list el replacement ~equal =
  match list with
  | [] -> raise (Invalid_argument "Element not found in list")
  | el' :: tail when equal el el' -> replacement @ tail
  | hd :: tail -> hd :: (update_concat [@tailcall]) tail el replacement ~equal
;;
