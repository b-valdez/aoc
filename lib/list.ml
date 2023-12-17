include Core.List

let count_drop_while =
  let rec aux ~f count = function
    | hd :: tl when f hd -> (aux [@tailcall]) ~f (count + 1) tl
    | l -> count, l
  in
  aux 0
;;
