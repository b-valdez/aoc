open! Core
include Moonpool.Fut

let equal (type value) equal_value a b =
  match peek a, peek b with
  | Some a, Some b -> [%equal: (value, _) result] a b
  | None, None -> phys_equal a b
  | _ -> false
;;

let sexp_of_t sexp_of_value a =
  match peek a with
  | Some a -> [%sexp_of: (value, _) result] a
  | None -> Sexp.Atom "Unresolved"
;;
