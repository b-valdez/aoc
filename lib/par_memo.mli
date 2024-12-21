val fix
  :  'a Kcas_data.Hashtbl.hashed_type
  -> ?min_buckets:int
  -> ?max_buckets:int
  -> (('a -> 'b) -> 'a -> 'b)
  -> 'a
  -> 'b

val repeat_fix
  :  (module Ppx_hash_lib.Hashable.S with type t = 'root)
  -> ?min_buckets:int
  -> ?max_buckets:int
  -> first_step:('root -> 'branch)
  -> ((int -> 'root -> 'branch) -> int -> 'branch -> 'branch)
  -> int
  -> 'root
  -> 'branch
