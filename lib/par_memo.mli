val fix
  :  'a Kcas_data.Hashtbl.hashed_type
  -> ?min_buckets:int
  -> ?max_buckets:int
  -> (('a -> 'b) -> 'a -> 'b)
  -> 'a
  -> 'b

val repeat_fix
  :  'a Kcas_data.Hashtbl.hashed_type
  -> ?min_buckets:int
  -> ?max_buckets:int
  -> first_step:('a -> 'intermediate_result)
  -> ((int -> 'a -> 'intermediate_result)
      -> int
      -> 'intermediate_result
      -> 'intermediate_result)
  -> int
  -> 'a
  -> 'intermediate_result
