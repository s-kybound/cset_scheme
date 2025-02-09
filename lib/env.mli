type name = string

type 'a env

val env_of : unit -> 'a env

val lookup : name -> 'a env -> (name * 'a) list -> 'a 

val extend : name -> 'a -> 'a env -> 'a env

val new_env : name list -> 'a list -> 'a env -> 'a env