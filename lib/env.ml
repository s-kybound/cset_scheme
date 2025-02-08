type name = string

type 'a binding = name * 'a

type 'a env = 
| Global
| Local of 'a env * 'a binding list

let rec lookup str env =
match env with
| Global -> raise (Failure "value does not exist in env!")
| Local (outer_env, bindings) ->
  try
    let _, v = List.find (fun (n, _) -> n = str) bindings in 
    v
  with Not_found -> lookup str outer_env

let extend_env (name: name) (value: 'a) (env: 'a env) : 'a env =
  Local (env, [(name, value)])