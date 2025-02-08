type stash = Ast.expr list

type val_env = Ast.expr Env.env

type macro_env = unit Env.env

type instr =
| Asgn of Ast.expr
| Branch of Ast.expr * Ast.expr
| Pop
| Call of int
| Env of val_env * macro_env (* for now its a value env *)

type control_item = 
| Instruction of instr
| Fragment of Ast.expr

type control = control_item list

type state = control * stash * val_env * macro_env

type sym = Ast.Symbol

let decompose xs pns =
  match xs with
  | []

(* takes a state and does a single step of running the state *)
let run state:state = 
  match state with 
  | ([], _, _, _) -> state (*do nothing*)
  | (c::cs, stash, venv, menv) ->
    match c with
    | Instruction i -> state
    | Fragment s ->
      match s with
      | Ast.Numeric _| Ast.String _| Ast.Bool _-> (cs, s::stash, venv, menv)
      | Ast.Symbol s -> (cs, (Env.lookup s venv)::stash, venv, menv) 
      | Ast.List xs -> 
        let partial_new_state = (cs, stash, venv, menv) in
        decompose xs partial_new_state 
      | _ -> raise (Failure "cannot evaluate values of this type")
(*let rec cse_eval =



*)
let eval a = a

