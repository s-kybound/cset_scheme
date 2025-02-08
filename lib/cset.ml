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

let decompose_state (cs, stash, venv, menv) =
  (cs, stash, venv, menv)

let rec decompose xs state =
  match xs with
  | [] -> state  (* No further decomposition needed *)
  | v :: vs ->
    let (cs, stash, venv, menv) = decompose_state state in
    match v with
    | Ast.Numeric _ | Ast.String _ | Ast.Bool _ -> decompose vs (cs, v :: stash, venv, menv)
    | Ast.Symbol s -> decompose vs (cs, (Env.lookup s venv) :: stash, venv, menv)
    | Ast.List (Ast.Symbol "define" :: Ast.Symbol x :: expr :: []) ->
      (Fragment expr :: Instruction (Asgn (Ast.Symbol x)) :: cs, stash, venv, menv)
    | Ast.List (Ast.Symbol "set!" :: Ast.Symbol x :: expr :: []) ->
      (Fragment expr :: Instruction (Asgn (Ast.Symbol x)) :: cs, stash, venv, menv)
    | Ast.List (Ast.Symbol "if" :: pred :: consq :: alt :: []) ->
      (Fragment pred :: Instruction (Branch (consq, alt)) :: cs, stash, venv, menv)
    | Ast.List (Ast.Symbol "lambda" :: Ast.List params :: body :: []) -> 
      let closure = Ast.Closure (params, body, venv, menv) in
      (cs, closure::stash, venv, menv)
    | Ast.List (Ast.Symbol "begin" :: exprs) ->
      let rec add_pops items =
        match items with
        | [] -> []
        | [last] -> [Fragment last]
        | hd :: tl -> Fragment hd :: Instruction Pop :: add_pops tl
      in
      (add_pops exprs @ cs, stash, venv, menv)
     | Ast.List (fn :: args) ->
      let call_instr = Instruction (Call (List.length args)) in
      let new_control_items = List.map (fun x -> Fragment x) (fn :: args) in
      (new_control_items @ (call_instr :: cs), stash, venv, menv)
    | _ -> raise (Failure "Unknown expression structure")

let split_list n lst =
  let rec aux i acc rest =
    if i = 0 then (List.rev acc, rest)
    else match rest with
      | [] -> raise (Failure "Not enough elements in list")
      | x :: xs -> aux (i - 1) (x :: acc) xs
  in aux n [] lst

let exec_instruction instr (cs, stash, venv, menv) =
  match instr with
  | Asgn (Ast.Symbol x) ->
    (cs, stash, Env.extend x (List.hd stash) venv, menv)
  | Asgn _ -> raise (Failure "invalid assingment")
  | Branch (consq, alt) ->
    (match stash with
     | Ast.Bool false :: rest -> (Fragment alt :: cs, rest, venv, menv)
     | _ :: rest -> (Fragment consq :: cs, rest, venv, menv)
     | _ -> raise (Failure "failed to evaluate conditional"))
  | Call n ->
    (match stash with
     | Ast.Closure (params, body, closure_venv, closure_menv) :: rest ->
      let (vals, new_stash) = split_list n rest in  
      let new_venv = Env.new_env (List.map (fun x -> match x with | Ast.Symbol x -> x | _ -> "") params) vals closure_venv in
       (Fragment body :: cs, new_stash, new_venv, closure_menv)
     | _ -> raise (Failure "Invalid function call"))
  | Env (venv', menv') -> (cs, stash, venv', menv')
  | Pop -> (cs, List.tl stash, venv, menv)


(* takes a state and does a single step of running the state *)
let run state:state = 
  match state with 
  | ([], _, _, _) -> state (*do nothing*)
  | (c::cs, stash, venv, menv) ->
    match c with
    | Instruction i -> exec_instruction i (cs, stash, venv, menv)
    | Fragment s ->
      match s with
      | Ast.Numeric _| Ast.String _| Ast.Bool _-> (cs, s::stash, venv, menv)
      | Ast.Symbol s -> (cs, (Env.lookup s venv)::stash, venv, menv) 
      | Ast.List xs -> 
        let partial_new_state = (cs, stash, venv, menv) in
        decompose xs partial_new_state 
      | _ -> raise (Failure "cannot evaluate values of this type")

let eval a = a

