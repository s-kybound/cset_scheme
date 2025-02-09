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

let builtins = [
  "+", Ast.Builtin "+";
  "-", Ast.Builtin "-";
  "*", Ast.Builtin "*";
  "/", Ast.Builtin "/";
]

let decompose_state (cs, stash, venv, menv) =
  (cs, stash, venv, menv)

let rec decompose xs state =
  match xs with
  | [] -> state  (* No further decomposition needed *)
  | v :: vs ->
    let (cs, stash, venv, menv) = decompose_state state in
    match v with
    | Ast.Numeric _ | Ast.String _ | Ast.Bool _ -> decompose vs (cs, v :: stash, venv, menv)
    | Ast.Symbol "define" -> 
      (match xs with | (Ast.Symbol "define" :: Ast.Symbol x :: expr :: []) ->
      (Fragment expr :: Instruction (Asgn (Ast.Symbol x)) :: cs, stash, venv, menv)
      | _ -> raise (Failure "Invalid define expression"))
    | Ast.Symbol "set!" -> 
      (match xs with | (Ast.Symbol "set!" :: Ast.Symbol x :: expr :: []) ->
      (Fragment expr :: Instruction (Asgn (Ast.Symbol x)) :: cs, stash, venv, menv)
      | _ -> raise (Failure "Invalid set! expression"))
    | Ast.Symbol "if" -> 
      (match xs with | (Ast.Symbol "if" :: pred :: consq :: alt :: []) ->
      (Fragment pred :: Instruction (Branch (consq, alt)) :: cs, stash, venv, menv)
      | _ -> raise (Failure "Invalid if expression"))
    | Ast.Symbol "lambda" ->
      (match xs with | (Ast.Symbol "lambda" :: Ast.List params :: body :: []) -> 
      let closure = Ast.Closure (params, body, venv, menv) in
      (cs, closure::stash, venv, menv)
      | _ -> raise (Failure "Invalid lambda expression"))
    | Ast.Symbol "begin" -> 
      (match xs with | (Ast.Symbol "begin" :: exprs) ->
      let rec add_pops items =
        match items with
        | [] -> []
        | [last] -> [Fragment last]
        | hd :: tl -> Fragment hd :: Instruction Pop :: add_pops tl
      in
      (add_pops exprs @ cs, stash, venv, menv)
      | _ -> raise (Failure "Invalid begin expression"))
    | _ ->
      (match xs with
      | fn :: args ->
      let call_instr = Instruction (Call (List.length args)) in
      let new_control_items = List.map (fun x -> Fragment x) (fn :: args) in
      (new_control_items @ (call_instr :: cs), stash, venv, menv)
      | _ -> raise (Failure "Invalid function call"))

let split_list n lst =
  let rec aux i acc rest =
    if i = 0 then (List.rev acc, rest)
    else match rest with
      | [] -> raise (Failure "Not enough elements in list")
      | x :: xs -> aux (i - 1) (x :: acc) xs
  in aux n [] lst

let execute_builtin b vals =
  match b with
  | "+" -> 
    let rec sum acc lst =
      match lst with
      | [] -> acc
      | (Ast.Numeric n) :: rest -> sum (acc + n) rest
      | _ -> raise (Failure "Invalid argument to +")
    in Ast.Numeric (sum 0 vals)
  | "*" ->
    let rec prod acc lst =
      match lst with
      | [] -> acc
      | (Ast.Numeric n) :: rest -> prod (acc * n) rest
      | _ -> raise (Failure "Invalid argument to *")
    in Ast.Numeric (prod 1 vals)
  | "-" ->
    (* behaviour depends on the number of values*)
    (match vals with
    | [] -> raise (Failure "Invalid argument to -")
    | [Ast.Numeric n] -> Ast.Numeric (-n)
    | (Ast.Numeric n) :: rest ->
      let rec sub acc lst =
        match lst with
        | [] -> acc
        | (Ast.Numeric n) :: rest -> sub (acc - n) rest
        | _ -> raise (Failure "Invalid argument to -")
      in Ast.Numeric (sub n rest)
    | _ -> raise (Failure "Invalid argument to -"))
  | "/" ->
    (* behaviour depends on the number of values*)
    (match vals with
    | [] -> raise (Failure "Invalid argument to /")
    | [Ast.Numeric n] -> Ast.Numeric (1 / n)
    | (Ast.Numeric n) :: rest ->
      let rec div acc lst =
        match lst with
        | [] -> acc
        | (Ast.Numeric n) :: rest -> div (acc / n) rest
        | _ -> raise (Failure "Invalid argument to /")
      in Ast.Numeric (div n rest)
    | _ -> raise (Failure "Invalid argument to /"))
  | _ -> raise (Failure "Invalid builtin")

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
    let (vals, new_stash) = split_list n stash in
    let vals = List.rev vals in
    (match new_stash with
     | Ast.Closure (params, body, closure_venv, closure_menv) :: _ ->
      let new_venv = Env.new_env (List.map (fun x -> match x with | Ast.Symbol x -> x | _ -> "") params) vals closure_venv in
       (Fragment body :: cs, new_stash, new_venv, closure_menv)
     | Ast.Builtin b :: _ -> 
      (cs, (execute_builtin b vals) :: new_stash, venv, menv)
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
      | Ast.Symbol s -> (cs, (Env.lookup s venv builtins)::stash, venv, menv) 
      | Ast.List xs -> 
        let partial_new_state = (cs, stash, venv, menv) in
        decompose xs partial_new_state 
      | _ -> raise (Failure "cannot evaluate values of this type")

let rec run_all state =
  let new_state = run state in
  if state = new_state then state
  else run_all new_state

let eval a = 
  let (_, vs, _, _) = run_all ([Fragment a], [], Env.Global, Env.Global) in
  match vs with
  | (v :: _)-> v
  | _ -> raise (Failure "Invalid evaluation result")

