type sexp = Sexplib.Sexp.t

let is_numeric str =
  try
    ignore (int_of_string str); 
    true
  with
  | Failure _ -> false

let is_bool str =
  match str with
  | "#f" | "#t" -> true
  | _ -> false

let is_bounded_by_quotes str =
  let len = String.length str in
  len >= 2 && str.[0] = '"' && str.[len - 1] = '"'

let trim_quotes str =
  if is_bounded_by_quotes str then
    String.sub str 1 (String.length str - 2)  (* Remove the first and last characters *)
  else
    str  (* Return the original string if not bounded by quotes *)

let is_dot_pair sexpr =
  match sexpr with
  | Sexplib.Sexp.List [_; Sexplib.Sexp.Atom "."; _] -> true
  | _ -> false

let is_empty lst =
  match lst with
  | [] -> true  (* The list is empty *)
  | _ -> false  (* The list is not empty *)

let rec parse sexpr =
  match sexpr with
  | Sexplib.Sexp.Atom str -> 
    if is_numeric str then 
      Ast.Numeric (int_of_string str)
    else if is_bounded_by_quotes str then
      Ast.String (trim_quotes str)
    else if is_bool str then
      Ast.Bool (str = "#t")
    else
      Ast.Symbol str
  | Sexplib.Sexp.List list -> 
    if is_dot_pair sexpr then
      let head = parse (List.nth list 0) in
      let tail = parse (List.nth list 2) in
      Ast.Pair (head, tail)
    else if is_empty list then
      Ast.Nil
    else
      Ast.List (List.map parse list)

let rec map_back sexpr =
  match sexpr with
  | Ast.Numeric int -> Sexplib.Sexp.Atom (string_of_int int)
  | Ast.String str -> Sexplib.Sexp.Atom ("\"" ^ str ^ "\"")
  | Ast.Symbol atom -> Sexplib.Sexp.Atom atom
  | Ast.List xs -> Sexplib.Sexp.List (List.map map_back xs)
  | Ast.Pair (head, tail) -> Sexplib.Sexp.List [map_back head; Sexplib.Sexp.Atom "." ;map_back tail]
  | Ast.Nil -> Sexplib.Sexp.List []
  | Ast.Bool t -> Sexplib.Sexp.Atom (if t then "#t" else "#f")
  | Ast.Closure (_, _, _, _) -> Sexplib.Sexp.Atom "<closure>"
