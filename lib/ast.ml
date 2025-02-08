(* The abstract syntax tree of Scheme *)

(*
type scheme_number = 
| Int of int
| Rat of int * int
| Real of float
| Complex of scheme_number * scheme_number
*)

type params = string

type expr =
| Numeric of int (* for now *)
| String of string
| Symbol of string
| Bool of bool
(* main priorities *)
| Pair of expr * expr
| List of expr list
| Closure of params * expr * expr Env.env * unit Env.env
| Nil