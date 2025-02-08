(* The abstract syntax tree of Scheme *)

type scheme_number = 
| Int of int
| Rat of int * int
| Real of float
| Complex of scheme_number * scheme_number

type sexpr =
| Numeric of scheme_number
| String of string
| Symbol of string
| Pair of sexpr * sexpr
| Nil