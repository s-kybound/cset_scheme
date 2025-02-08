type name = string

type value = Ast.sexpr

type binding = name * value

type env = 
| Global
| Local of env * binding list

