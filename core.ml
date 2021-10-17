type operator =
    | Add
    | Sub
    | Mul
    | Div;;

type pattern =
    | SinglePat of string
    | TuplePat of pattern list

type value =
    | Number of float
    | Tuple of value list
    | Unit

let rec string_of_val = function
    | Number n -> string_of_float n
    | Tuple ls -> String.concat ", " (List.map string_of_val ls)
    | Unit -> "()"

type expr =
    | Atomic of value
    | Ident of string
    | Binary of {lhs: expr; op: operator; rhs: expr}
    | Let of {assignee: pattern; assigned_expr: expr};;
