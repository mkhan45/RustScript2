type operator =
    | Add
    | Sub
    | Mul
    | Div
    | LT
    | GT
    | EQ;;

type pattern =
    | SinglePat of string
    | TuplePat of pattern list

type lambda = {lambda_expr: expr; lambda_args: pattern}
and lambda_call = {callee: string; call_args: expr}

and if_expr = {cond: expr; then_expr: expr; else_expr: expr}

and value =
    | Number of float
    | Boolean of bool
    | Tuple of value list
    | Unit
    | Lambda of lambda

and expr =
    | Atomic of value
    | Ident of string
    | Binary of {lhs: expr; op: operator; rhs: expr}
    | Let of {assignee: pattern; assigned_expr: expr}
    | LambdaCall of lambda_call
    | IfExpr of  if_expr
    | TupleExpr of expr list;;

let rec string_of_val = function
    | Number n -> string_of_float n
    | Boolean b -> string_of_bool b
    | Tuple ls -> "(" ^ Base.String.concat ~sep:", " (List.map string_of_val ls) ^ ")"
    | Unit -> "()"
    | Lambda _ -> "Lambda"

let rec string_of_expr = function
    | Atomic v -> string_of_val v
    | Ident s -> s
    | Binary (_ as b) -> "{lhs: " ^ (string_of_expr b.lhs) ^ ", rhs: " ^ (string_of_expr b.rhs) ^ "}"
    | Let {assignee = a; assigned_expr = e} -> "Let " ^ (string_of_pat a) ^ " = " ^ (string_of_expr e)
    | LambdaCall _ -> "LambdaCall"
    | TupleExpr ls -> "(" ^ (Base.String.concat ~sep:", " (List.map string_of_expr ls)) ^ ")"
    | IfExpr _ -> "IfExpr"

and string_of_pat = function
    | SinglePat s -> s
    | TuplePat ls -> "(" ^ (Base.String.concat ~sep:", " (List.map string_of_pat ls)) ^ ")"
