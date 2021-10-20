open Base
open Printf

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
    | TupleExpr of expr list
    | BlockExpr of expr list;;

let rec string_of_val = function
    | Number n -> Float.to_string n
    | Boolean b -> Bool.to_string b
    | Tuple ls -> "(" ^ String.concat ~sep:", " (List.map ~f:string_of_val ls) ^ ")"
    | Unit -> "()"
    | Lambda _ -> "Lambda"

let rec string_of_expr = function
    | Atomic v -> string_of_val v
    | Ident s -> s
    | Binary (_ as b) -> sprintf "{lhs: %s, rhs: %s}" (string_of_expr b.lhs) (string_of_expr b.rhs)
    | Let (_ as l) -> sprintf "Let %s = %s" (string_of_pat l.assignee) (string_of_expr l.assigned_expr)
    | LambdaCall call -> sprintf "{Call: %s, args: %s}" call.callee (string_of_expr call.call_args)
    | TupleExpr ls -> sprintf "(%s)" (String.concat ~sep:", " (List.map ~f:string_of_expr ls))
    | IfExpr _ -> "IfExpr"
    | BlockExpr ls -> sprintf "{\n\t%s\n}" (String.concat ~sep:"\n\t" (List.map ~f:string_of_expr ls))

and string_of_pat = function
    | SinglePat s -> s
    | TuplePat ls -> "(" ^ (String.concat ~sep:", " (List.map ~f:string_of_pat ls)) ^ ")"

type state = (string, value, String.comparator_witness) Map.t
