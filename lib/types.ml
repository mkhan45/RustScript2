open Base
open Printf
open Stdio

type operator =
    | Add
    | Neg
    | Mul
    | Div
    | LT
    | GT
    | LEQ
    | GEQ
    | EQ
    | NEQ
    | And
    | Or
    | Mod
    | Head
    | Tail
    | Not

type value =
    | Number of float
    | Boolean of bool
    | Tuple of value list
    | ValList of value list
    | Lambda of lambda
    | Thunk of {thunk_fn: lambda; thunk_args: value; thunk_fn_name: string}
    | Dictionary of (int, (value * value) list, Int.comparator_witness) Map.t
    | Atom of int

and pattern =
    | SinglePat of string
    | NumberPat of float
    | UnresolvedAtomPat of string
    | AtomPat of int
    | TuplePat of pattern list
    | ListPat of list_pattern
    | MapPat of (expr * pattern) list
    | OrPat of pattern * pattern
    | AsPat of pattern * string
    | WildcardPat

and list_pattern =
    | FullPat of pattern list
    | HeadTailPat of (pattern list) * pattern

and static_state = { static_atoms: (string * int) list }
and state = (string, value, String.comparator_witness) Map.t

and lambda = {lambda_expr: expr; lambda_args: pattern; enclosed_state: state}
and lambda_call = {callee: string; call_args: expr}
and if_expr = {cond: expr; then_expr: expr; else_expr: expr}

and expr =
    | Atomic of value
    | Ident of string
    | Binary of {lhs: expr; op: operator; rhs: expr}
    | Prefix of {op: operator; rhs: expr}
    | Let of {assignee: pattern; assigned_expr: expr}
    | LambdaDef of {lambda_def_expr: expr; lambda_def_args: pattern}
    | LambdaCall of lambda_call
    | IfExpr of if_expr
    | TupleExpr of expr list
    | BlockExpr of expr list
    | MatchExpr of {match_val: expr; match_arms: (pattern * expr * expr option) list}
    | MapExpr of ((expr * expr) list) * (expr option)
    | ListExpr of (expr list) * (expr option)
    | UnresolvedAtom of string

let rec string_of_val ss v = 
    let string_of_val = string_of_val ss in
    match v with
    | Number n -> Float.to_string n
    | Boolean b -> Bool.to_string b
    | Tuple ls -> "(" ^ String.concat ~sep:", " (List.map ~f:string_of_val ls) ^ ")"
    | ValList ls -> "[" ^ String.concat ~sep:", " (List.map ~f:string_of_val ls) ^ "]"
    | Lambda _ -> "Lambda"
    | Thunk _ -> "Thunk"
    | Dictionary d ->
        let string_of_pair = fun (k, v) -> sprintf "%s: %s" (string_of_val k) (string_of_val v) in
        let map_fn = fun ls -> String.concat ~sep:", " (List.map ~f:string_of_pair ls) in
        let map_pairs = (List.map ~f:map_fn (Map.data d)) in
        sprintf "{%s}" (String.concat ~sep:", " map_pairs)
    | Atom n -> 
        let reverse_map = List.Assoc.inverse ss.static_atoms in
        sprintf ":%s" (List.Assoc.find_exn reverse_map ~equal:Int.equal n)

let rec string_of_expr ss e = 
    let string_of_expr = string_of_expr ss in
    let string_of_val  = string_of_val ss in
    match e with
    | Atomic v -> string_of_val v
    | Ident s -> s
    | Prefix (_ as p) -> sprintf "{rhs: %s}" (string_of_expr p.rhs)
    | Binary (_ as b) -> sprintf "{lhs: %s, rhs: %s}" (string_of_expr b.lhs) (string_of_expr b.rhs)
    | Let (_ as l) -> sprintf "Let %s = %s" (string_of_pat l.assignee) (string_of_expr l.assigned_expr)
    | LambdaDef _ -> "Lambda"
    | LambdaCall call -> sprintf "{Call: %s, args: %s}" call.callee (string_of_expr call.call_args)
    | TupleExpr ls -> sprintf "(%s)" (String.concat ~sep:", " (List.map ~f:string_of_expr ls))
    | ListExpr (ls, tail) -> 
        sprintf "[%s|%s]"
        (String.concat ~sep:", " (List.map ~f:string_of_expr ls))
        (if Option.is_none tail then "None" else "Tail")
    | IfExpr _ -> "IfExpr"
    | BlockExpr ls -> sprintf "{\n\t%s\n}" (String.concat ~sep:"\n\t" (List.map ~f:string_of_expr ls))
    | MatchExpr _ -> "MatchExpr"
    | MapExpr _ -> "Map"
    | UnresolvedAtom _ -> "UnresolvedAtom"

and string_of_list_pat = function
    | FullPat ls -> "[" ^ (String.concat ~sep:", " (List.map ~f:string_of_pat ls)) ^ "]"
    | HeadTailPat (_hd, _tl) -> assert false (* TODO *)

and string_of_pat = function
    | SinglePat s -> s
    | ListPat lp -> (string_of_list_pat lp)
    | MapPat _ -> "MapPat"
    | NumberPat f -> Float.to_string f
    | TuplePat ls -> sprintf "(%s)" (String.concat ~sep:", " (List.map ~f:string_of_pat ls))
    | WildcardPat -> "_"
    | OrPat _ -> "OrPat"
    | AsPat _ -> "AsPat"
    | UnresolvedAtomPat _ -> "UnresolvedAtomPat"
    | AtomPat _ -> "AtomPat"

let rec hash_value = function
    | Number f -> Hashtbl.hash (0, f)
    | Boolean b -> Hashtbl.hash (1, b)
    | Tuple ls -> Hashtbl.hash (2, List.map ~f:hash_value ls)
    | Atom i -> Hashtbl.hash (3, i)
    | _ ->
        printf "Tried to hash an unhashable type";
        assert false
