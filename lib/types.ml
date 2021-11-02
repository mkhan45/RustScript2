open Base
open Printf
open Stdio

type location = { line_num: int }
let location_to_string location = sprintf "line %s" (Int.to_string location.line_num)

module Located = struct
    type 'a t = { location: location; data: 'a }

    let locate location a = { location; data = a }
    let extract {data; _} = data
end

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
    | Fn of func
    | Thunk of {thunk_fn: lambda; thunk_args: value; thunk_fn_name: string}
    | Dictionary of (int, (value * value) list, Int.comparator_witness) Map.t
    | Atom of int
    | StringVal of string

and pattern =
    | SinglePat of string
    | NumberPat of float
    | StringPat of string
    | UnresolvedAtomPat of string
    | AtomPat of int
    | TuplePat of pattern list
    | ListPat of list_pattern
    | MapPat of ((expr Located.t) * pattern) list
    | OrPat of pattern * pattern
    | AsPat of pattern * string
    | WildcardPat

and list_pattern =
    | FullPat of pattern list
    | HeadTailPat of (pattern list) * pattern

(* TODO: Make static_block_funcs a map *)
and static_state = { static_atoms: (string * int) list; static_block_funcs: (string * func) list }
and state = (string, value, String.comparator_witness) Map.t

and lambda = {lambda_expr: expr Located.t; lambda_args: pattern; enclosed_state: state}
and lambda_call = {callee: string; call_args: expr Located.t}
and func = {fn_expr: expr Located.t; fn_args: pattern}
and if_expr = {cond: expr Located.t; then_expr: expr Located.t; else_expr: expr Located.t}

and expr =
    | Atomic of value
    | IdentExpr of string
    | Binary of {lhs: expr Located.t; op: operator; rhs: expr Located.t}
    | Prefix of {op: operator; rhs: expr Located.t}
    | Let of {assignee: pattern; assigned_expr: expr Located.t}
    | LambdaDef of {lambda_def_expr: expr Located.t; lambda_def_args: pattern}
    | LambdaCall of lambda_call
    | FnDef of {fn_name: string; fn_def_func: func}
    | IfExpr of if_expr
    | TupleExpr of (expr Located.t) list
    | BlockExpr of (expr Located.t) list
    | MatchExpr of {match_val: expr Located.t; match_arms: (pattern * (expr Located.t) * (expr Located.t) option) list}
    | MapExpr of (((expr Located.t) * (expr Located.t)) list) * ((expr Located.t) option)
    | ListExpr of ((expr Located.t) list) * ((expr Located.t) option)
    | UnresolvedAtom of string

let escape_string s =
    s 
    |> String.substr_replace_all ~pattern:"\\n" ~with_:"\n"
    |> String.substr_replace_all ~pattern:"\\t" ~with_:"\t"

let rec string_of_val ss v = 
    let string_of_val = string_of_val ss in
    match v with
    | Number n -> Float.to_string n
    | Boolean true -> "T"
    | Boolean false -> "F"
    | Tuple ls -> "(" ^ String.concat ~sep:", " (List.map ~f:string_of_val ls) ^ ")"
    | ValList ls -> "[" ^ String.concat ~sep:", " (List.map ~f:string_of_val ls) ^ "]"
    | Lambda _ -> "Lambda"
    | Fn _ -> "Fn"
    | Thunk _ -> "Thunk"
    | Dictionary d ->
        let string_of_pair = fun (k, v) -> sprintf "%s: %s" (string_of_val k) (string_of_val v) in
        let map_fn = fun ls -> String.concat ~sep:", " (List.map ~f:string_of_pair ls) in
        let map_pairs = (List.map ~f:map_fn (Map.data d)) in
        sprintf "{%s}" (String.concat ~sep:", " map_pairs)
    | Atom n -> 
        let reverse_map = List.Assoc.inverse ss.static_atoms in
        sprintf ":%s" (List.Assoc.find_exn reverse_map ~equal:Int.equal n)
    | StringVal s -> s |> escape_string |> sprintf "\"%s\"" 

let rec string_of_expr ss e = 
    let string_of_expr = string_of_expr ss in
    let string_of_val  = string_of_val ss in
    match e with
    | Atomic v -> string_of_val v
    | IdentExpr s -> s
    | Prefix (_ as p) -> sprintf "{rhs: %s}" (string_of_expr p.rhs.data)
    | Binary (_ as b) -> sprintf "{lhs: %s, rhs: %s}" (string_of_expr b.lhs.data) (string_of_expr b.rhs.data)
    | Let (_ as l) -> sprintf "Let %s = %s" (string_of_pat l.assignee) (string_of_expr l.assigned_expr.data)
    | LambdaDef _ -> "Lambda"
    | FnDef _ -> "FnDef"
    | LambdaCall call -> sprintf "{Call: %s, args: %s}" call.callee (string_of_expr call.call_args.data)
    | TupleExpr ls -> 
            sprintf "(%s)" (String.concat ~sep:", " (ls |> List.map ~f:Located.extract |> List.map ~f:string_of_expr))
    | ListExpr (ls, tail) -> 
        sprintf "[%s|%s]"
        (String.concat ~sep:", " (ls |> List.map ~f:Located.extract |> List.map ~f:string_of_expr))
        (if Option.is_none tail then "None" else "Tail")
    | IfExpr _ -> "IfExpr"
    | BlockExpr ls -> 
        sprintf "{\n\t%s\n}" 
        (String.concat ~sep:"\n\t" (ls |> List.map ~f:Located.extract |> List.map ~f:string_of_expr))
    | MatchExpr _ -> "MatchExpr"
    | MapExpr _ -> "Map"
    | UnresolvedAtom _ -> "UnresolvedAtom"

and string_of_list_pat = function
    | FullPat ls -> "[" ^ (String.concat ~sep:", " (List.map ~f:string_of_pat ls)) ^ "]"
    | HeadTailPat (_hd, _tl) -> assert false (* TODO *)

and string_of_pat = function
    | SinglePat s -> s
    | StringPat s -> sprintf "StringPat (\"%s\")" s
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
    | StringVal s -> Hashtbl.hash (4, s)
    | _ ->
        printf "Tried to hash an unhashable type";
        assert false
