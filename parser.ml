open Core
open Scanner

let op_bp = function
    | Add | Sub -> (4, 5)
    | Mul | Div -> (6, 7);;

let rec complete_expr lhs ls min_bp = match ls with
    | (Operator op)::xs ->
            let (l_bp, r_bp) = op_bp op
            in
            if l_bp < min_bp 
                then (lhs, ls)
                else let (rhs, rem) = expr_bp xs r_bp in 
                     let complete = Binary {op = op; lhs = lhs; rhs = rhs}
                      in complete_expr complete rem min_bp
    | _ -> (lhs, ls)

and expr_bp ls min_bp = match ls with
    | (LParen::xs) ->
            let (paren_expr, temp) = expr_bp xs 0
            in
            if temp == [] || List.hd temp != RParen
                then assert false
                else complete_expr paren_expr (List.tl temp) min_bp
    | (Number f)::xs -> complete_expr (Atomic (Number f)) xs min_bp
    | (Ident n)::xs -> complete_expr (Ident n) xs min_bp
    | _ -> assert false;;

let rec parse_pat ls = match ls with
    | LParen::xs ->
            let rec aux toks acc = match toks with
                | RParen::rest -> (acc, rest)
                | _ -> let (nx, rest) = parse_pat toks in
                        aux rest (nx::acc)
            in 
            let (parsed, remaining) = aux xs [] 
            in (TuplePat (List.rev parsed), remaining)
    | (Ident s)::xs -> (SinglePat s, xs)
    | _ -> assert false

and parse_let ls =
    let (pat, xs) = parse_pat ls in
    match xs with
        | Equal::xs -> let (rhs, rest) = parse xs in
                       let let_expr: expr = Let {assignee = pat; assigned_expr = rhs}
                        in (let_expr, rest)
        | _ -> assert false

and parse: token list -> expr * (token list) = fun s -> match s with
    | Let::xs -> parse_let xs
    | (Number _ | Ident _)::_ -> expr_bp s 0
    | _ -> assert false (* TODO *);;

let parse_str s = s |> Scanner.scan |> parse
