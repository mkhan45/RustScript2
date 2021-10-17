open Core
open Scanner
open Printf

let op_bp = function
    | EQ -> (1, 2)
    | LT | GT -> (3, 4)
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
    | True::xs -> complete_expr (Atomic (Boolean true)) xs min_bp
    | False::xs -> complete_expr (Atomic (Boolean false)) xs min_bp
    | _ -> assert false;;

let rec parse_pat ls = match ls with
    | LParen::xs ->
            let rec aux toks acc = match toks with
                | RParen::rest -> (acc, rest)
                | _ ->  let (nx, rest) = parse_pat toks in
                        match rest with
                            | (Comma::rest) -> aux rest (nx::acc)
                            | (RParen::rest) -> (nx::acc, rest)
                            | _ -> assert false
            in 
            let (parsed, remaining) = aux xs [] 
            in (TuplePat (List.rev parsed), remaining)
    | (Ident s)::xs -> (SinglePat s, xs)
    | _ ->
            print_toks ls;
            assert false

and parse_let ls =
    let (pat, xs) = parse_pat ls in
    match xs with
        | Equal::xs ->
                let (rhs, rest) = parse xs in
                let let_expr: expr = Let {assignee = pat; assigned_expr = rhs}
                in (let_expr, rest)
        | _ -> assert false

and parse_tup ls = 
    match ls with
    | LParen::xs ->
            let rec aux toks acc = match toks with
                | RParen::rest -> (acc, rest)
                | _ -> let (nx, rest) = parse toks in
                       match rest with
                           | (Comma::rest) -> aux rest (nx::acc)
                           | (RParen::rest) -> (nx::acc, rest)
                           | _ -> assert false
            in
            let (parsed, remaining) = aux xs []
            in 
            (TupleExpr (List.rev parsed), remaining)
    | _ -> 
            printf "Error parsing as tuple: ";
            print_toks ls;
            assert false

and parse_args ls = 
    match ls with
    | LParen::xs ->
            let rec aux toks acc = match toks with
                | RParen::rest -> (acc, rest)
                | ((Ident n)::rest)|(Comma::(Ident n)::rest) -> aux rest (n::acc)
                | _ -> 
                        printf "Error parsing args: ";
                        print_toks toks;
                        assert false
            in
            let (parsed, remaining) = aux xs []
            in 
            (List.rev parsed, remaining)
    | _ -> 
            printf "Error parsing as tuple: ";
            print_toks ls;
            assert false

and parse_lambda = function
    | Fn::xs -> 
            begin
                let (args, rest) = parse_args xs in
                match rest with
                    | Arrow::xs ->
                            let (lambda_expr, rest) = parse xs in
                            let lambda = Lambda {lambda_expr = lambda_expr; lambda_args = args} 
                            in (Atomic lambda, rest)
                    | _ -> assert false
            end
    | _ -> assert false

and parse_lambda_call = function
    | (Ident lambda_name)::xs ->
            begin
                match parse_tup xs with
                    | TupleExpr (call_args), rest ->
                            (LambdaCall {callee = lambda_name; call_args = call_args}, rest)
                    | _ -> assert false
            end
    | _ -> assert false

and parse: token list -> expr * (token list) = fun s -> 
    match s with
    | (Ident _)::LParen::_ -> parse_lambda_call s
    | (True|False|Number _| Ident _)::_ -> expr_bp s 0
    | Let::xs -> parse_let xs
    | LParen::_ ->  parse_tup s
    | Fn::_ ->  parse_lambda s
    | _ -> assert false (* TODO *);;

let parse_str s = s |> Scanner.scan |> parse
