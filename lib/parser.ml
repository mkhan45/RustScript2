open Types
open Scanner
open Printf
open Base

let op_bp = function
    | EQ -> (1, 2)
    | LT | GT -> (3, 4)
    | Add | Sub -> (4, 5)
    | Mul | Div -> (6, 7)

let rec complete_expr lhs ls min_bp = match ls with
    | (Operator op)::xs ->
            let (l_bp, r_bp) = op_bp op
            in
            if l_bp < min_bp 
                then (lhs, ls)
                else let (rhs, rem) = parse xs r_bp in 
                     let complete = Binary {op = op; lhs = lhs; rhs = rhs}
                      in complete_expr complete rem min_bp
    | _ -> (lhs, ls)

and expr_bp ls min_bp = match ls with
    | (LParen::xs) -> 
            let rec aux toks saw_comma acc = match toks with
                | RParen::rest -> acc, rest, saw_comma
                | _ -> let nx, rest = parse toks 0 in begin
                    match rest with
                        | Comma::rest -> aux rest true (nx::acc)
                        | RParen::rest -> (nx::acc), rest, saw_comma
                        | _ -> assert false
                end
            in let expr_list, rest, saw_comma = aux xs false [] in begin
               match expr_list, saw_comma with
                   | _, true -> complete_expr (TupleExpr (List.rev expr_list)) rest min_bp
                   | [], false -> complete_expr (TupleExpr []) rest min_bp
                   | _, false -> complete_expr (List.hd_exn expr_list) rest min_bp
            end
    | (Number f)::xs -> complete_expr (Atomic (Number f)) xs min_bp
    | (Ident n)::xs -> complete_expr (Ident n) xs min_bp
    | True::xs -> complete_expr (Atomic (Boolean true)) xs min_bp
    | False::xs -> complete_expr (Atomic (Boolean false)) xs min_bp
    | _ -> assert false

and parse_pat ls = match ls with
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
                let (rhs, rest) = parse xs 0 in
                let let_expr: expr = Let {assignee = pat; assigned_expr = rhs}
                in (let_expr, rest)
        | _ -> assert false

and parse_args toks = 
    match toks with
    | LParen::xs ->
            let rec aux toks acc = match toks with
                | RParen::rest -> (acc, rest)
                | _ -> let (nx, rest) = parse toks 0 in begin
                    match rest with
                        | Comma::rest -> aux rest (nx::acc)
                        | RParen::rest -> (nx::acc), rest
                        | _ -> assert false
                end
            in
            let (parsed, remaining) = aux xs []
            in 
            (List.rev parsed, remaining)
    | _ -> 
            printf "Error parsing args: ";
            print_toks toks;
            assert false

and parse_lambda = function
    | Fn::xs -> 
            begin
                let (args, rest) = parse_pat xs in
                match rest with
                    | Arrow::xs ->
                            let (lambda_expr, rest) = parse xs (-1) in
                            let lambda = 
                                LambdaDef {lambda_def_expr = lambda_expr; lambda_def_args = args} 
                            in (lambda, rest)
                    | _ ->
                            printf "Expected an arrow, got (%s)\n" (string_of_toks rest);
                            assert false
            end
    | _ -> assert false

and parse_lambda_call = function
    | (Ident lambda_name)::xs -> begin
            match parse_args xs with
                | args, rest ->
                        let call_args = TupleExpr args in
                        (LambdaCall {callee = lambda_name; call_args = call_args}, rest)
    end
    | _ -> assert false

and parse_if_expr = function
    | If::xs -> begin
        let (cond, xs) = parse xs 0 in
        match skip_newlines xs with
            | Then::xs -> begin
                let (then_expr, xs) = parse xs 0 in
                match (skip_newlines xs) with
                    | Else::xs ->
                        let (else_expr, rest) = parse xs 0 in
                        (IfExpr {cond = cond; then_expr = then_expr; else_expr = else_expr}, rest)
                    | _ -> 
                        printf "Error parsing as else: ";
                        print_toks xs;
                        assert false
                end
            | _ -> assert false
    end
    | _ -> assert false

and parse_block_expr ls =
    let rec aux ls acc = match skip_newlines ls with
        | RBrace::rest -> (BlockExpr (List.rev acc), rest)
        | rest ->
                let (next_expr, rest) = parse rest 0 in
                aux rest (next_expr::acc)
    in aux ls []

and parse: token list -> int -> expr * (token list) = fun s min_bp ->
    match s with
    | LBrace::xs -> parse_block_expr xs
    | (Ident _)::LParen::_ -> 
            let (call, xs) = parse_lambda_call s in
            complete_expr call xs min_bp
    | LParen::_ -> expr_bp s 0
    | (True|False|Number _| Ident _)::_ -> expr_bp s min_bp
    | Let::xs -> parse_let xs
    | Fn::_ -> 
            let (lambda_parsed, xs) = parse_lambda s in
            complete_expr lambda_parsed xs min_bp
    | If::_ -> 
            let (if_parsed, xs) = parse_if_expr s in
            complete_expr if_parsed xs min_bp
    | _ -> 
            printf "Expected expression, got (%s)\n" (string_of_toks s);
            assert false

let parse_str s = parse (Scanner.scan s) 0
