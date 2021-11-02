open Types
open Types.Located
open Scanner
open Printf
open Base

let binary_op_bp = function
    | Or        -> (1, 2)
    | And       -> (3, 4)
    | EQ | NEQ | GEQ | LEQ  -> (5, 6)
    | LT | GT   -> (7, 8)
    | Add | Neg -> (9, 10)
    | Mul | Div | Mod -> (11, 12)
    | Head | Tail -> (13, 14)
    | Not -> assert false

let prefix_op_bp = 13

let rec complete_expr: expr t -> (token t) list -> int -> (expr t * (token t) list) = 
    fun lhs ls min_bp -> match ls with
    | {data = Percent; location}::xs -> complete_expr lhs (({data = Operator Mod; location})::xs) min_bp
    | ({data = Operator op; location})::xs ->
            let (l_bp, r_bp) = binary_op_bp op in
            if l_bp < min_bp then 
                (lhs, ls)
            else 
                let (rhs, rem) = parse xs r_bp in 
                let complete = Binary {op = op; lhs = lhs; rhs = rhs} |> locate location in 
                complete_expr complete rem min_bp
    | _ -> (lhs, ls)

and parse_prefix_expr op xs min_bp =
    let (rhs, rem) = parse xs min_bp in 
    let complete = Prefix {op = op; rhs = rhs} |> locate rhs.location in
    complete_expr complete rem min_bp

and parse_paren_expr xs min_bp =
    let rec aux toks saw_comma acc = match toks with
        | {data = RParen; location}::rest -> acc, rest, saw_comma, location
        | _ -> 
            let nx, rest = parse toks 0 in
            match rest with
                | {data = Comma; _}::rest -> aux rest true (nx::acc)
                | {data = RParen; location}::rest -> (nx::acc), rest, saw_comma, location
                | {location; _}::_ ->
                    printf "Error parsing parenthesised expression at %s\n" (location_to_string location);
                    assert false
                | [] ->
                    printf "Error parsing parenthesised expression at end of file\n";
                    assert false
    in 
    let expr_list, rest, saw_comma, location = aux xs false [] in
    let locate = locate location in
    match expr_list, saw_comma with
        | _, true -> complete_expr (TupleExpr (List.rev expr_list) |> locate) rest min_bp
        | [], false -> complete_expr (TupleExpr [] |> locate) rest min_bp
        | [paren_expr], false -> complete_expr paren_expr rest min_bp
        | _, false -> assert false


and parse_list_expr xs min_bp =
    let rec parse_tail ls expr_list =
        let tail, more = parse ls 0 in
        match more with
            | {data = RBracket; location}::rest ->
                let parsed_list = ListExpr((List.rev expr_list), Some tail) in
                complete_expr (parsed_list |> locate location) rest min_bp
            | {location; _}::_ -> 
                printf "Error parsing list expression at %s\n" (location_to_string location);
                assert false
            | [] ->
                printf "Error parsing list expression at end of file\n";
                assert false

    and parse_range ls expr_list =
        let (end_, rest) = parse ls 0 in
        match expr_list, rest with
            | [snd; fst], {data = RBracket; location}::rest ->
                let step = (Binary {lhs = snd; rhs = fst; op = Neg}) |> locate location in
                let call =
                    LambdaCall {callee = "range_step"; call_args = TupleExpr [fst;end_;step] |> locate location}
                in
                complete_expr (call |> locate location) rest min_bp
            | [start], {data = RBracket; location}::rest ->
                let call =
                    LambdaCall {callee = "range"; call_args = TupleExpr [start;end_] |> locate location}
                in
                complete_expr (call |> locate location) rest min_bp
            | _, {location; _}::_ ->
                printf "Invalid range expression at %s\n" (location_to_string location);
                assert false
            | _, [] ->
                printf "Invalid range expression at end of file\n";
                assert false

    and parse_filter_clause ls = match ls with
        | {data = RBracket; _}::xs -> None, xs
        | {data = If; _}::rest -> begin match parse rest 0 with
            | e, {data = RBracket; _}::more ->
                Some e, more
            | _, {location; _}::_ ->
                printf "Invalid filter clause in list comprehension at %s\n" (location_to_string location);
                assert false
            | _, [] ->
                printf "Invalid filter clause in list comprehension at end of file\n";
                assert false
        end
        | {location; _}::_ ->
            printf "Invalid list comprehension at %s\n" (location_to_string location);
            assert false
        | [] ->
            printf "Invalid list comprehension at end of file\n";
            assert false

    and parse_listcomp ls expr_list =
        let arg_pat, rest = parse_pat ls in
        let arg_pat = TuplePat [arg_pat] in
        match expr_list, rest with
            | [map_expr], {data = In; location}::rest ->
                let ls_expr, rest = parse rest 0 in
                let map_fn = LambdaDef {lambda_def_expr = map_expr; lambda_def_args = arg_pat} in
                let map_args = TupleExpr [map_fn |> locate location; ls_expr.data |> locate location] in
                let mapped_ls = LambdaCall {callee = "map_rev"; call_args = map_args |> locate location} in
                let filter_expr, more = parse_filter_clause rest in
                let locate = locate location in
                begin match filter_expr with
                    | Some e ->
                        let filter_fn = LambdaDef {lambda_def_expr = e; lambda_def_args = arg_pat} in
                        let filter_args = TupleExpr [filter_fn |> locate; mapped_ls |> locate] in
                        LambdaCall {callee = "filter_rev"; call_args = filter_args |> locate} |> locate, more
                    | None ->
                        let reverse_args = TupleExpr [mapped_ls |> locate] in
                        LambdaCall {callee = "reverse"; call_args = reverse_args |> locate} |> locate, more
                end
            | _, {location; _}::_ ->
                printf "Invalid list comprehension at %s\n" (location_to_string location);
                assert false
            | _ ->
                printf "Invalid list comprehension end of file\n";
                assert false

    and aux toks acc = match toks with
        | {data = RBracket; location}::rest -> 
            let expr_list, tail = (acc, None) in
            let parsed_list = ListExpr ((List.rev expr_list), tail) in
            complete_expr (parsed_list |> locate location) rest min_bp
        | _ -> let nx, rest = parse toks 0 in
            match rest with
                | {data = Comma; _}::rest -> aux rest (nx::acc)
                | {data = RBracket; location}::rest -> 
                    let expr_list, tail = (nx::acc, None) in
                    let parsed_list = ListExpr ((List.rev expr_list), tail) in
                    complete_expr (parsed_list |> locate location) rest min_bp
                | {data = Pipe; _}::rest -> parse_tail rest (nx::acc)
                | {data = DotDot; _}::rest -> parse_range rest (nx::acc)
                | {data = For; _}::rest -> parse_listcomp rest (nx::acc)
                | {location; _}::_ -> 
                    printf "Invalid list expression at %s\n" (location_to_string location);
                    assert false
                | [] ->
                    printf "Invalid list expression at end of file\n";
                    assert false
    in 
    aux xs []

and expr_bp ls min_bp = match ls with
    | ({data = LParen; _}::xs) -> parse_paren_expr xs min_bp
    | ({data = LBracket; _}::xs) -> parse_list_expr xs min_bp
    | ({data = Number f; location})::xs -> complete_expr (Atomic (Number f) |> locate location) xs min_bp
    | ({data = Ident n; location})::xs -> complete_expr (IdentExpr n |> locate location) xs min_bp
    | ({data = StringTok s; location})::xs -> complete_expr (Atomic (StringVal s) |> locate location) xs min_bp
    | ({data = Operator op; _})::xs -> parse_prefix_expr op xs min_bp
    | {data = True; location}::xs -> complete_expr (Atomic (Boolean true) |> locate location) xs min_bp
    | {data = False; location}::xs -> complete_expr (Atomic (Boolean false) |> locate location) xs min_bp
    | _ -> assert false

and complete_pat lhs ls in_list = match ls with
    | {data = Pipe; _}::xs when not in_list ->
        let rhs, rest = parse_pat xs in
        OrPat (lhs, rhs), rest
    | {data = As; _}::({data = Ident n; _})::xs ->
        AsPat (lhs, n), xs
    | {data = As; location}::_ ->
        printf "Expected a name at %s\n" (location_to_string location);
        assert false
    | _ -> lhs, ls

and parse_pat ?in_list:(in_list=false) ls = match ls with
    | {data = LParen; _}::xs ->
            let rec aux toks acc = match toks with
                | {data = RParen; _}::rest -> (acc, rest)
                | _ ->  let (nx, rest) = parse_pat toks in
                        match rest with
                            | ({data = Comma; _}::rest) -> aux rest (nx::acc)
                            | ({data = RParen; _}::rest) -> (nx::acc, rest)
                            | _ -> assert false
            in 
            let (parsed, remaining) = aux xs [] 
            in complete_pat (TuplePat (List.rev parsed)) remaining in_list
    | {data = LBracket; _}::xs ->
            let rec aux toks acc = match toks with
                | {data = RBracket; _}::rest -> (acc, None), rest
                | _ -> let (nx, rest) = parse_pat ~in_list:true toks in
                    match rest with
                        | {data = Comma; _}::rest -> aux rest (nx::acc)
                        | {data = RBracket; _}::rest -> (nx::acc, None), rest
                        | {data = Pipe; _}::rest ->
                            let tail_pat, more = parse_pat rest in begin
                                match more with
                                    | {data = RBracket; _}::rest -> (nx::acc, Some tail_pat), rest
                                    | _ -> assert false
                            end
                        | _ -> assert false
            in
            let (pat_list, tail), rest = aux xs [] in
            let parsed_list_pat = match tail with
                | None -> FullPat (List.rev pat_list)
                | Some tail_pat -> HeadTailPat (pat_list, tail_pat)
            in
            complete_pat (ListPat parsed_list_pat) rest in_list
    | {data = Percent; _}::{data = LBrace; _}::xs ->
        let parse_pair: (token t) list -> (expr t * pattern) * ((token t) list) = fun toks ->
            let key, rest = parse toks 0 in
            match rest with
                | {data = Colon; _}::more ->
                    let val_pat, more = parse_pat more in
                    let key = match key with
                        | ({data = IdentExpr n; location}) -> UnresolvedAtom n |> locate location
                        | _ -> assert false
                    in
                    (key, val_pat), more
                | {data = Arrow; _}::more ->
                    let val_pat, more = parse_pat more in
                    (key, val_pat), more
                | _ -> 
                    printf "Expected a colon\n";
                    assert false
        in
        let rec aux toks acc = match toks with
            | {data = RBrace; _}::rest -> acc, rest
            | {data = Comma; _}::rest ->
                let pair, more = parse_pair rest in
                aux more (pair::acc)
            | _ -> assert false
        in begin match xs with
            | {data = RBrace; _}::rest -> complete_pat (MapPat []) rest in_list
            | _ ->
                let first_pair, rest = parse_pair xs in
                let pair_ls, more = aux rest [first_pair] in
                complete_pat (MapPat (List.rev pair_ls)) more in_list
        end
    | {data = Percent; _}::_ -> 
        printf "Expected LBrace\n";
        assert false
    | {data = Colon; _}::({data = Ident s; _})::xs -> complete_pat (UnresolvedAtomPat s) xs in_list
    | ({data = Ident s; _})::xs -> complete_pat (SinglePat s) xs in_list
    | ({data = Number f; _})::xs -> complete_pat (NumberPat f) xs in_list
    | ({data = StringTok f; _})::xs -> complete_pat (StringPat f) xs in_list
    | {data = Underscore; _}::xs -> complete_pat WildcardPat xs in_list
    | _ ->
            printf "Expected pattern";
            assert false

and parse_let ls =
    let (pat, xs) = parse_pat ls in
    match xs with
        | {data = Equal; _}::xs ->
                let (rhs, rest) = parse xs 0 in
                let let_expr: expr = Let {assignee = pat; assigned_expr = rhs}
                in (let_expr, rest)
        | {data = LParen; _}::_ -> begin match pat with
            | SinglePat fn_name ->
                let (fn_args, xs) = parse_pat xs in begin
                match xs with
                    | {data = Equal; _}::xs ->
                        let (fn_expr, rest) = parse xs 0 in
                        let def = FnDef {fn_name; fn_def_func = {fn_args; fn_expr = fn_expr}} in
                        (def, rest)
                    | _ -> assert false
                end
            | _ -> assert false
        end
        | _ -> assert false

and parse_args toks =
    match toks with
    | {data = LParen; _}::xs ->
            let rec aux toks acc = match toks with
                | {data = RParen; _}::rest -> (acc, rest)
                | _ -> let (nx, rest) = parse toks 0 in begin
                    match rest with
                        | {data = Comma; _}::rest -> aux rest (nx::acc)
                        | {data = RParen; _}::rest -> (nx::acc), rest
                        | _ -> assert false
                end
            in
            let (parsed, remaining) = aux xs []
            in
            (List.rev parsed, remaining)
    | _ ->
            printf "Error parsing args";
            assert false

and parse_lambda = function
    | {data = Fn; _}::xs -> 
        begin
            let (args, rest) = parse_pat xs in
            match rest with
                | {data = Arrow; _}::xs ->
                        let (lambda_expr, rest) = parse xs (-1) in
                        let lambda = 
                            LambdaDef {lambda_def_expr = lambda_expr; lambda_def_args = args} 
                        in (lambda, rest)
                | {location; _}::_ ->
                        printf "Expected an arrow at %s\n" (location_to_string location);
                        assert false
                | [] -> 
                        printf "Expected an arrow at end of file\n";
                        assert false
        end
    | _ -> assert false

and parse_lambda_call = function
    | ({data = Ident lambda_name; location})::xs -> begin
            match parse_args xs with
                | args, rest ->
                        let call_args = TupleExpr args |> locate location in
                        (LambdaCall {callee = lambda_name; call_args = call_args}, rest)
    end
    | _ -> assert false


and parse_if_expr = function
    | {data = If; _}::xs -> begin
        let (cond, xs) = parse xs 0 in
        match skip_newlines xs with
            | {data = Then; _}::xs -> begin
                let (then_expr, xs) = parse xs 0 in
                match (skip_newlines xs) with
                    | {data = Else; _}::xs ->
                        let (else_expr, rest) = parse xs 0 in
                        (IfExpr {cond = cond; then_expr = then_expr; else_expr = else_expr}, rest)
                    | _ -> 
                        printf "Error parsing as else: ";
                        assert false
                end
            | _ -> assert false
    end
    | _ -> assert false

and parse_block_expr ls =
    let rec aux ls acc = match skip_newlines ls with
        | {data = RBrace; _}::rest -> (BlockExpr (List.rev acc), rest)
        | rest ->
                let (next_expr, rest) = parse rest 0 in
                aux rest (next_expr::acc)
    in aux ls []

and parse_map = function
    | {data = LBrace; _}::rest ->
        let rest = skip_newlines rest in
        let parse_key_val ls =
            let key_expr, xs = parse ls 0 in
            match xs with
                | {data = Colon; _}::xs ->
                    let xs = skip_newlines xs in
                    let key = match key_expr with
                        | {data = IdentExpr n; _} -> UnresolvedAtom n
                        | _ ->
                            printf "Only use colon in maps with atom keys";
                            assert false
                    in
                    let (val_expr, more) = parse xs 0 in
                    (key, val_expr, more)
                | {data = Arrow; _}::xs ->
                    let xs = skip_newlines xs in
                    let (val_expr, more) = parse xs 0 in
                    (key_expr.data, val_expr, more)
                | _ ->
                    printf "Expected comma";
                    assert false
        in
        let rec aux ls acc = match ls with
            | {data = RBrace; _}::more -> ((acc, None), more)
            | {data = Comma; _}::xs ->
                let xs = skip_newlines xs in
                let (key_expr, val_expr, rest) = parse_key_val xs in
                let rest = skip_newlines rest in
                aux rest ((key_expr, val_expr)::acc)
            | {data = Pipe; _}::xs ->
                let xs = skip_newlines xs in
                let tail, rest = parse xs 0 in
                let rest = skip_newlines rest in begin
                match rest with
                    | {data = RBrace; _}::more -> ((acc, Some tail), more)
                    | _ ->
                        printf "Invalid map expression\n";
                        assert false
                end
            | _ -> assert false
        in begin match rest with
            | {data = RBrace; _}::xs ->
                (MapExpr ([], None), xs)
            | _ ->
                let k0, v0, rest = parse_key_val rest in
                let (res, tail), more = aux rest [(k0, v0)] in
                let res = List.map ~f:(fun (a, b) -> a |> locate b.location, b) res in
                (MapExpr (List.rev res, tail), more)
        end

    | _ ->
        printf "Expected LBrace\n";
        assert false

and parse_match_expr ls =
    let (match_val, rest) = parse ls 0 in
    let rest = skip_newlines rest in
    let rec parse_match_arms toks acc = match toks with
        | {data = Pipe; _}::xs ->
            let arm_pat, rest = parse_pat xs in begin
            let cond, rest = match rest with
                | {data = MatchArrow; _}::_ -> None, rest
                | {data = When; _}::rest ->
                        let cond, rest = parse rest 0 in
                        Some cond, rest
                | _ ->
                    printf "Expected When or MatchArrow";
                    assert false
            in
            match rest with
                | {data = MatchArrow; _}::rest ->
                    let rest = skip_newlines rest in
                    let arm_expr, rest = parse rest 0 in begin
                        match rest with
                            | {data = Newline; _}::xs -> 
                                parse_match_arms xs ((arm_pat, arm_expr, cond)::acc)
                            | {data = Pipe; _}::_ -> 
                                printf "Must break line after each match arm\n";
                                assert false
                            | _ ->
                                printf "Error parsing expression in match arm\n";
                                assert false
                    end
                | _ ->
                    printf "Expected an arrow\n";
                    assert false
            end
        | more -> List.rev acc, more
    in
    let (match_arms, rest) = parse_match_arms rest [] in
    if (not (phys_equal match_arms [])) then
        MatchExpr {match_val = match_val; match_arms = match_arms}, rest
    else begin
        printf "No match arms in match expression\n";
        assert false
    end

and parse: (token Located.t) list -> int -> (expr Located.t) * ((token Located.t) list) = fun s min_bp ->
    let s = skip_newlines s in
    match s with
        | {data = LBrace; location}::xs -> 
            let (block, xs) = parse_block_expr xs in
            complete_expr (block |> locate location) xs min_bp
        | {data = Percent; location}::xs -> 
            let (map, xs) = parse_map xs in
            complete_expr (map |> locate location) xs min_bp
        | {data = Colon; location}::({data = Ident n; _})::xs -> 
            complete_expr ((UnresolvedAtom n) |> locate location) xs min_bp
        | ({data = Ident _; location})::{data = LParen; _}::_ -> 
            let (call, xs) = parse_lambda_call s in
            complete_expr (call |> locate location) xs min_bp
        | {data = LParen; _}::_ -> expr_bp s 0
        | {data = LBracket; _}::_ -> expr_bp s 0
        | ({data = Operator _; _})::_ -> expr_bp s 0
        | {data = (True|False|Number _| Ident _| StringTok _); _}::_ -> expr_bp s min_bp
        | {data = Let; location}::xs -> 
            let l, remaining = parse_let xs in
            l |> locate location, remaining
        | {data = Fn; location}::_ -> 
            let (lambda_parsed, xs) = parse_lambda s in
            complete_expr (lambda_parsed |> locate location) xs min_bp
        | {data = If; location}::_ -> 
            let (if_parsed, xs) = parse_if_expr s in
            complete_expr (if_parsed |> locate location) xs min_bp
        | {data = Match; location}::xs -> 
            let (match_parsed, xs) = parse_match_expr xs in
            complete_expr (match_parsed |> locate location) xs min_bp
        | {location; _}::_ -> 
            printf "Expected expression at %s\n" (location_to_string location);
            assert false
        | [] -> 
            printf "Expected expression at end of file\n";
            assert false

let parse_str s = parse (Scanner.scan s) 0
