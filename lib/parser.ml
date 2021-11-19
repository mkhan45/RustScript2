open Types
open Types.Located
open Scanner
open Printf
open Base

let binary_op_bp = function
    | PipeOp    -> (1, 2)
    | Or        -> (3, 4)
    | And       -> (5, 6)
    | EQ | NEQ | GEQ | LEQ  -> (7, 8)
    | LT | GT   -> (9, 10)
    | Add | Neg -> (11, 12)
    | Mul | Div | Mod -> (13, 14)
    | Head | Tail -> (15, 16)
    | Not -> assert false

let prefix_op_bp = 13

let rec complete_expr: expr t -> (token t) list -> int -> (expr t * (token t) list) = 
    fun lhs ls min_bp -> match (skip_newlines ls) with
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
            let rest = skip_newlines rest in
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
                    LambdaCall {
                        callee = UnresolvedIdent "range_step"; 
                        call_args = TupleExpr [fst;end_;step] |> locate location
                    }
                in
                complete_expr (call |> locate location) rest min_bp
            | [start], {data = RBracket; location}::rest ->
                let call =
                    LambdaCall {
                        callee = UnresolvedIdent "range"; 
                        call_args = TupleExpr [start;end_] |> locate location
                    }
                in
                complete_expr (call |> locate location) rest min_bp
            | _, {location; _}::_ ->
                printf "Invalid range expression at %s\n" (location_to_string location);
                assert false
            | _, [] ->
                printf "Invalid range expression at end of file\n";
                assert false

    and parse_filter_clause ls = 
        let ls = skip_newlines ls in
        match ls with
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
            Caml.exit 0
        | [] ->
            printf "Invalid list comprehension at end of file\n";
            Caml.exit 0

    and parse_listcomp ls expr_list =
        let arg_pat, rest = parse_pat ls in
        let arg_pat = TuplePat [arg_pat] in
        let rest = skip_newlines rest in
        match expr_list, rest with
            | [map_expr], {data = In; location}::rest ->
                let ls_expr, rest = parse rest 0 in
                let map_fn = LambdaDef {lambda_def_expr = map_expr; lambda_def_args = arg_pat} in
                let filter_expr, more = parse_filter_clause rest in
                let locate = locate location in
                begin match filter_expr with
                    | Some e ->
                        let filter_fn = LambdaDef {lambda_def_expr = e; lambda_def_args = arg_pat} in
                        let filter_args = TupleExpr [filter_fn |> locate; ls_expr] in
                        let filtered_ls = LambdaCall {
                            callee = UnresolvedIdent "filter_rev"; 
                            call_args = filter_args |> locate
                        } 
                        in
                        let map_args = TupleExpr [map_fn |> locate; filtered_ls |> locate] in
                        LambdaCall {
                            callee = UnresolvedIdent "map_rev"; 
                            call_args = map_args |> locate
                        } |> locate, more
                    | None ->
                        let map_args = TupleExpr [map_fn |> locate; ls_expr] in
                        LambdaCall {
                            callee = UnresolvedIdent "map"; 
                            call_args = map_args |> locate
                        } |> locate, more
                end
            | _, {location; _}::_ ->
                printf "Invalid list comprehension at %s\n" (location_to_string location);
                Caml.exit 0
            | _ ->
                printf "Invalid list comprehension end of file\n";
                Caml.exit 0

    and aux toks acc = match toks with
        | {data = RBracket; location}::rest -> 
            let expr_list, tail = (acc, None) in
            let parsed_list = ListExpr ((List.rev expr_list), tail) in
            complete_expr (parsed_list |> locate location) rest min_bp
        | _ -> let nx, rest = parse toks 0 in
            let rest = skip_newlines rest in
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
                    Caml.exit 0
                | [] ->
                    printf "Invalid list expression at end of file\n";
                    Caml.exit 0
    in 
    aux xs []

and expr_bp ls min_bp = 
    let ls = skip_newlines ls in
    match ls with
    | ({data = LParen; _}::xs) -> parse_paren_expr xs min_bp
    | ({data = LBracket; _}::xs) -> parse_list_expr xs min_bp
    | ({data = Number f; location})::xs -> complete_expr (Atomic (Number f) |> locate location) xs min_bp
    | ({data = Integer i; location})::xs -> complete_expr (Atomic (Integer i) |> locate location) xs min_bp
    | ({data = Ident n; location})::xs -> complete_expr (IdentExpr (UnresolvedIdent n) |> locate location) xs min_bp
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
                            | ({data; _}::_) ->
                                Stdio.printf "Expected Comma or RParen in parsing of tuple pattern, got %s\n"
                                    (string_of_tok data);
                                Caml.exit 0
                            | [] -> 
                                Stdio.printf "Expected Comma or RParen in parsing of tuple pattern at end of file\n";
                                Caml.exit 0
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
                | Some tail_pat -> HeadTailPat (List.rev pat_list, tail_pat)
            in
            complete_pat (ListPat parsed_list_pat) rest in_list
    | {data = Percent; _}::{data = LBrace; _}::xs ->
        let parse_pair: (token t) list -> (expr t * pattern) * ((token t) list) = fun toks ->
            let key, rest = parse toks 0 in
            match rest with
                | {data = Comma | RBrace; _}::_ ->
                    (* Field punning *)
                    let key, val_pat = match key with
                        | ({data = IdentExpr (UnresolvedIdent n); location}) -> 
                            let key = UnresolvedAtom n |> locate location in
                            let val_pat = SinglePat (UnresolvedIdent n) in
                            key, val_pat
                        | _ -> assert false
                    in
                    (key, val_pat), rest
                | {data = Colon; _}::more ->
                    let val_pat, more = parse_pat more in
                    let key = match key with
                        | ({data = IdentExpr (UnresolvedIdent n); location}) -> UnresolvedAtom n |> locate location
                        | _ -> assert false
                    in
                    (key, val_pat), more
                | {data = Arrow; _}::more ->
                    let val_pat, more = parse_pat more in
                    (key, val_pat), more
                | {data; location}::_ -> 
                    printf "Expected a colon in map at %s, got %s\n" 
                        (location_to_string location)
                        (string_of_tok data);
                    Caml.exit 0
                | [] ->
                    printf "Expected a colon in map at end of file\n";
                    Caml.exit 0
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
    | ({data = Ident s; _})::xs -> complete_pat (SinglePat (UnresolvedIdent s)) xs in_list
    | ({data = Number f; _})::xs -> complete_pat (NumberPat f) xs in_list
    | ({data = Integer i; _})::xs -> complete_pat (IntegerPat i) xs in_list
    | ({data = StringTok f; _})::xs -> complete_pat (StringPat f) xs in_list
    | {data = Underscore; _}::xs -> complete_pat WildcardPat xs in_list
    | {data; location}::_ ->
            printf "Expected pattern at %s, got %s" (location_to_string location) (string_of_tok data);
            Caml.exit 0
    | [] ->
            printf "Expected pattern at end of file";
            Caml.exit 0

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
                | _ -> 
                    let nx, rest = match toks with
                        | {data = Underscore; _}::rest ->
                            BlankCaptureExprHole, rest
                        | {data = Percent; _}::{data = Integer n; _}::rest ->
                            LabeledCaptureExprHole n, rest
                        | _ ->
                            let (nx, rest) = parse toks 0 in
                            CaptureExprArg nx, rest
                    in
                    let acc = nx::acc in
                    match rest with
                        | {data = Comma; _}::rest -> aux rest acc
                        | {data = RParen; _}::rest -> acc, rest
                        | {data; location}::_ ->
                            printf "Error: expected a ), got %s at %s"
                                (string_of_tok data)
                                (location_to_string location);
                            Caml.exit 0
                        | [] ->
                            printf "Error: missing parentheses in argument list at end of file";
                            Caml.exit 0
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
                        Caml.exit 0
                | [] -> 
                        printf "Expected an arrow at end of file\n";
                        Caml.exit 0
        end
    | _ -> assert false

and parse_lambda_call = function
    | ({data = Ident lambda_name; location})::xs -> 
        begin match parse_args xs with
            | args, rest when List.for_all args ~f:(function | CaptureExprArg _ -> true | _ -> false) ->
                let args = List.map args ~f:(function | CaptureExprArg e -> e | _ -> assert false) in
                let call_args = TupleExpr args |> locate location in
                (LambdaCall {callee = UnresolvedIdent lambda_name; call_args = call_args}, rest)
            | args, rest ->
                LambdaCaptureExpr {capture_expr_fn = UnresolvedIdent lambda_name; capture_expr_args = args}, rest
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
            | {data; location}::_ ->
                printf "Error parsing if expression at %s: expected Then, got %s\n" 
                    (location_to_string location)
                    (string_of_tok data);
                Caml.exit 0
            | [] ->
                printf "Error parsing if expression at end of file";
                Caml.exit 0
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
            let ls = skip_newlines ls in
            let key_expr, xs = parse ls 0 in
            let xs = skip_newlines xs in
            match xs with
                | {data = Colon; _}::xs ->
                    let xs = skip_newlines xs in
                    let key = match key_expr with
                        | {data = IdentExpr (UnresolvedIdent n); _} -> UnresolvedAtom n
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
        let rec aux ls acc = match (skip_newlines ls) with
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
            | {data; location}::_ -> 
                printf "Expected Pipe, Comma, or RBrace at %s, got %s" 
                    (location_to_string location) 
                    (string_of_tok data);
                Caml.exit 0
            | [] -> 
                printf "Expected Pipe, Comma, or RBrace at end of file" ;
                Caml.exit 0
        in begin match rest with
            | {data = RBrace; _}::xs ->
                (MapExpr ([], None), xs)
            | _ ->
                let k0, v0, rest = parse_key_val rest in
                let rest = skip_newlines rest in
                let (res, tail), more = aux rest [(k0, v0)] in
                let res = List.map ~f:(fun (a, b) -> a |> locate b.location, b) res in
                (MapExpr (List.rev res, tail), more)
        end

    | _ ->
        printf "Expected LBrace\n";
        assert false

and parse_match_expr ls loc =
    let (match_val, rest) = parse ls 0 in
    let rest = skip_newlines rest in
    let rec parse_match_arms toks acc =
        let toks = skip_newlines toks in
        match toks with
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
        printf "No match arms in match expression at %s\n" (location_to_string loc);
        Caml.exit 0
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
        | {data = (True|False|Number _|Integer _| Ident _| StringTok _); _}::_ -> expr_bp s min_bp
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
            let (match_parsed, xs) = parse_match_expr xs location in
            complete_expr (match_parsed |> locate location) xs min_bp
        | {location; data = Pipe}::_ -> 
            printf "Expected expression at %s, got %s. Did you forget a %%?\n" 
                (location_to_string location) (string_of_tok Pipe);
            Caml.exit 0
        | {location; data}::_ -> 
            printf "Expected expression at %s, got %s\n" (location_to_string location) (string_of_tok data);
            Caml.exit 0
        | [] -> 
            printf "Expected expression at end of file\n";
            assert false

let parse_str s filename = parse (Scanner.scan s ~filename:filename) 0
