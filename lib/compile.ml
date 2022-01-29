open Base
open Types
open Located

let compile_val v = match v with
    | Number f -> Printf.sprintf "%f" f
    | Integer i -> Printf.sprintf "%d" i
    | Boolean true -> "true"
    | Boolean false -> "false"
    | _ -> assert false

let rec bind pat expr ss = 
    let _bind lhs rhs = bind lhs rhs ss in
    let _ident = List.Assoc.find_exn (List.Assoc.inverse ss.static_idents) ~equal:Int.equal in
    match pat with
    | SinglePat (UnresolvedIdent s) ->
            Printf.sprintf "var __ident_%s = %s" s (compile_expr expr ss)
    | NumberPat _ | IntegerPat _ | StringPat _ | UnresolvedAtomPat _ -> ""
    | TuplePat ls ->
            let bind_ls = String.concat ~sep:", " (List.map ~f:compile_pat ls) in
            Printf.sprintf "var [%s] = %s" bind_ls (compile_expr expr ss)
    | _ -> 
            Stdio.printf "%s" (string_of_pat ss pat);
            assert false

and check_match pat expr ~ss =
    match pat, expr with
    | SinglePat _, _ -> ""
    | NumberPat lhs, expr ->
            Printf.sprintf "if (%f !== (%s)) throw 'Invalid binding'" lhs (compile_expr expr ss)
    | IntegerPat lhs, expr ->
            Printf.sprintf "if (%d !== (%s)) throw 'Invalid binding'" lhs (compile_expr expr ss)
    | _ -> assert false

and compile_pat pat = match pat with
    | TuplePat ls -> Printf.sprintf "[%s]" (String.concat ~sep:", " (List.map ~f:compile_pat ls))
    | SinglePat (UnresolvedIdent s) -> Printf.sprintf("__ident_%s") s
    | NumberPat _ | IntegerPat _ -> "_"
    | _ -> assert false

and compile_expr expr ?tc:(tc=false) ss =
    let compile expr = compile_expr expr ss ~tc in
    match expr with
    | {data = Atomic v; _} ->
            compile_val v
    | {data = BlockExpr ls; _} ->
            let (expr_ls, last) = List.split_n ls ((List.length ls) - 1) in
            let last = List.hd_exn last in
            let fold_step = fun acc expr -> acc ^ (compile expr) ^ ";\n" in
            let items = List.fold_left ~init:"" ~f:fold_step expr_ls in
            Printf.sprintf "{\n%s\nreturn %s\n}" items (compile last)
    | {data = IdentExpr (ResolvedIdent n); _} -> 
            Printf.sprintf "__ident_%s" (List.Assoc.find_exn (List.Assoc.inverse ss.static_idents) ~equal:Int.equal n)
    | {data = IdentExpr (UnresolvedIdent n); _} -> 
            Printf.sprintf "__ident_%s" n
    | {data = Prefix({op = Neg; rhs}); _} -> 
            Printf.sprintf "-%s" (compile rhs)
    | {data = Prefix({op = Not; rhs}); _} -> 
            Printf.sprintf "!%s" (compile rhs)
    | {data = Binary({op = Add; lhs; rhs}); _} -> 
            Printf.sprintf "%s + %s" (compile lhs) (compile rhs)
    | {data = Binary({op = Neg; lhs; rhs}); _} -> 
            Printf.sprintf "%s - %s" (compile lhs) (compile rhs)
    | {data = Binary({op = Mul; lhs; rhs}); _} -> 
            Printf.sprintf "%s * %s" (compile lhs) (compile rhs)
    | {data = Binary({op = Div; lhs; rhs}); _} -> 
            Printf.sprintf "%s / %s" (compile lhs) (compile rhs)
    | {data = Binary({op = EQ; lhs; rhs}); _} -> 
            Printf.sprintf "%s === %s" (compile lhs) (compile rhs)
    | {data = Binary({op = NEQ; lhs; rhs}); _} -> 
            Printf.sprintf "%s !== %s" (compile lhs) (compile rhs)
    | {data = Binary({op = LEQ; lhs; rhs}); _} -> 
            Printf.sprintf "%s <= %s" (compile lhs) (compile rhs)
    | {data = Binary({op = GEQ; lhs; rhs}); _} -> 
            Printf.sprintf "%s >= %s" (compile lhs) (compile rhs)
    | {data = Binary({op = LT; lhs; rhs}); _} -> 
            Printf.sprintf "%s < %s" (compile lhs) (compile rhs)
    | {data = Binary({op = GT; lhs; rhs}); _} -> 
            Printf.sprintf "%s > %s" (compile lhs) (compile rhs)
    | {data = Binary({op = And; lhs; rhs}); _} -> 
            Printf.sprintf "%s && %s" (compile lhs) (compile rhs)
    | {data = Binary({op = Or; lhs; rhs}); _} -> 
            Printf.sprintf "%s || %s" (compile lhs) (compile rhs)
    | {data = Binary({op = Mod; lhs; rhs}); _} -> 
            Printf.sprintf "%s %% %s" (compile lhs) (compile rhs)
    | {data = Let l; _} -> 
            bind l.assignee l.assigned_expr ss
    | {data = TupleExpr ls; _} ->
        Printf.sprintf "[%s]" (String.concat ~sep:", " (List.map ~f:compile ls))
    | {data = LambdaCall {callee = UnresolvedIdent "println"; call_args = {data = TupleExpr [call_args]; _}}; _} -> 
            Printf.sprintf "console.log(%s)" (compile call_args)
    | {data = LambdaCall {callee = UnresolvedIdent fn; call_args = {data = TupleExpr call_args; _}; _}; _} -> 
            let args = String.concat ~sep:", " (List.map ~f:compile call_args) in
            Printf.sprintf "__ident_%s(%s)" fn args
    | {data = LambdaDef d; _} -> 
            let args = match d.lambda_def_args with
            | TuplePat args -> args
            | _ -> assert false
            in
            let args = String.concat ~sep:", " (List.map ~f:compile_pat args) in
            Printf.sprintf "(%s) => %s" args (compile d.lambda_def_expr)
    | {data = FnDef {fn_name = UnresolvedIdent name; fn_def_func}; _} ->
            let args = match fn_def_func.fn_args with
            | TuplePat args -> args
            | _ -> assert false
            in
            let args = String.concat ~sep:", " (List.map ~f:compile_pat args) in
            let body = compile fn_def_func.fn_expr in
            Printf.sprintf "const __ident_%s = (%s) => %s" name args body
    | {data = IfExpr {cond; then_expr; else_expr}; _} ->
            Printf.sprintf "%s ? %s : %s" (compile cond) (compile then_expr) (compile else_expr)
    | _ -> 
            Stdio.printf "%s\n" (string_of_expr ss expr.data);
            assert false

let compile_str ?ss:(ss=let ss, _ = Run.default_state () in ss) ?name:(name="compile") s =
    let tokens = s |> Scanner.scan ~filename:name |> Scanner.skip_newlines in
    let expr_ls =
        let rec loop remaining acc = match (Scanner.skip_newlines remaining) with
        | [] -> 
                List.rev acc
        | _ ->
                let parsed, remaining = Parser.parse remaining 0 in
                loop remaining (parsed::acc)
        in
        loop tokens []
    in
    let fold_step = fun acc expr -> acc ^ (compile_expr expr ss) ^ ";\n" in
    List.fold_left ~init:"" ~f:fold_step expr_ls

let compile_file filename =
    let in_stream = Stdio.In_channel.create filename in
    let in_string = Stdio.In_channel.input_all in_stream in
    compile_str ~name:filename in_string
