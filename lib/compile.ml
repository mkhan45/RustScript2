open Base
open Types
open Located

let ident_id = ref 0
let new_id () = 
    ident_id := !ident_id + 1;
    !ident_id

let idents = ref (Hashtbl.create (module String))

let new_ident name = 
    match Hashtbl.find !idents name with
    | Some id -> 
            Printf.sprintf "__ident_%d_%s" id name
    | None ->
        let id = new_id () in
        let s = Printf.sprintf "__ident_%d_%s" id name in
        Hashtbl.set ~key:name ~data:id !idents;
        s

and get_ident name = match name with
    | "inspect" -> "rsc_inspect"
    | _ ->
        let id = Hashtbl.find_exn !idents name in
        Printf.sprintf "__ident_%d_%s" id name

let compile_val v = match v with
    | Number f -> Printf.sprintf "%f" f
    | Integer i -> Printf.sprintf "%d" i
    | Boolean true -> "true"
    | Boolean false -> "false"
    | StringVal s -> Printf.sprintf "\"%s\"" s
    | _ -> assert false

let rec bind pat expr ss = 
    let _bind lhs rhs = bind lhs rhs ss in
    let _ident = List.Assoc.find_exn (List.Assoc.inverse ss.static_idents) ~equal:Int.equal in
    match pat with
    | SinglePat (UnresolvedIdent s) ->
            let var_ident = new_ident s in
            Printf.sprintf "var %s = %s" var_ident (compile_expr expr ss)
    | WildcardPat | NumberPat _ | IntegerPat _ | StringPat _ | UnresolvedAtomPat _ -> 
            compile_expr expr ss
    | TuplePat ls ->
            let bind_ls = String.concat ~sep:", " (List.map ~f:compile_bind_pat ls) in
            Printf.sprintf "var [%s] = %s" bind_ls (compile_expr expr ss)
    | _ -> 
            Stdio.printf "%s" (string_of_pat ss pat);
            assert false

and pat_cond expr pat ~ss = match pat with
    | WildcardPat | SinglePat _ -> "true"
    | NumberPat lhs  -> Printf.sprintf "%s === %f" (compile_expr expr ss) lhs
    | IntegerPat lhs -> Printf.sprintf "%s === %d" (compile_expr expr ss) lhs
    | _ -> Printf.sprintf "rsc_matches(%s, %s)" (compile_expr expr ss) (compile_pat pat)

and check_match pat expr ~ss =
    let cond = pat_cond expr pat ~ss in
    Printf.sprintf "if (!(%s)) throw 'Invalid Binding'" cond

and compile_pat pat = match pat with
    | SinglePat (UnresolvedIdent s) -> new_ident s
    | TuplePat ls -> Printf.sprintf "[%s]" (String.concat ~sep:", " (List.map ~f:compile_pat ls))
    | IntegerPat i -> Printf.sprintf "%d" i
    | NumberPat f -> Printf.sprintf "%f" f
    | WildcardPat -> "null"
    | _ -> assert false

and compile_bind_pat pat = match pat with
    | SinglePat (UnresolvedIdent s) -> new_ident s
    | TuplePat ls -> Printf.sprintf "[%s]" (String.concat ~sep:", " (List.map ~f:compile_pat ls))
    | IntegerPat _ | NumberPat _ | WildcardPat -> new_ident "unused"
    | _ -> assert false

and compile_expr expr ?tc:(tc=false) ss =
    let compile ?tc:(tc=false) expr = compile_expr ~tc expr ss in
    match expr with
    | {data = Atomic v; _} ->
            compile_val v
    | {data = BlockExpr ls; _} ->
            let (expr_ls, last) = List.split_n ls ((List.length ls) - 1) in
            let last = List.hd_exn last in
            let fold_step = fun acc expr -> acc ^ (compile expr) ^ ";\n" in
            let items = List.fold_left ~init:"" ~f:fold_step expr_ls in
            Printf.sprintf "(_ => {\n%s\nreturn %s\n})()" items (compile ~tc last)
    | {data = IdentExpr (UnresolvedIdent n); _} -> 
            get_ident n
    | {data = Prefix({op = Neg; rhs}); _} -> 
            Printf.sprintf "-%s" (compile rhs)
    | {data = Prefix({op = Not; rhs}); _} -> 
            Printf.sprintf "!%s" (compile rhs)
    | {data = Binary({op = Add; lhs; rhs}); _} -> 
            Printf.sprintf "(%s + %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = Neg; lhs; rhs}); _} -> 
            Printf.sprintf "(%s - %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = Mul; lhs; rhs}); _} -> 
            Printf.sprintf "(%s * %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = Div; lhs; rhs}); _} -> 
            Printf.sprintf "(%s / %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = EQ; lhs; rhs}); _} -> 
            Printf.sprintf "rustscript_equal(%s, %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = NEQ; lhs; rhs}); _} -> 
            Printf.sprintf "(%s !== %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = LEQ; lhs; rhs}); _} -> 
            Printf.sprintf "(%s <= %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = GEQ; lhs; rhs}); _} -> 
            Printf.sprintf "(%s >= %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = LT; lhs; rhs}); _} -> 
            Printf.sprintf "(%s < %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = GT; lhs; rhs}); _} -> 
            Printf.sprintf "(%s > %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = And; lhs; rhs}); _} -> 
            Printf.sprintf "(%s && %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = Or; lhs; rhs}); _} -> 
            Printf.sprintf "(%s || %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = Mod; lhs; rhs}); _} -> 
            Printf.sprintf "(%s %% %s)" (compile lhs) (compile rhs)
    | {data = Binary({op = PipeOp; lhs; rhs}); _} -> 
            if tc then
                Printf.sprintf "mk_thunk(%s, [%s])" (compile rhs) (compile lhs)
            else
                Printf.sprintf "unwrap_thunk(%s(%s))" (compile rhs) (compile lhs)
    | {data = Let l; _} -> 
            bind l.assignee l.assigned_expr ss
    | {data = TupleExpr ls; _} ->
        Printf.sprintf "[%s]" (String.concat ~sep:", " (List.map ~f:compile ls))
    | {data = LambdaCall {callee = UnresolvedIdent fn; call_args = {data = TupleExpr call_args; _}; _}; _} -> 
            let args = String.concat ~sep:", " (List.map ~f:compile call_args) in
            if tc then
                Printf.sprintf "mk_thunk(%s, [%s])" (get_ident fn) args
            else
                Printf.sprintf "unwrap_thunk(%s(%s))" (get_ident fn) args
    | {data = LambdaDef d; _} -> 
            let args = match d.lambda_def_args with
            | TuplePat args -> args
            | _ -> assert false
            in
            let args = String.concat ~sep:", " (List.map ~f:compile_pat args) in
            Printf.sprintf "((%s) => %s)" args (compile d.lambda_def_expr)
    | {data = FnDef {fn_name = UnresolvedIdent name; fn_def_func}; _} ->
            let fn_ident = new_ident name in
            let args = match fn_def_func.fn_args with
            | TuplePat args -> args
            | _ -> assert false
            in
            let args = String.concat ~sep:", " (List.map ~f:compile_pat args) in
            let body = compile_expr ~tc:true fn_def_func.fn_expr ss in
            Printf.sprintf "const %s = (%s) => %s" fn_ident args body
    | {data = IfExpr {cond; then_expr; else_expr}; _} ->
            Printf.sprintf "(_ => {if (%s) { return (_ => %s)() } else { return (_ => %s)() }})()" 
                            (compile cond) (compile ~tc then_expr) (compile ~tc else_expr)
    | {data = MatchExpr {match_val; match_arms}; location} ->
            let match_val_ident = new_ident "match_val" in
            Printf.sprintf "
            (() => {
                const %s = %s;
                if (false) {}
                %s
            })()
            "
            match_val_ident
            (compile match_val)
            (List.fold_left match_arms ~init:"" ~f:(fun acc (pat, arm_body, arm_guard) -> begin
                Printf.sprintf "
                %s
                else if ((%s) && (%s)) {
                    %s
                    return %s
                }
                "
                acc
                (pat_cond (locate location (IdentExpr (UnresolvedIdent "match_val"))) pat ~ss)
                (Option.value ~default:"true" (Option.map ~f:compile arm_guard))
                (bind pat (locate location (IdentExpr (UnresolvedIdent "match_val"))) ss)
                (compile arm_body)
            end))
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
    let shim = [%blob "shim.js"] in
    let lodash = [%blob "lodash.js"] in
    lodash ^ "\n" ^ shim ^ "\n" ^ List.fold_left ~init:"" ~f:fold_step expr_ls

let compile_file filename =
    let in_stream = Stdio.In_channel.create filename in
    let in_string = Stdio.In_channel.input_all in_stream in
    compile_str ~name:filename in_string
