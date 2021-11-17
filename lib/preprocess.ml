open Base
open Types

let inline_threshold = 3

let assoc_add ls x = 
    if List.Assoc.mem ls ~equal:String.equal x then
        ls
    else
        (x, List.length ls)::ls

type tree_node =
    | ExprNode of (expr Located.t)
    | PatNode of pattern

let unwrap_expr_node = function
    | ExprNode n -> n
    | _ -> assert false

let unwrap_pat_node = function
    | PatNode n -> n
    | _ -> assert false

let rec tree_fold_map: tree_node -> accumulator:'a -> f:('a -> tree_node -> (tree_node * 'a)) -> (tree_node * 'a) = 
    fun node ~accumulator ~f -> match node with
    | ExprNode ({data = Atomic _ | IdentExpr _ | UnresolvedAtom _; _}) ->
        f accumulator node
    | ExprNode (({data = Binary ({lhs; rhs; _} as b); _}) as located) ->
        let lhs, accumulator = tree_fold_map (ExprNode lhs) ~accumulator:accumulator ~f:f in
        let rhs, accumulator = tree_fold_map (ExprNode rhs) ~accumulator:accumulator ~f:f in
        let (lhs, rhs) = (unwrap_expr_node lhs), (unwrap_expr_node rhs) in
        f accumulator (ExprNode {located with data = Binary {b with lhs; rhs}})
    | ExprNode ({data = Prefix ({rhs; _} as p); _} as located) ->
        let rhs, accumulator = tree_fold_map (ExprNode rhs) ~accumulator:accumulator ~f:f in
        let rhs = unwrap_expr_node rhs in
        f accumulator (ExprNode {located with data = Prefix {p with rhs}})
    | ExprNode ({data = Let {assignee; assigned_expr}; location}) ->
        let assignee, accumulator = tree_fold_map (PatNode assignee) ~accumulator:accumulator ~f:f in
        let assigned_expr, accumulator = tree_fold_map (ExprNode assigned_expr) ~accumulator:accumulator ~f:f in
        let (assignee, assigned_expr) = unwrap_pat_node assignee, unwrap_expr_node assigned_expr in
        f accumulator (ExprNode {data = Let {assignee; assigned_expr}; location})
    | ExprNode ({data = LambdaDef {lambda_def_expr; lambda_def_args}; _} as located) ->
        let lambda_def_expr, accumulator = 
            tree_fold_map (ExprNode lambda_def_expr) ~accumulator:accumulator ~f:f 
        in
        let lambda_def_args, accumulator = 
            tree_fold_map (PatNode lambda_def_args) ~accumulator:accumulator ~f:f 
        in
        let (lambda_def_expr, lambda_def_args) = 
            (unwrap_expr_node lambda_def_expr, unwrap_pat_node lambda_def_args)
        in
        f accumulator (ExprNode {located with data = LambdaDef {lambda_def_expr; lambda_def_args}})
    | ExprNode ({data = LambdaCall ({call_args; _} as c); _} as located) ->
        let call_args, accumulator = tree_fold_map (ExprNode call_args) ~accumulator:accumulator ~f:f in
        let call_args = unwrap_expr_node call_args in
        f accumulator (ExprNode ({located with data = LambdaCall {c with call_args}}))
    | ExprNode ({data = LambdaCaptureExpr {capture_expr_args; capture_expr_fn}; _} as located) ->
        let step accumulator capture_arg = match capture_arg with
            | CaptureExprArg e ->
                let e, accumulator = tree_fold_map (ExprNode e) ~accumulator:accumulator ~f:f in
                accumulator, CaptureExprArg (unwrap_expr_node e)
            | _ -> accumulator, capture_arg
        in
        let accumulator, capture_expr_args = List.fold_map ~init:accumulator ~f:step capture_expr_args in
        f accumulator (ExprNode {located with data = LambdaCaptureExpr {capture_expr_args; capture_expr_fn}})
    | ExprNode ({data = FnDef ({fn_def_func = {fn_expr; fn_args}; _} as def); _} as located) ->
        let fn_expr, accumulator = tree_fold_map (ExprNode fn_expr) ~accumulator:accumulator ~f:f in
        let fn_args, accumulator = tree_fold_map (PatNode fn_args) ~accumulator:accumulator ~f:f in
        let (fn_expr, fn_args) = unwrap_expr_node fn_expr, unwrap_pat_node fn_args in
        let fn_def_func = {fn_expr; fn_args} in
        f accumulator (ExprNode ({located with data = FnDef {def with fn_def_func}}))
    | ExprNode ({data = IfExpr {cond; then_expr; else_expr}; _} as located) ->
        let cond, accumulator = tree_fold_map (ExprNode cond) ~accumulator:accumulator ~f:f in
        let then_expr, accumulator = tree_fold_map (ExprNode then_expr) ~accumulator:accumulator ~f:f in
        let else_expr, accumulator = tree_fold_map (ExprNode else_expr) ~accumulator:accumulator ~f:f in
        let (cond, then_expr, else_expr) = 
            (unwrap_expr_node cond, unwrap_expr_node then_expr, unwrap_expr_node else_expr) 
        in
        f accumulator (ExprNode ({located with data = IfExpr {cond; then_expr; else_expr}}))
    | ExprNode ({data = TupleExpr ls; _} as located) ->
        let step accumulator node =
            let mapped_node, accumulator = tree_fold_map (ExprNode node) ~accumulator:accumulator ~f:f in
            accumulator, unwrap_expr_node mapped_node
        in
        let accumulator, ls = List.fold_map ~init:accumulator ~f:step ls in
        f accumulator (ExprNode ({located with data = TupleExpr ls}))
    | ExprNode ({data = BlockExpr ls; _} as located) ->
        let step accumulator node =
            let mapped_node, accumulator = tree_fold_map (ExprNode node) ~accumulator:accumulator ~f:f in
            accumulator, unwrap_expr_node mapped_node
        in
        let accumulator, ls = List.fold_map ~init:accumulator ~f:step ls in
        f accumulator (ExprNode {located with data = BlockExpr ls})
    | ExprNode ({data = MatchExpr {match_val; match_arms}; _} as located) ->
        let match_val, accumulator = tree_fold_map (ExprNode match_val) ~accumulator:accumulator ~f:f in
        let match_val = unwrap_expr_node match_val in
        let step accumulator (match_pat, match_expr, match_cond_opt) =
            let match_pat, accumulator = tree_fold_map (PatNode match_pat) ~accumulator:accumulator ~f:f in
            let match_expr, accumulator = tree_fold_map (ExprNode match_expr) ~accumulator:accumulator ~f:f in
            let match_cond_opt, accumulator = match match_cond_opt with
                | Some c -> 
                    let match_cond, accumulator = tree_fold_map (ExprNode c) ~accumulator:accumulator ~f:f in
                    Some (unwrap_expr_node match_cond), accumulator
                | None -> None, accumulator
            in
            accumulator, (unwrap_pat_node match_pat, unwrap_expr_node match_expr, match_cond_opt)
        in
        let accumulator, match_arms = List.fold_map ~init:accumulator ~f:step match_arms in
        f accumulator (ExprNode {located with data = MatchExpr {match_val; match_arms}})
    | ExprNode ({data = MapExpr (pairs, tail_opt); _} as located) ->
        let step accumulator (k, v) =
            let k, accumulator = tree_fold_map (ExprNode k) ~accumulator:accumulator ~f:f in
            let v, accumulator = tree_fold_map (ExprNode v) ~accumulator:accumulator ~f:f in
            accumulator, (unwrap_expr_node k, unwrap_expr_node v)
        in
        let accumulator, pairs = List.fold_map ~init:accumulator ~f:step pairs in
        let tail_opt, accumulator = match tail_opt with
            | Some t -> 
                let tail, accumulator = tree_fold_map (ExprNode t) ~accumulator:accumulator ~f:f in
                Some (unwrap_expr_node tail), accumulator
            | None -> None, accumulator
        in
        f accumulator (ExprNode {located with data = MapExpr (pairs, tail_opt)})
    | ExprNode ({data = ListExpr (ls, tail_opt); _} as located) ->
        let step accumulator node =
            let mapped_node, accumulator = tree_fold_map (ExprNode node) ~accumulator:accumulator ~f:f in
            accumulator, (unwrap_expr_node mapped_node)
        in
        let accumulator, ls = List.fold_map ~init:accumulator ~f:step ls in
        let tail_opt, accumulator = match tail_opt with
            | Some t -> 
                let tail, accumulator = tree_fold_map (ExprNode t) ~accumulator:accumulator ~f:f in
                Some (unwrap_expr_node tail), accumulator
            | None -> None, accumulator
        in
        f accumulator (ExprNode {located with data = ListExpr (ls, tail_opt)})
    | PatNode (SinglePat _ | NumberPat _ | IntegerPat _ | StringPat _ | UnresolvedAtomPat _ | AtomPat _ | WildcardPat) ->
        f accumulator node
    | PatNode (TuplePat ls) ->
        let step accumulator node =
            let mapped_node, accumulator = tree_fold_map (PatNode node) ~accumulator:accumulator ~f:f in
            accumulator, (unwrap_pat_node mapped_node)
        in
        let accumulator, ls = List.fold_map ~init:accumulator ~f:step ls in
        f accumulator (PatNode (TuplePat ls))
    | PatNode (ListPat (FullPat ls)) ->
        let step accumulator node =
            let mapped_node, accumulator = tree_fold_map (PatNode node) ~accumulator:accumulator ~f:f in
            accumulator, (unwrap_pat_node mapped_node)
        in
        let accumulator, ls = List.fold_map ~init:accumulator ~f:step ls in
        f accumulator (PatNode (ListPat (FullPat ls)))
    | PatNode (ListPat (HeadTailPat (ls, tail))) ->
        let step accumulator node =
            let mapped_node, accumulator = tree_fold_map (PatNode node) ~accumulator:accumulator ~f:f in
            accumulator, (unwrap_pat_node mapped_node)
        in
        let accumulator, ls = List.fold_map ~init:accumulator ~f:step ls in
        let tail, accumulator = tree_fold_map (PatNode tail) ~accumulator:accumulator ~f:f in
        let tail = unwrap_pat_node tail in
        f accumulator (PatNode (ListPat (HeadTailPat (ls, tail))))
    | PatNode (MapPat pairs) ->
        let step accumulator (k, v) =
            let k, accumulator = tree_fold_map (ExprNode k) ~accumulator:accumulator ~f:f in
            let v, accumulator = tree_fold_map (PatNode v) ~accumulator:accumulator ~f:f in
            let (k, v) = unwrap_expr_node k, unwrap_pat_node v in
            accumulator, (k, v) 
        in
        let accumulator, pairs = List.fold_map ~init:accumulator ~f:step pairs in
        f accumulator (PatNode (MapPat pairs))
    | PatNode (OrPat (l, r)) ->
        let l, accumulator = tree_fold_map (PatNode l) ~accumulator:accumulator ~f:f in
        let r, accumulator = tree_fold_map (PatNode r) ~accumulator:accumulator ~f:f in
        let (l, r) = (unwrap_pat_node l), (unwrap_pat_node r) in
        f accumulator (PatNode (OrPat (l, r)))
    | PatNode (AsPat (p, s)) ->
        let p, accumulator = tree_fold_map (PatNode p) ~accumulator:accumulator ~f:f in
        let p = unwrap_pat_node p in
        f accumulator (PatNode (AsPat (p, s)))

let find_expr_atoms_step expr atoms = match (Located.extract expr) with
    | UnresolvedAtom s -> expr, assoc_add atoms s
    | _ -> expr, atoms

let find_pat_atoms_step pat atoms = match pat with
    | UnresolvedAtomPat s -> pat, assoc_add atoms s
    | _ -> pat, atoms

let find_atoms tree atoms =
    let fold_step atoms tree = match tree with
        | ExprNode e ->
            let e, atoms = find_expr_atoms_step e atoms in
            (ExprNode e), atoms
        | PatNode p -> 
            let p, atoms = find_pat_atoms_step p atoms in
            (PatNode p), atoms
    in
    let _, atoms = tree_fold_map tree ~accumulator:atoms ~f:fold_step in
    atoms

let resolve_atoms_step atoms node = match node with
    | ExprNode {data = UnresolvedAtom s; location} -> begin
        match List.Assoc.find atoms ~equal:String.equal s with
            | Some i -> ExprNode {data = Atomic (Atom i); location}, atoms
            | None ->
                Stdio.printf "Could not resolve atom :%s\n" s;
                Caml.exit 0
    end
    | PatNode (UnresolvedAtomPat s) -> begin
        match List.Assoc.find atoms ~equal:String.equal s with
            | Some i -> PatNode (AtomPat i), atoms
            | None ->
                Stdio.printf "Could not resolve atom :%s\n" s;
                Caml.exit 0
    end
    | _ -> node, atoms

let resolve_atoms tree atoms =
    let tree, _ = tree_fold_map tree ~accumulator:atoms ~f:resolve_atoms_step in
    tree

let find_idents_step idents node = match node with
    | ExprNode {data = IdentExpr (UnresolvedIdent s); _} ->
        node, assoc_add idents s
    | ExprNode {data = LambdaCall {callee = UnresolvedIdent s; _}; _} ->
        node, assoc_add idents s
    | ExprNode {data = FnDef {fn_name = UnresolvedIdent s; _}; _} ->
        node, assoc_add idents s
    | ExprNode {data = LambdaCaptureExpr {capture_expr_fn = UnresolvedIdent s; _}; _} ->
        node, assoc_add idents s
    | PatNode (SinglePat (UnresolvedIdent s)) ->
        node, assoc_add idents s
    | _ -> node, idents

let find_idents tree idents =
    let _, idents = tree_fold_map tree ~accumulator:idents ~f:find_idents_step in
    idents

let resolve_idents_step idents node = 
    let find = List.Assoc.find_exn idents ~equal:String.equal in
    match node with
    | ExprNode {data = IdentExpr (UnresolvedIdent s); location} ->
        let ident = ResolvedIdent (find s) in
        ExprNode {data = IdentExpr ident; location}, idents
    | ExprNode {data = LambdaCall ({callee = UnresolvedIdent s; _} as c); location} ->
        let ident = ResolvedIdent (find s) in
        ExprNode {data = LambdaCall {c with callee = ident}; location}, idents
    | ExprNode {data = FnDef ({fn_name = UnresolvedIdent s; _} as def); location} ->
        let ident = ResolvedIdent (find s) in
        ExprNode {data = FnDef {def with fn_name = ident}; location}, idents
    | ExprNode {data = LambdaCaptureExpr ({capture_expr_fn = UnresolvedIdent s; _} as capture); location} ->
        let ident = ResolvedIdent (find s) in
        ExprNode {data = LambdaCaptureExpr {capture with capture_expr_fn = ident}; location}, idents
    | PatNode (SinglePat (UnresolvedIdent s)) ->
        let ident = ResolvedIdent (find s) in
        PatNode (SinglePat ident), idents
    | _ -> node, idents

let resolve_idents tree idents =
    let tree, _ = tree_fold_map tree ~accumulator:idents ~f:resolve_idents_step in
    tree

(* To be replaced with a proper module system later *)
let resolve_imports_step () node = match node with
    | ExprNode {
        data = 
            LambdaCall {
                callee = UnresolvedIdent "import"; 
                call_args = 
                    {data = TupleExpr [{data = Atomic (StringVal filename); _}]; _}
            }; location} ->
            let in_stream = Stdio.In_channel.create filename in
            let in_string = Stdio.In_channel.input_all in_stream in
            let file_block, _ = Parser.parse_str ("{" ^ in_string ^ "}") filename in
            ExprNode file_block, ()
    | _ -> node, ()

let resolve_imports tree =
    let tree, _ = tree_fold_map tree ~accumulator:() ~f:resolve_imports_step in
    unwrap_expr_node tree

let find_expr_functions ss acc e = match e with
    | FnDef {fn_name = UnresolvedIdent fn_name; fn_def_func} -> 
        (fn_name |> List.Assoc.find_exn ss.static_idents ~equal:String.equal, fn_def_func)::acc
    | FnDef {fn_name = ResolvedIdent i; fn_def_func} ->
        (i, fn_def_func)::acc
    | _ -> acc

let find_block_funcs ss expr_ls acc =
    List.fold_left ~init:acc ~f:(find_expr_functions ss) expr_ls

let inline_functions_step static_funcs node = match node with
    | ExprNode {data = LambdaCall {callee = ResolvedIdent id; call_args}; location} ->
        let fn = List.Assoc.find static_funcs ~equal:Int.equal id in
        begin match fn with
            | None -> 
                node, static_funcs
            | Some {fn_expr = {data = BlockExpr ls; _}; _} when List.length ls > inline_threshold ->
                node, static_funcs
            | Some {fn_expr; fn_args} ->
                let e = BlockExpr [
                    Let {assignee = fn_args; assigned_expr = call_args} |> Located.locate location;
                    fn_expr
                ] |> Located.locate fn_expr.location
                in ExprNode e, static_funcs
        end
    | _ -> node, static_funcs

let inline_functions tree static_funcs = 
    let tree, _ = tree_fold_map tree ~accumulator:static_funcs ~f:inline_functions_step in
    tree

let clobbers_declared_fn_step (acc, static_funcs) node = match node with
    | PatNode (SinglePat (ResolvedIdent id)) ->
        let clobbers = List.Assoc.mem static_funcs ~equal:Int.equal id in
        node, (clobbers || acc, static_funcs)
    | _ ->
        node, (false || acc, static_funcs)

let clobbers_declared_fn_test tree static_funcs =
    let _, (result, _) = tree_fold_map tree ~accumulator:(false, static_funcs) ~f:clobbers_declared_fn_step in
    result
