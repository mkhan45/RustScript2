open Base
open Types

let rec find_pat_atoms pat atoms = match pat with
    | SinglePat _ | NumberPat _ | AtomPat _ | StringPat _ | WildcardPat -> atoms
    | TuplePat ls -> List.fold_left ~init:atoms ~f:(fun atoms pat -> find_pat_atoms pat atoms) ls
    | ListPat (FullPat ls) -> List.fold_left ~init:atoms ~f:(fun atoms pat -> find_pat_atoms pat atoms) ls
    | ListPat (HeadTailPat (ls, tail)) -> atoms |> find_pat_atoms (ListPat (FullPat ls)) |> find_pat_atoms tail
    | MapPat pairs -> List.fold_left ~init:atoms ~f:(fun a (e, p) -> a |> find_atoms e |> find_pat_atoms p) pairs
    | OrPat (l, r) -> atoms |> find_pat_atoms l |> find_pat_atoms r
    | AsPat (p, _) -> atoms |> find_pat_atoms p
    | UnresolvedAtomPat s -> match List.Assoc.find atoms ~equal:String.equal s with
        | Some _ -> atoms
        | None -> (s, List.length atoms)::atoms

and find_atoms: expr -> (string * int) list -> (string * int) list = 
    fun expr atoms -> match expr with
    | Binary b     -> atoms |> find_atoms b.lhs |> find_atoms b.rhs
    | Prefix p     -> atoms |> find_atoms p.rhs
    | Let l        -> atoms |> find_atoms l.assigned_expr |> find_pat_atoms l.assignee
    | FnDef d      -> atoms |> find_atoms d.fn_def_func.fn_expr |> find_pat_atoms d.fn_def_func.fn_args
    | LambdaDef d  -> atoms |> find_atoms d.lambda_def_expr |> find_pat_atoms d.lambda_def_args
    | LambdaCall c -> atoms |> find_atoms c.call_args
    | IfExpr i     -> atoms |> find_atoms i.cond |> find_atoms i.then_expr |> find_atoms i.else_expr
    | TupleExpr ls -> List.fold_left ~init:atoms ~f:(fun atoms e -> atoms |> find_atoms e) ls
    | BlockExpr ls -> List.fold_left ~init:atoms ~f:(fun atoms e -> atoms |> find_atoms e) ls
    | MatchExpr m  -> 
        let fold_step a (p, e, _9) = a |> find_pat_atoms p |> find_atoms e in (* TODO: potentially recurse on o *)
        let atoms = atoms |> find_atoms m.match_val in
        List.fold_left ~init:atoms ~f:fold_step m.match_arms
    | MapExpr (m, _)  -> List.fold_left ~init:atoms ~f:(fun atoms (a, b) -> atoms |> find_atoms a |> find_atoms b) m
    | ListExpr (ls, _) -> List.fold_left ~init:atoms ~f:(fun atoms e -> atoms |> find_atoms e) ls
    | UnresolvedAtom s -> begin match List.Assoc.find atoms ~equal:String.equal s with
        | Some _ -> atoms
        | None -> (s, List.length atoms)::atoms
    end
    | _ -> atoms

let rec resolve_pat_atoms ss p =
    let resolve = resolve_pat_atoms ss in
    let resolve_expr = resolve_atoms ss in
    match p with
    | SinglePat _ | NumberPat _ | AtomPat _ | WildcardPat | StringPat _ -> p
    | TuplePat ls -> TuplePat (List.map ~f:resolve ls)
    | ListPat (FullPat ls) ->  ListPat (FullPat (List.map ~f:resolve ls))
    | ListPat (HeadTailPat (ls, p)) -> ListPat (HeadTailPat (List.map ~f:resolve ls, resolve p))
    | MapPat pairs -> MapPat (List.map ~f:(fun (k, v) -> resolve_expr k, resolve v) pairs)
    | OrPat (l, r) -> OrPat (resolve l, resolve r)
    | AsPat (p, n) -> AsPat (resolve p, n)
    | UnresolvedAtomPat s -> AtomPat (List.Assoc.find_exn ss.static_atoms ~equal:String.equal s)

and resolve_atoms ss e = 
    let resolve = resolve_atoms ss in
    match e with
        | Atomic _ | Ident _ -> e
        | Binary b ->
            let lhs = resolve b.lhs in
            let rhs = resolve b.rhs in
            Binary { lhs; rhs; op = b.op }
        | Prefix p ->
            let rhs = resolve p.rhs in
            Prefix { rhs; op = p.op }
        | Let l ->
            let assignee = resolve_pat_atoms ss l.assignee in
            let assigned_expr = resolve l.assigned_expr in
            Let { assignee; assigned_expr }
        | LambdaDef d ->
            let lambda_def_expr = resolve d.lambda_def_expr in
            let lambda_def_args = resolve_pat_atoms ss d.lambda_def_args in
            LambdaDef { lambda_def_expr; lambda_def_args }
        | FnDef d ->
            let fn_expr = resolve d.fn_def_func.fn_expr in
            let fn_args = resolve_pat_atoms ss d.fn_def_func.fn_args in
            FnDef { d with fn_def_func = {fn_expr; fn_args} }
        | LambdaCall c ->
            let call_args = resolve c.call_args in
            LambdaCall { call_args; callee = c.callee }
        | IfExpr i ->
            let cond = resolve i.cond in
            let then_expr = resolve i.then_expr in
            let else_expr = resolve i.else_expr in
            IfExpr { cond; then_expr; else_expr }
        | TupleExpr ls ->
            TupleExpr (List.map ~f:resolve ls)
        | BlockExpr ls ->
            BlockExpr (List.map ~f:resolve ls)
        | MatchExpr m ->
            let match_val = resolve m.match_val in
            let match_arms = 
                List.map 
                ~f:(fun (p, a, b) -> (resolve_pat_atoms ss p, resolve a, Option.map ~f:resolve b)) 
                m.match_arms 
            in
            MatchExpr { match_val; match_arms }
        | MapExpr (pairs, tail) ->
            let pairs = List.map ~f:(fun (a, b) -> (resolve a, resolve b)) pairs in
            let tail = Option.map ~f:resolve tail in
            MapExpr (pairs, tail)
        | ListExpr (ls, tail) ->
            let ls = List.map ~f:resolve ls in
            let tail = Option.map ~f:resolve tail in
            ListExpr (ls, tail)
        | UnresolvedAtom s -> Atomic (Atom (List.Assoc.find_exn ss.static_atoms ~equal:String.equal s))

let find_expr_functions acc e = match e with
    | FnDef d -> (d.fn_name, d.fn_def_func)::acc
    | _ -> acc

let find_block_funcs expr_ls acc =
    List.fold_left ~init:acc ~f:find_expr_functions expr_ls
