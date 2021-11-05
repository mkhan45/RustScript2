open Base
open Types

let assoc_add ls x = 
    if List.Assoc.mem ls ~equal:String.equal x then
        ls
    else
        (x, List.length ls)::ls

let rec find_pat_atoms pat atoms = match pat with
    | SinglePat _ | NumberPat _ | IntegerPat _ | AtomPat _ | StringPat _ | WildcardPat  -> atoms
    | TuplePat ls -> List.fold_left ~init:atoms ~f:(fun atoms pat -> find_pat_atoms pat atoms) ls
    | ListPat (FullPat ls) -> List.fold_left ~init:atoms ~f:(fun atoms pat -> find_pat_atoms pat atoms) ls
    | ListPat (HeadTailPat (ls, tail)) -> atoms |> find_pat_atoms (ListPat (FullPat ls)) |> find_pat_atoms tail
    | MapPat pairs -> List.fold_left ~init:atoms ~f:(fun a (e, p) -> a |> find_atoms e.data |> find_pat_atoms p) pairs
    | OrPat (l, r) -> atoms |> find_pat_atoms l |> find_pat_atoms r
    | AsPat (p, _) -> atoms |> find_pat_atoms p
    | UnresolvedAtomPat s -> assoc_add atoms s

and find_atoms: expr -> (string * int) list -> (string * int) list = 
    fun expr atoms -> match expr with
    | Binary b     -> atoms |> find_atoms b.lhs.data |> find_atoms b.rhs.data
    | Prefix p     -> atoms |> find_atoms p.rhs.data
    | Let l        -> atoms |> find_atoms l.assigned_expr.data |> find_pat_atoms l.assignee
    | FnDef d      -> atoms |> find_atoms d.fn_def_func.fn_expr.data |> find_pat_atoms d.fn_def_func.fn_args
    | LambdaDef d  -> atoms |> find_atoms d.lambda_def_expr.data |> find_pat_atoms d.lambda_def_args
    | LambdaCall c -> atoms |> find_atoms c.call_args.data
    | IfExpr i     -> atoms |> find_atoms i.cond.data |> find_atoms i.then_expr.data |> find_atoms i.else_expr.data
    | TupleExpr ls -> List.fold_left ~init:atoms ~f:(fun atoms e -> atoms |> find_atoms e.data) ls
    | BlockExpr ls -> List.fold_left ~init:atoms ~f:(fun atoms e -> atoms |> find_atoms e.data) ls
    | MatchExpr m  -> 
        let fold_step a (p, e, _o) = 
            a |> find_pat_atoms p |> find_atoms (Located.extract e) 
        in
        let atoms = atoms |> find_atoms m.match_val.data in
        List.fold_left ~init:atoms ~f:fold_step m.match_arms
    | MapExpr (m, _)  -> 
        List.fold_left ~init:atoms ~f:(fun atoms (a, b) -> atoms |> find_atoms a.data |> find_atoms b.data) m
    | ListExpr (ls, _) -> List.fold_left ~init:atoms ~f:(fun atoms e -> atoms |> find_atoms e.data) ls
    | UnresolvedAtom s -> assoc_add atoms s
    | _ -> atoms

let rec resolve_pat_atoms ss p =
    let resolve = resolve_pat_atoms ss in
    let resolve_expr = resolve_atoms ss in
    match p with
    | SinglePat _ | NumberPat _ | IntegerPat _ | AtomPat _ | WildcardPat | StringPat _ -> p
    | TuplePat ls -> TuplePat (List.map ~f:resolve ls)
    | ListPat (FullPat ls) ->  ListPat (FullPat (List.map ~f:resolve ls))
    | ListPat (HeadTailPat (ls, p)) -> ListPat (HeadTailPat (List.map ~f:resolve ls, resolve p))
    | MapPat pairs -> MapPat (List.map ~f:(fun (k, v) -> resolve_expr k, resolve v) pairs)
    | OrPat (l, r) -> OrPat (resolve l, resolve r)
    | AsPat (p, n) -> AsPat (resolve p, n)
    | UnresolvedAtomPat s -> AtomPat (List.Assoc.find_exn ss.static_atoms ~equal:String.equal s)

and resolve_atoms: static_state -> expr Located.t -> expr Located.t = fun ss e ->
    let open Types.Located in
    let resolve = resolve_atoms ss in
    match e with
        | {data = (Atomic _ | IdentExpr _); _} -> e
        | {data = Binary b; location} ->
            let lhs = resolve b.lhs in
            let rhs = resolve b.rhs in
            Binary { lhs; rhs; op = b.op } |> locate location
        | {data = Prefix p; location} ->
            let rhs = resolve p.rhs in
            Prefix { rhs; op = p.op } |> locate location
        | {data = Let l; location} ->
            let assignee = resolve_pat_atoms ss l.assignee in
            let assigned_expr = resolve l.assigned_expr in
            Let { assignee; assigned_expr } |> locate location
        | {data = LambdaDef d; location} ->
            let lambda_def_expr = resolve d.lambda_def_expr in
            let lambda_def_args = resolve_pat_atoms ss d.lambda_def_args in
            LambdaDef { lambda_def_expr; lambda_def_args } |> locate location
        | {data = FnDef d; location} ->
            let fn_expr = resolve d.fn_def_func.fn_expr in
            let fn_args = resolve_pat_atoms ss d.fn_def_func.fn_args in
            FnDef { d with fn_def_func = {fn_expr; fn_args} } |> locate location
        | {data = LambdaCall c; location} ->
            let call_args = resolve c.call_args in
            LambdaCall { call_args; callee = c.callee } |> locate location
        | {data = IfExpr i; location} ->
            let cond = resolve i.cond in
            let then_expr = resolve i.then_expr in
            let else_expr = resolve i.else_expr in
            IfExpr { cond; then_expr; else_expr } |> locate location
        | {data = TupleExpr ls; location} ->
            TupleExpr (List.map ~f:resolve ls) |> locate location
        | {data = BlockExpr ls; location} ->
            BlockExpr (List.map ~f:resolve ls) |> locate location
        | {data = MatchExpr m; location} ->
            let match_val = resolve m.match_val in
            let match_arms = 
                List.map 
                ~f:(fun (p, a, b) -> (resolve_pat_atoms ss p, resolve a, Option.map ~f:resolve b)) 
                m.match_arms 
            in
            MatchExpr { match_val; match_arms } |> locate location
        | {data = MapExpr (pairs, tail); location} ->
            let pairs = List.map ~f:(fun (a, b) -> (resolve a, resolve b)) pairs in
            let tail = Option.map ~f:resolve tail in
            MapExpr (pairs, tail) |> locate location
        | {data = ListExpr (ls, tail); location} ->
            let ls = List.map ~f:resolve ls in
            let tail = Option.map ~f:resolve tail in
            ListExpr (ls, tail) |> locate location
        | {data = UnresolvedAtom s; location} -> 
            match List.Assoc.find ss.static_atoms ~equal:String.equal s with
                | Some i -> (Atomic (Atom i)) |> locate location
                | None ->
                    Stdio.printf "Could not resolve atom :%s\n" s;
                    Caml.exit 0

let rec find_pat_idents pat idents = match pat with
    | SinglePat (UnresolvedIdent s) -> assoc_add idents s
    | NumberPat _ | AtomPat _ | StringPat _ | WildcardPat -> idents
    | TuplePat ls -> List.fold_left ~init:idents ~f:(fun idents pat -> find_pat_idents pat idents) ls
    | ListPat (FullPat ls) -> List.fold_left ~init:idents ~f:(fun idents pat -> find_pat_idents pat idents) ls
    | ListPat (HeadTailPat (ls, tail)) -> idents |> find_pat_idents (ListPat (FullPat ls)) |> find_pat_idents tail
    | MapPat pairs -> List.fold_left ~init:idents ~f:(fun a (e, p) -> a |> find_idents e.data |> find_pat_idents p) pairs
    | OrPat (l, r) -> idents |> find_pat_idents l |> find_pat_idents r
    | AsPat (p, _) -> idents |> find_pat_idents p
    | _ -> idents

and find_idents: expr -> (string * int) list -> (string * int) list =
    fun expr idents -> match expr with
    | IdentExpr (UnresolvedIdent s)  -> assoc_add idents s
    | Binary b     -> idents |> find_idents b.lhs.data |> find_idents b.rhs.data
    | Prefix p     -> idents |> find_idents p.rhs.data
    | Let l        -> idents |> find_idents l.assigned_expr.data |> find_pat_idents l.assignee
    | FnDef d      -> 
            idents 
            |> find_pat_idents (SinglePat d.fn_name)
            |> find_idents d.fn_def_func.fn_expr.data 
            |> find_pat_idents d.fn_def_func.fn_args
    | LambdaDef d  -> idents |> find_idents d.lambda_def_expr.data |> find_pat_idents d.lambda_def_args
    | LambdaCall {callee = UnresolvedIdent s; call_args } -> 
            let idents = idents |> find_idents call_args.data in
            assoc_add idents s
    | LambdaCall {call_args; _} -> idents |> find_idents call_args.data
    | IfExpr i     -> idents |> find_idents i.cond.data |> find_idents i.then_expr.data |> find_idents i.else_expr.data
    | TupleExpr ls -> List.fold_left ~init:idents ~f:(fun idents e -> idents |> find_idents e.data) ls
    | BlockExpr ls -> List.fold_left ~init:idents ~f:(fun idents e -> idents |> find_idents e.data) ls
    | MatchExpr m  -> 
        let fold_step a (p, e, _o) = 
            a |> find_pat_idents p |> find_idents (Located.extract e) 
        in
        let idents = idents |> find_idents m.match_val.data in
        List.fold_left ~init:idents ~f:fold_step m.match_arms
    | MapExpr (m, _)  -> 
        List.fold_left ~init:idents ~f:(fun idents (a, b) -> idents |> find_idents a.data |> find_idents b.data) m
    | ListExpr (ls, _) -> List.fold_left ~init:idents ~f:(fun idents e -> idents |> find_idents e.data) ls
    | _ -> idents

let rec resolve_pat_idents ss p =
    let resolve = resolve_pat_idents ss in
    let resolve_expr = resolve_idents ss in
    match p with
    | SinglePat (UnresolvedIdent s) -> 
        SinglePat (ResolvedIdent (List.Assoc.find_exn ss.static_idents ~equal:String.equal s))
    | SinglePat (ResolvedIdent _) -> p
    | NumberPat _ | IntegerPat _ | AtomPat _ | WildcardPat | StringPat _ -> p
    | TuplePat ls -> TuplePat (List.map ~f:resolve ls)
    | ListPat (FullPat ls) ->  ListPat (FullPat (List.map ~f:resolve ls))
    | ListPat (HeadTailPat (ls, p)) -> ListPat (HeadTailPat (List.map ~f:resolve ls, resolve p))
    | MapPat pairs -> MapPat (List.map ~f:(fun (k, v) -> resolve_expr k, resolve v) pairs)
    | OrPat (l, r) -> OrPat (resolve l, resolve r)
    | AsPat (p, n) -> AsPat (resolve p, n)
    | UnresolvedAtomPat s -> AtomPat (List.Assoc.find_exn ss.static_idents ~equal:String.equal s)

and resolve_idents: static_state -> expr Located.t -> expr Located.t = fun ss e ->
    let open Types.Located in
    let resolve = resolve_idents ss in
    match e with
        | {data = IdentExpr (UnresolvedIdent s); location} ->
            (IdentExpr (ResolvedIdent (List.Assoc.find_exn ss.static_idents ~equal:String.equal s))) |> locate location
        | {data = IdentExpr (ResolvedIdent _); _} -> e
        | {data = Atomic _; _} -> e
        | {data = Binary b; location} ->
            let lhs = resolve b.lhs in
            let rhs = resolve b.rhs in
            Binary { lhs; rhs; op = b.op } |> locate location
        | {data = Prefix p; location} ->
            let rhs = resolve p.rhs in
            Prefix { rhs; op = p.op } |> locate location
        | {data = Let l; location} ->
            let assignee = resolve_pat_idents ss l.assignee in
            let assigned_expr = resolve l.assigned_expr in
            Let { assignee; assigned_expr } |> locate location
        | {data = LambdaDef d; location} ->
            let lambda_def_expr = resolve d.lambda_def_expr in
            let lambda_def_args = resolve_pat_idents ss d.lambda_def_args in
            LambdaDef { lambda_def_expr; lambda_def_args } |> locate location
        | {data = FnDef d; location} ->
            let fn_name = match d.fn_name with
                | UnresolvedIdent n -> begin
                    match List.Assoc.find ss.static_idents ~equal:String.equal n with
                        | Some i -> ResolvedIdent i
                        | None ->
                            Stdio.printf "Error resolving ident %s\n" n;
                            Caml.exit 0
                end
                | ResolvedIdent _ -> d.fn_name
            in
            let fn_expr = resolve d.fn_def_func.fn_expr in
            let fn_args = resolve_pat_idents ss d.fn_def_func.fn_args in
            FnDef { fn_name; fn_def_func = {fn_expr; fn_args} } |> locate location
        | {data = LambdaCall c; location} ->
            let callee = match c.callee with
                | ResolvedIdent _ -> c.callee
                | UnresolvedIdent s -> ResolvedIdent (List.Assoc.find_exn ss.static_idents ~equal:String.equal s)
            in
            let call_args = resolve c.call_args in
            LambdaCall { call_args; callee } |> locate location
        | {data = IfExpr i; location} ->
            let cond = resolve i.cond in
            let then_expr = resolve i.then_expr in
            let else_expr = resolve i.else_expr in
            IfExpr { cond; then_expr; else_expr } |> locate location
        | {data = TupleExpr ls; location} ->
            TupleExpr (List.map ~f:resolve ls) |> locate location
        | {data = BlockExpr ls; location} ->
            BlockExpr (List.map ~f:resolve ls) |> locate location
        | {data = MatchExpr m; location} ->
            let match_val = resolve m.match_val in
            let match_arms = 
                List.map 
                ~f:(fun (p, a, b) -> (resolve_pat_idents ss p, resolve a, Option.map ~f:resolve b)) 
                m.match_arms 
            in
            MatchExpr { match_val; match_arms } |> locate location
        | {data = MapExpr (pairs, tail); location} ->
            let pairs = List.map ~f:(fun (a, b) -> (resolve a, resolve b)) pairs in
            let tail = Option.map ~f:resolve tail in
            MapExpr (pairs, tail) |> locate location
        | {data = ListExpr (ls, tail); location} ->
            let ls = List.map ~f:resolve ls in
            let tail = Option.map ~f:resolve tail in
            ListExpr (ls, tail) |> locate location
        | {data = UnresolvedAtom s; location} -> 
            Atomic (Atom (List.Assoc.find_exn ss.static_idents ~equal:String.equal s)) |> locate location

let find_expr_functions ss acc e = match e with
    | FnDef {fn_name = UnresolvedIdent fn_name; fn_def_func} -> 
        (fn_name |> List.Assoc.find_exn ss.static_idents ~equal:String.equal, fn_def_func)::acc
    | FnDef {fn_name = ResolvedIdent i; fn_def_func} ->
        (i, fn_def_func)::acc
    | _ -> acc

let find_block_funcs ss expr_ls acc =
    List.fold_left ~init:acc ~f:(find_expr_functions ss) expr_ls
