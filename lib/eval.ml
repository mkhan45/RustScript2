open Types
open Stdio
open Base
open Operators

(* Throughout, static state is abbreviated as ss *)

let rec bind lhs rhs ss loc = 
    let bind lhs rhs = bind lhs rhs ss loc in
    let pattern_matches lhs rhs = pattern_matches lhs rhs ss in
    (* printf "Binding %s to %s\n" (string_of_pat lhs) (string_of_val rhs); *)
    match lhs, rhs with
    | SinglePat s, _ -> fun state ->
            Map.set state ~key:s ~data:rhs;
    | NumberPat lhs, Number rhs when Float.equal lhs rhs -> 
            fun state -> state
    | StringPat lhs, StringVal rhs when String.equal lhs rhs ->
            fun state -> state
    | AtomPat lhs, Atom rhs when Int.equal lhs rhs ->
            fun state -> state
    | OrPat (l, r), _ -> fun state ->
            if (pattern_matches l rhs state) then (bind l rhs) state else (bind r rhs) state
    | AsPat (pat, n), _ -> fun state ->
            let state = bind (SinglePat n) rhs state in
            bind pat rhs state
    | ((TuplePat lhs_ls) as lhs, ((Tuple rhs_ls) as rhs))|
      ((ListPat (FullPat lhs_ls)) as lhs, ((ValList rhs_ls) as rhs)) -> fun state -> begin
            (* TODO: Look into moving the closure inwards, moving some "runtime" computation to "comptime" *)
            match List.zip lhs_ls rhs_ls with
                | Ok zipped -> 
                    List.fold_left ~init:state ~f:(fun state (k, v) -> (bind k v) state) zipped
                | _ ->
                    printf "\n";
                    printf "Error at %s, Tried to bind %s of len %d to %s of len %d\n"
                        (location_to_string loc)
                        (string_of_pat lhs) (List.length lhs_ls)
                        (string_of_val ss rhs) (List.length rhs_ls);
                    Caml.exit 0
      end
    | (ListPat (HeadTailPat (head_pat_ls, tail_pat))), ValList rhs_ls -> fun s ->
            let (head_ls, tail_ls) = List.split_n rhs_ls (List.length head_pat_ls) in
            let s = (bind (ListPat (FullPat head_pat_ls)) (ValList head_ls)) s in
            let s = (bind tail_pat (ValList tail_ls)) s in
            s
    | MapPat kv_pairs, Dictionary rhs -> fun s ->
        let fetched_pairs = kv_pairs
            |> List.map ~f:(fun (k, v) -> let ev_k, _ = (eval_expr k ss) s in ev_k, v)
            |> List.map ~f:(fun (k, v) -> dict_get rhs k ss, v)
        in
        let fold_step state (k, v) = (bind v k) state in
        List.fold_left ~init:s ~f:fold_step fetched_pairs
    | WildcardPat, _ -> fun state -> state
    | _ -> 
        printf "Error at %s, Tried to bind %s to %s"
            (location_to_string loc)
            (string_of_pat lhs)
            (string_of_val ss rhs);
        Caml.exit 0

and dict_get dict key ss =
    (* Can probably be replaced by Base.Option functions *)
    match Map.find dict (hash_value key) with
        | Some found_values ->
            let res = List.Assoc.find found_values ~equal:(fun a b -> val_eq_bool a b ss) key in
            Option.value ~default:(Tuple []) res
        | _ -> Tuple []

and list_equal_len lhs rhs = match lhs, rhs with
    | [], [] -> true
    | [], _ | _, [] -> false
    | _::xs, _::ys -> list_equal_len xs ys

and pattern_matches pat value ss state =
    let pattern_matches pat value = pattern_matches pat value ss in
    let eval_expr expr ?tc:(tc=false) = eval_expr expr ss ~tc:tc in
    match pat, value with
        | WildcardPat, _ -> true
        | SinglePat _, _ -> true
        | AsPat (pat, _), _ -> pattern_matches pat value state
        | OrPat (lhs, rhs), value -> (pattern_matches lhs value state) || (pattern_matches rhs value state)
        | NumberPat lhs, Number rhs -> Float.equal lhs rhs
        | StringPat lhs, StringVal rhs -> String.equal lhs rhs
        | AtomPat lhs, Atom rhs -> Int.equal lhs rhs
        | ((TuplePat lhs_ls), (Tuple rhs_ls))|(ListPat (FullPat lhs_ls), ValList rhs_ls) ->
            if list_equal_len lhs_ls rhs_ls then
                let zipped = List.zip_exn lhs_ls rhs_ls in
                List.for_all ~f:(fun (p, v) -> pattern_matches p v state) zipped
            else false
        | (ListPat (HeadTailPat (head_pat_ls, tail_pat)), ValList rhs_ls) ->
            let (head_ls, tail_ls) = List.split_n rhs_ls (List.length head_pat_ls) in
            let head_matches = pattern_matches (ListPat (FullPat head_pat_ls)) (ValList head_ls) state in
            let tail_matches = pattern_matches tail_pat (ValList tail_ls) state in
            head_matches && tail_matches
        | (MapPat kv_pairs, Dictionary rhs) ->
            let fetched_pairs = kv_pairs
                |> List.map ~f:(fun (k, v) -> let ev_k, _ = (eval_expr k) state in ev_k, v)
                |> List.map ~f:(fun (k, v) -> dict_get rhs k ss, v)
            in
            List.for_all ~f:(fun (k, v) -> pattern_matches v k state) fetched_pairs
        | _ -> false

and inspect_builtin (args, state) ss =
    match args with
        | Tuple [v] ->
            printf "%s\n" (string_of_val ss v);
            v, state
        | _ ->
            printf "Expected only one argument to inspect";
            assert false

and print_builtin (args, state) _ss loc =
    match args with
        | Tuple [StringVal s] ->
            s |> escape_string |> printf "%s";
            Stdio.Out_channel.flush Stdio.stdout;
            (Tuple [], state)
        | _ ->
            printf "Expected one string argument to print at %s\n" (location_to_string loc);
            Caml.exit 0

(* Better as a builtin since concatenating strings is expensive *)
and println_builtin (args, state) _ss loc =
    match args with
        | Tuple [StringVal s] ->
            s |> escape_string |> printf "%s\n";
            Stdio.Out_channel.flush Stdio.stdout;
            (Tuple [], state)
        | _ ->
            printf "Expected one string argument to println at %s\n" (location_to_string loc);
            Caml.exit 0

and to_string_builtin (args, state) ss loc =
    match args with
        | Tuple [v] ->
            (StringVal (string_of_val ss v)), state
        | _ ->
            printf "Expected only one argument to to_string at %s\n" (location_to_string loc);
            Caml.exit 0

and scanln_builtin (args, state) _ss loc =
    match args with
        | Tuple [] -> begin
            match Stdio.In_channel.input_line ~fix_win_eol:true Stdio.stdin with
            | Some line -> (StringVal line), state
            | None -> Tuple [], state
        end
        | _ ->
            printf "Expected () as an argument to scan_line at %s\n" (location_to_string loc);
            Caml.exit 0

and range_builtin (args, state) =
    match args with
        | Tuple [Number start; Number end_; Number step] 
        when (Caml.Float.is_integer start) && (Caml.Float.is_integer end_) && (Caml.Float.is_integer step) ->
            let caml_ls = List.range (Float.to_int start) (Float.to_int end_) ~stride:(Float.to_int step) in
            let val_ls = List.map ~f:(fun n -> Number (Int.to_float n)) caml_ls in
            ValList val_ls, state
        | _ ->
            printf "Expected three integer arguments to range_step";
            assert false

and fold_builtin (args, state) ss loc =
    match args with
        | Tuple [init; Lambda fn; ValList ls] ->
            let call_fn = fun args ->
                let lambda_call = Thunk {thunk_fn = fn; thunk_args= args; thunk_fn_name = ""} in
                let res, _ = unwrap_thunk lambda_call state ss loc in
                res
            in
            let fold_result = 
                List.fold 
                ~init:init
                ~f:(fun acc v -> call_fn (Tuple [acc; v]))
                ls
            in
            fold_result, state
        | _ ->
            printf "Expected (init, fn, ls) as arguments to fold\n";
            assert false

and to_charlist_builtin (args, state) _ss =
    match args with
        | Tuple [StringVal s] ->
            let chars = String.to_list s in
            let char_strs = List.map ~f:String.of_char chars in
            let val_ls = List.map ~f:(fun c -> StringVal c) char_strs in
            ValList val_ls, state
        | _ ->
            printf "Expected a single string argument to to_charlist";
            assert false

and eval_op op lhs rhs ss = fun s ->
    let (lhs, s) = (eval_expr lhs ss) s in
    let (rhs, s) = (eval_expr rhs ss) s in
    op lhs rhs ss, s
    
and eval_prefix_op op rhs ss = fun s ->
    let (rhs, s) = (eval_expr rhs ss) s in
    op rhs ss, s

and eval_ident name loc = fun state ->
    match Map.find state name with
        | Some value -> value, state
        | None ->
            printf "Error at %s: variable not found: %s\n" (location_to_string loc) name;
            Caml.exit 0

and eval_let lhs rhs ss loc = fun state ->
    let (evaled, new_state) = (eval_expr rhs ss) state in
    let new_state = (bind lhs evaled ss loc) new_state in
    (Tuple [], new_state)

and eval_fn_def name {fn_expr; fn_args} ss loc = fun state ->
    let fn = Fn {fn_expr; fn_args} in
    let new_state = (bind (SinglePat name) fn ss loc) state in
    (Tuple [], new_state)

and eval_lambda_def e args =
    fun s -> (Lambda {lambda_expr = e; lambda_args = args; enclosed_state = s}), s

and unwrap_thunk thunk state ss loc = match thunk with
    | Thunk {thunk_fn = thunk_fn; thunk_args = thunk_args; thunk_fn_name = thunk_fn_name} ->
            let inner_state = (bind thunk_fn.lambda_args thunk_args ss loc) thunk_fn.enclosed_state in
            let inner_state = Map.set inner_state ~key:thunk_fn_name ~data:(Lambda thunk_fn) in
            let (new_thunk, _) = (eval_expr ~tc:true thunk_fn.lambda_expr ss) inner_state in
            unwrap_thunk new_thunk state ss loc
    | value -> value, state

and eval_lambda_call ?tc:(tail_call=false) call ss loc =
    fun (state: state) -> match Map.find state call.callee with
        | Some(Lambda lambda_val) ->
            let (evaled, _) = (eval_expr call.call_args ss) state in
            let thunk = Thunk {thunk_fn = lambda_val; thunk_args = evaled; thunk_fn_name = call.callee} in
            if tail_call then 
                (thunk, state)
            else 
                let res, _ = unwrap_thunk thunk state ss loc in
                (res, state)
        | Some(Fn fn_val) ->
            let (evaled, _) = (eval_expr call.call_args ss) state in
            let block_funcs = List.Assoc.map ss.static_block_funcs ~f:(fun f -> Fn f) in
            let enclosed_state = Map.of_alist_exn (module String) block_funcs in
            let pseudo_lambda = 
                {lambda_expr = fn_val.fn_expr; lambda_args = fn_val.fn_args; enclosed_state }
            in
            let thunk = Thunk {thunk_fn = pseudo_lambda; thunk_args = evaled; thunk_fn_name = call.callee } in
            if tail_call then
                (thunk, state)
            else
                let res, _ = unwrap_thunk thunk state ss loc in
                (res, state)
        | Some(Dictionary dict) ->
            let (evaled, state) = (eval_expr call.call_args ss) state in begin
                match evaled with
                    | Tuple [key] -> dict_get dict key ss, state
                    | _ ->
                        printf "Expected a single key\n";
                        assert false
            end
        | None -> begin
            match call.callee with
                | "inspect" -> inspect_builtin ((eval_expr call.call_args ss) state) ss
                | "print" -> print_builtin ((eval_expr call.call_args ss) state) ss loc
                | "println" -> println_builtin ((eval_expr call.call_args ss) state) ss loc
                | "scanln" -> scanln_builtin ((eval_expr call.call_args ss) state) ss loc
                | "to_string" -> to_string_builtin ((eval_expr call.call_args ss) state) ss loc
                | "range_step" -> range_builtin ((eval_expr call.call_args ss) state)
                | "fold" -> fold_builtin ((eval_expr call.call_args ss) state) ss loc
                | "to_charlist" -> to_charlist_builtin ((eval_expr call.call_args ss) state) ss
                | "get" ->
                    let (args, state) = (eval_expr call.call_args ss) state in begin
                    match args with
                        | Tuple [Dictionary m; key] -> begin
                            match Map.find m (hash_value key) with
                                | Some found_values -> 
                                    let res = List.Assoc.find found_values ~equal:(fun a b -> val_eq_bool a b ss) key in
                                    let v = Option.value ~default:(Tuple []) res in
                                    v, state
                                | None -> (Tuple [], state)
                            end
                        | _ ->
                            printf "get requires two arguments, a list, and a value";
                            assert false
                    end
                | _ ->
                    printf "Error: function %s not found at %s\n" call.callee (location_to_string loc);
                    Caml.exit 0
        end
        | _ ->
            printf "Tried to call %s at %s\n" call.callee (location_to_string loc);
            Caml.exit 0

and eval_tuple_expr ls ss state =
    let (eval_ls, state) =
        List.fold_left 
            ~init:([], state) 
            ~f:(fun (acc, s) e -> let (ev, s) = (eval_expr e ss) s in (ev::acc, s))
            ls
    in
    Tuple (List.rev eval_ls), state

and eval_if_expr ?tc:(tail_call=false) if_expr ss = fun state ->
    match (eval_expr if_expr.cond ss) state with
        | Boolean true, state -> (eval_expr ~tc:tail_call if_expr.then_expr ss) state
        | Boolean false, state -> 
                (eval_expr ~tc:tail_call if_expr.else_expr ss) state
        | _ -> assert false

and eval_block_expr ?tc:(tail_call=false) ls ss =
    let static_block_funcs = Preprocess.find_block_funcs (ls |> List.map ~f:Located.extract) ss.static_block_funcs in
    let ss = { ss with static_block_funcs } in
    fun state ->
        let (res, _) =
            let len = List.length ls in
            match List.split_n ls (len - 1) with
                | exprs, [last_expr] ->
                    let block_state =
                        List.fold_left 
                            ~init:state 
                            ~f:(fun line_state e -> let _, s = (eval_expr e ss) line_state in s) 
                            exprs
                    in
                    (eval_expr ~tc:tail_call last_expr ss) block_state
                | _ -> assert false (* Unreachable *)
        in (res, state)
    
and eval_match_expr ?tc:(tail_call=false) match_val match_arms ss loc state =
    let (match_val, state) = (eval_expr match_val ss) state in
    let eval_expr expr ?tc:(tc=false) = eval_expr expr ss ~tc:tc in
    let bind lhs rhs = bind lhs rhs ss in
    let result_state_opt = List.find_map ~f:(
        fun (pat, arm_expr, cond) -> 
            if pattern_matches pat match_val ss state then
                match cond with
                    | Some cond ->
                        let inner_state = (bind pat match_val loc) state in
                        let cond_eval, inner_state = (eval_expr cond) inner_state in
                        if val_is_true cond_eval ss then
                            let (result, _) = (eval_expr ~tc:tail_call arm_expr) inner_state in
                            Some (result, state)
                        else
                            None
                    | None ->
                        let inner_state = (bind pat match_val loc) state in
                        let (result, _) = (eval_expr ~tc:tail_call arm_expr) inner_state in
                        Some(result, state)
            else
                None
    ) match_arms
    in
    match result_state_opt with
        | Some((result, state)) -> 
            result, state
        | None ->
            printf "No patterns matched in match expression at %s\n" (location_to_string loc);
            Caml.exit 0

and eval_map_expr ?tc:(_tail_call=false) map_pairs tail_map ss state =
    let fold_fn = fun (map_acc, state) (key_expr, val_expr) ->
        let key_val, state = (eval_expr key_expr ss) state in
        let data_val, state = (eval_expr val_expr ss) state in
        let key_hash = hash_value key_val in
        let new_data = match Map.find map_acc key_hash with
            | Some assoc_list -> List.Assoc.add assoc_list ~equal:(fun l r -> val_eq_bool l r ss) key_val data_val
            | None -> [(key_val, data_val)]
        in
        (Map.set map_acc ~key:key_hash ~data:new_data, state)
    in
    let tail_map, state = match tail_map with
        | Some e ->
            let m, state = (eval_expr e ss) state in
            Some m, state
        | None -> None, state
    in
    let start_map = match tail_map with
        | Some (Dictionary m) -> m
        | None -> Map.empty (module Int)
        | _ ->
            printf "Expected a map\n";
            assert false
    in
    let (val_map, state) = 
        List.fold_left ~init:(start_map, state) ~f:fold_fn map_pairs
    in (Dictionary val_map, state)

and eval_list_expr ?tc:(_tail_call=false) ls tail ss = fun s ->
    let eval_expr_list ~init =
        List.fold_left
        ~init:init
        ~f:(fun (acc, s) e -> let (ev, s) = (eval_expr e ss) s in (ev::acc, s))
    in
    let eval_prepend ls tail =
        let (tail_eval, s) = (eval_expr tail ss) s in
        match tail_eval with
            | ValList tail_ls ->
                let (eval_ls, state) = eval_expr_list ~init:(tail_ls, s) (List.rev ls) in
                ValList eval_ls, state
            | _ ->
                printf "tried to prepend to a non-list";
                assert false
    in
    match tail with
        | Some tail -> eval_prepend ls tail
        | None ->
            let (eval_ls, state) = eval_expr_list ~init:([], s) ls in
            ValList (List.rev eval_ls), state

and eval_expr: (expr Located.t) -> static_state -> ?tc:bool -> state -> value * state = 
    let open Located in
    fun expr ss ?tc:(tail_call=false) -> 
        let eval_prefix_op op e = eval_prefix_op op e ss in
        let eval_op op lhs rhs = eval_op op lhs rhs ss in
        (* printf "Evaluating: %s\n" (string_of_expr expr); *)
        match expr with
        | {data = Atomic v; _} -> fun s -> v, s
        | {data = IdentExpr name; location} -> fun state -> eval_ident name location state
        | {data = Prefix ({op = Head; _} as e); _} -> eval_prefix_op val_list_head e.rhs
        | {data = Prefix ({op = Tail; _} as e); _} -> eval_prefix_op val_list_tail e.rhs
        | {data = Prefix ({op = Neg; _} as e); _} -> eval_prefix_op val_negate e.rhs
        | {data = Prefix ({op = Not; _} as e); _} -> eval_prefix_op val_negate_bool e.rhs
        | {data = Prefix ({op = _op; _}); _} -> assert false (* Invalid prefix op *)
        | {data = Binary ({op = Add; _} as e); _} -> eval_op val_add e.lhs e.rhs
        | {data = Binary ({op = Neg; _} as e); _} -> eval_op val_sub e.lhs e.rhs
        | {data = Binary ({op = Mul; _} as e); _} -> eval_op val_mul e.lhs e.rhs
        | {data = Binary ({op = Div; _} as e); _} -> eval_op val_div e.lhs e.rhs
        | {data = Binary ({op = EQ; _} as e); _} -> eval_op val_eq e.lhs e.rhs
        | {data = Binary ({op = NEQ; _} as e); _} -> eval_op val_neq e.lhs e.rhs
        | {data = Binary ({op = LEQ; _} as e); _} -> eval_op val_leq e.lhs e.rhs
        | {data = Binary ({op = GEQ; _} as e); _} -> eval_op val_geq e.lhs e.rhs
        | {data = Binary ({op = LT; _} as e); _} -> eval_op val_lt e.lhs e.rhs
        | {data = Binary ({op = GT; _} as e); _} -> eval_op val_gt e.lhs e.rhs
        | {data = Binary ({op = And; _} as e); _} -> eval_op val_and e.lhs e.rhs
        | {data = Binary ({op = Or; _} as e); _} -> eval_op val_or e.lhs e.rhs
        | {data = Binary ({op = Mod; _} as e); _} -> eval_op val_mod e.lhs e.rhs
        | {data = Binary ({op = _op; _}); _} -> assert false (* Invalid binary op *)
        | {data = LambdaDef d; _} -> eval_lambda_def d.lambda_def_expr d.lambda_def_args
        | {data = Let l; location} -> fun s -> (eval_let l.assignee l.assigned_expr ss location) s
        | {data = FnDef d; location} -> fun s -> (eval_fn_def d.fn_name d.fn_def_func ss location) s
        | {data = TupleExpr ls; _} -> fun s -> (eval_tuple_expr ls ss) s
        | {data = LambdaCall l; location} -> fun s -> (eval_lambda_call ~tc:tail_call l ss) location s
        | {data = IfExpr i; _} -> fun s -> (eval_if_expr ~tc:tail_call i ss) s
        | {data = BlockExpr ls; _} -> fun s -> (eval_block_expr ~tc:tail_call ls ss) s
        | {data = MatchExpr m; location} -> 
            fun s -> (eval_match_expr ~tc:tail_call m.match_val m.match_arms ss location) s
        | {data = MapExpr (ls, tail); _} -> fun s -> (eval_map_expr ~tc:tail_call ls tail ss) s
        | {data = ListExpr (ls, tail); _} -> eval_list_expr ls tail ss
        | {data = UnresolvedAtom n; _} ->
            printf "Found unresolved atom :%s\n" n;
            assert false
