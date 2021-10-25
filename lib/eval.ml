open Types
open Stdio
open Base
open Operators

let rec bind lhs rhs = 
    (* printf "Binding %s to %s\n" (string_of_pat lhs) (string_of_val rhs); *)
    match lhs, rhs with
    | SinglePat s, _ -> fun state ->
            Map.set state ~key:s ~data:rhs;
    | NumberPat lhs, Number rhs when Float.equal lhs rhs -> 
            fun state -> state
    | ((TuplePat lhs_ls) as lhs, ((Tuple rhs_ls) as rhs))|
      ((ListPat (FullPat lhs_ls)) as lhs, ((ValList rhs_ls) as rhs)) -> fun state -> begin
            (* TODO: Look into moving the closure inwards, moving some "runtime" computation to "comptime" *)
            match List.zip lhs_ls rhs_ls with
                | Ok zipped -> 
                    List.fold_left ~init:state ~f:(fun state (k, v) -> (bind k v) state) zipped
                | _ ->
                    printf "\n";
                    printf "Tried to bind %s of len %d to %s of len %d\n"
                        (string_of_pat lhs) (List.length lhs_ls)
                        (string_of_val rhs) (List.length rhs_ls);
                    assert false
      end
    | (ListPat (HeadTailPat (head_pat_ls, tail_pat))), ValList rhs_ls -> fun s ->
            let (head_ls, tail_ls) = List.split_n rhs_ls (List.length head_pat_ls) in
            let s = (bind (ListPat (FullPat head_pat_ls)) (ValList head_ls)) s in
            let s = (bind tail_pat (ValList tail_ls)) s in
            s
    | WildcardPat, _ -> fun state -> state
    | _ -> assert false

let rec list_equal_len lhs rhs = match lhs, rhs with
    | [], [] -> true
    | [], _ | _, [] -> false
    | _::xs, _::ys -> list_equal_len xs ys

let rec pattern_matches pat value =
    match pat, value with
        | WildcardPat, _ -> true
        | SinglePat _, _ -> true
        | NumberPat lhs, Number rhs -> 
                Float.equal lhs rhs
        | ((TuplePat lhs_ls), (Tuple rhs_ls))|(ListPat (FullPat lhs_ls), ValList rhs_ls) ->
            if list_equal_len lhs_ls rhs_ls then
                let zipped = List.zip_exn lhs_ls rhs_ls in
                List.for_all ~f:(fun (p, v) -> pattern_matches p v) zipped
            else false
        | (ListPat (HeadTailPat (head_pat_ls, tail_pat)), ValList rhs_ls) ->
            let (head_ls, tail_ls) = List.split_n rhs_ls (List.length head_pat_ls) in
            let head_matches = pattern_matches (ListPat (FullPat head_pat_ls)) (ValList head_ls) in
            let tail_matches = pattern_matches tail_pat (ValList tail_ls) in
            head_matches && tail_matches
        | _ -> false

let rec eval_op op lhs rhs = fun s ->
    let (lhs, s) = (eval_expr lhs) s in
    let (rhs, s) = (eval_expr rhs) s in
    op lhs rhs, s
    
and eval_prefix_op op rhs = fun s ->
    let (rhs, s) = (eval_expr rhs) s in
    op rhs, s

and eval_ident name = fun state ->
    match Map.find state name with
        | Some value -> value, state
        | None ->
            printf "Error: variable not found: %s\n" name;
            assert false

and eval_let lhs rhs = fun state ->
    let (evaled, new_state) = (eval_expr rhs) state in
    let new_state = (bind lhs evaled) new_state in
    (Tuple [], new_state)

and eval_lambda_def e args =
    fun s -> (Lambda {lambda_expr = e; lambda_args = args; enclosed_state = s}), s

and unwrap_thunk thunk state = match thunk with
    | Thunk {thunk_fn = thunk_fn; thunk_args = thunk_args; thunk_fn_name = thunk_fn_name} ->
            let inner_state = (bind thunk_fn.lambda_args thunk_args) thunk_fn.enclosed_state in
            let inner_state = Map.set inner_state ~key:thunk_fn_name ~data:(Lambda thunk_fn) in
            let (new_thunk, _) = (eval_expr ~tc:true thunk_fn.lambda_expr) inner_state in
            unwrap_thunk new_thunk state
    | value -> value, state

and eval_lambda_call ?tc:(tail_call=false) call =
    fun (state: state) -> match Map.find state call.callee with
        | Some(Lambda lambda_val) -> begin
            let (evaled, _) = (eval_expr call.call_args) state in
            let thunk = Thunk {thunk_fn = lambda_val; thunk_args = evaled; thunk_fn_name = call.callee} in
            if tail_call 
                then (thunk, state)
                else 
                    let res, _ = unwrap_thunk thunk state in
                    (res, state)
        end
        | None -> begin
            match call.callee with
                | "inspect" ->
                    let (result, _) = (eval_expr call.call_args) state in begin
                    match result with
                        | Tuple [v] -> 
                            printf "%s\n" (string_of_val v);
                            (v, state)
                        | _ -> 
                            printf "Expected only one argument to inspect";
                            assert false
                    end
                | _ -> 
                    printf "Error: function not found: %s\n" call.callee;
                    assert false
        end
        | _ -> assert false

and eval_tuple_expr ls state =
    let (eval_ls, state) =
        List.fold_left 
            ~init:([], state) 
            ~f:(fun (acc, s) e -> let (ev, s) = eval_expr e s in (ev::acc, s))
            ls
    in
    Tuple (List.rev eval_ls), state

and eval_if_expr ?tc:(tail_call=false) if_expr = fun state ->
    match (eval_expr if_expr.cond) state with
        | Boolean true, state -> (eval_expr ~tc:tail_call if_expr.then_expr) state
        | Boolean false, state -> 
                (eval_expr ~tc:tail_call if_expr.else_expr) state
        | _ -> assert false

and eval_block_expr ?tc:(tail_call=false) ls state =
    let (res, _) =
        let len = List.length ls in
        match List.split_n ls (len - 1) with
            | exprs, [last_expr] ->
                let block_state =
                    List.fold_left 
                        ~init:state 
                        ~f:(fun line_state e -> let _, s = (eval_expr e) line_state in s) 
                        exprs
                in
                (eval_expr ~tc:tail_call last_expr) block_state
            | _ -> assert false
    in (res, state)
    
and eval_match_expr ?tc:(tail_call=false) match_val match_arms state =
    let (match_val, state) = (eval_expr match_val) state in
    let result_state_opt = List.find_map ~f:(
        fun (pat, arm_expr, cond) -> 
            if pattern_matches pat match_val then
                match cond with
                    | Some cond ->
                        let inner_state = (bind pat match_val) state in
                        let cond_eval, inner_state = (eval_expr cond) inner_state in
                        if val_is_true cond_eval then
                            let (result, _) = (eval_expr ~tc:tail_call arm_expr) inner_state in
                            Some (result, state)
                        else
                            None
                    | None ->
                        let inner_state = (bind pat match_val) state in
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
            printf "No patterns matched in match expression\n";
            assert false

and eval_list_expr ?tc:(_tail_call=false) ls tail = fun s ->
    let eval_expr_list ~init =
        List.fold_left
        ~init:init
        ~f:(fun (acc, s) e -> let (ev, s) = eval_expr e s in (ev::acc, s))
    in
    let eval_prepend ls tail =
        let (tail_eval, s) = (eval_expr tail) s in
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

and eval_expr: expr -> ?tc:bool -> state -> value * state = 
    fun expr ?tc:(tail_call=false) -> 
        (* printf "Evaluating: %s\n" (string_of_expr expr); *)
        match expr with
        | Atomic v -> fun s -> v, s
        | Ident name -> fun state -> eval_ident name state
        | Prefix ({op = Head; _} as e) -> eval_prefix_op val_list_head e.rhs
        | Prefix ({op = Tail; _} as e) -> eval_prefix_op val_list_tail e.rhs
        | Prefix ({op = Neg; _} as e) -> eval_prefix_op val_negate e.rhs
        | Prefix ({op = Not; _} as e) -> eval_prefix_op val_negate_bool e.rhs
        | Prefix ({op = _op; _}) -> assert false (* Invalid prefix op *)
        | Binary ({op = Add; _} as e) -> eval_op val_add e.lhs e.rhs
        | Binary ({op = Neg; _} as e) -> eval_op val_sub e.lhs e.rhs
        | Binary ({op = Mul; _} as e) -> eval_op val_mul e.lhs e.rhs
        | Binary ({op = Div; _} as e) -> eval_op val_div e.lhs e.rhs
        | Binary ({op = EQ; _} as e) -> eval_op val_eq e.lhs e.rhs
        | Binary ({op = NEQ; _} as e) -> eval_op val_neq e.lhs e.rhs
        | Binary ({op = LT; _} as e) -> eval_op val_lt e.lhs e.rhs
        | Binary ({op = GT; _} as e) -> eval_op val_gt e.lhs e.rhs
        | Binary ({op = And; _} as e) -> eval_op val_and e.lhs e.rhs
        | Binary ({op = Or; _} as e) -> eval_op val_or e.lhs e.rhs
        | Binary ({op = Mod; _} as e) -> eval_op val_mod e.lhs e.rhs
        | Binary ({op = _op; _}) -> assert false (* Invalid binary op *)
        | LambdaDef d -> eval_lambda_def d.lambda_def_expr d.lambda_def_args
        | Let l -> fun s -> (eval_let l.assignee l.assigned_expr) s
        | TupleExpr ls -> fun s -> eval_tuple_expr ls s
        | LambdaCall l -> fun s -> (eval_lambda_call ~tc:tail_call l) s
        | IfExpr i -> fun s -> (eval_if_expr ~tc:tail_call i) s
        | BlockExpr ls -> fun s -> eval_block_expr ~tc:tail_call ls s
        | MatchExpr m -> fun s -> eval_match_expr ~tc:tail_call m.match_val m.match_arms s
        | ListExpr (ls, tail) -> eval_list_expr ls tail
