open Types
open Stdio
open Base

let val_add lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs +. rhs)
    | _ -> 
            printf "Invalid Add: lhs = %s, rhs = %s\n" (string_of_val lhs) (string_of_val rhs);
            assert false

let val_sub lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs -. rhs)
    | _ -> assert false

let val_mul lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs *. rhs)
    | _ -> 
            printf "Invalid Mul: lhs = %s, rhs = %s\n" (string_of_val lhs) (string_of_val rhs);
            assert false

let val_div lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs /. rhs)
    | _ -> assert false

let val_is_true = function
    | Boolean true -> true
    | _ -> false

let rec val_eq lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.equal lhs rhs)
    | Boolean lhs, Boolean rhs -> Boolean (Bool.equal lhs rhs)
    | Tuple lhs, Tuple rhs ->
            if phys_equal (List.length lhs) (List.length rhs)
                then
                    let zipped = List.zip_exn lhs rhs in
                    let res = List.for_all zipped ~f:(fun (a, b) -> val_is_true (val_eq a b))
                    in Boolean res
                else
                    Boolean false
    | _ -> assert false

let val_lt lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs < 0)
    | _ -> assert false

let val_gt lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (Float.compare lhs rhs > 0)
    | _ -> assert false

let rec bind lhs rhs = 
    (* printf "Binding %s to %s\n" (string_of_pat lhs) (string_of_val rhs); *)
    match lhs, rhs with
    | SinglePat s, _ -> fun state ->
            Map.set state ~key:s ~data:rhs;
    | (TuplePat lhs_ls), (Tuple rhs_ls) -> fun state ->
            if phys_equal (List.length lhs_ls) (List.length rhs_ls)
                then 
                    let zipped = List.zip_exn lhs_ls rhs_ls in
                    List.fold_left ~init:state ~f:(fun state (k, v) -> (bind k v) state) zipped
                else begin
                    printf "\n";
                    printf "Tried to bind %s of len %d to %s of len %d\n"
                        (string_of_pat (TuplePat lhs_ls)) (List.length lhs_ls)
                        (string_of_val (Tuple rhs_ls)) (List.length rhs_ls);
                    assert false
                end
    | _ -> assert false

let rec eval_let lhs rhs = fun state ->
    let (evaled, new_state) = (eval_expr rhs) state in
    let new_state = (bind lhs evaled) new_state in
    (Unit, new_state)

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

and eval_expr: expr -> ?tc:bool -> state -> value * state = 
    fun expr ?tc:(tail_call=false) -> 
        (* printf "Evaluating: %s\n" (string_of_expr expr); *)
        match expr with
        | Atomic n -> fun s -> n, s
        | Ident n -> fun state -> begin
            match Map.find state n with
                | Some v -> v, state
                | None -> 
                        printf "Error: variable not found: %s\n" n;
                        assert false
        end
        | LambdaDef {lambda_def_expr = e; lambda_def_args = args} -> fun s ->
                (Lambda {lambda_expr = e; lambda_args = args; enclosed_state = s}), s
        | Binary ({op = Add; _} as e) -> fun s -> 
                let (lhs, s) = (eval_expr e.lhs) s in
                let (rhs, s) = (eval_expr e.rhs) s in
                val_add lhs rhs, s
        | Binary ({op = Sub; _} as e) -> fun s -> 
                let (lhs, s) = (eval_expr e.lhs) s in
                let (rhs, s) = (eval_expr e.rhs) s in
                val_sub lhs rhs, s
        | Binary ({op = Mul; _} as e) -> fun s -> 
                let (lhs, s) = (eval_expr e.lhs) s in
                let (rhs, s) = (eval_expr e.rhs) s in
                val_mul lhs rhs, s
        | Binary ({op = Div; _} as e) -> fun s -> 
                let (lhs, s) = (eval_expr e.lhs) s in
                let (rhs, s) = (eval_expr e.rhs) s in
                val_div lhs rhs, s
        | Binary ({op = EQ; _} as e) -> fun s -> 
                let (lhs, s) = (eval_expr e.lhs) s in
                let (rhs, s) = (eval_expr e.rhs) s in
                val_eq lhs rhs, s
        | Binary ({op = LT; _} as e) -> fun s -> 
                let (lhs, s) = (eval_expr e.lhs) s in
                let (rhs, s) = (eval_expr e.rhs) s in
                val_lt lhs rhs, s
        | Binary ({op = GT; _} as e) -> fun s -> 
                let (lhs, s) = (eval_expr e.lhs) s in
                let (rhs, s) = (eval_expr e.rhs) s in
                val_gt lhs rhs, s
        | Let l -> fun s -> (eval_let l.assignee l.assigned_expr) s
        | TupleExpr ls -> fun s -> 
                let (eval_ls, state) =
                    List.fold_left 
                        ~init:([], s) 
                        ~f:(fun (acc, s) e -> let (ev, s) = eval_expr e s in (ev::acc, s))
                        ls
                in
                Tuple (List.rev eval_ls), state
        | LambdaCall l -> fun s -> (eval_lambda_call ~tc:tail_call l) s
        | IfExpr i -> fun s -> (eval_if_expr ~tc:tail_call i) s
        | BlockExpr ls -> fun s -> eval_block_expr ~tc:tail_call ls s
