open Types
open Stdio
open Base

let val_add lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs +. rhs)
    | List lhs, List rhs -> List (lhs @ rhs)
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

let val_negate rhs = match rhs with
    | Number rhs -> Number (~-.rhs)
    | Boolean rhs -> Boolean (not rhs)
    | _ -> assert false

let val_list_head rhs = match rhs with
    | List (head::_) -> head
    | _ ->
        printf "Invalid Head: rhs = %s\n" (string_of_val rhs);
        assert false

let val_list_tail rhs = match rhs with
    | List (_::tail) -> List tail
    | _ ->
        printf "Invalid Tail: rhs = %s\n" (string_of_val rhs);
        assert false

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

and eval_lambda_call call =
    fun (state: state) -> match Map.find state call.callee with
        | Some(Lambda (lambda_val) as l) -> begin
            let (evaled, _) = (eval_expr call.call_args) state in
            let inner_state = (bind lambda_val.lambda_args evaled) state in
            let inner_state = Map.set inner_state ~key:call.callee ~data:l in
            let (result, _) = (eval_expr lambda_val.lambda_expr) inner_state in
            (result, state)
        end
        | None ->
                printf "Error: function not found: %s\n" call.callee;
                assert false
        | _ -> assert false

and eval_if_expr if_expr = fun state ->
    match (eval_expr if_expr.cond) state with
        | Boolean true, state -> (eval_expr if_expr.then_expr) state
        | Boolean false, state -> 
                (eval_expr if_expr.else_expr) state
        | _ -> assert false

and eval_block_expr ls state =
    let (res, _) =
        List.fold_left ~init:(Unit, state) ~f:(fun (_, state) e -> (eval_expr e) state) ls
    in (res, state)

and eval_expr: expr -> state -> value * state = fun expr -> 
    (* printf "Evaluating: %s\n" (string_of_expr expr); *)
    match expr with
        | Atomic n -> fun s -> n, s
        | Ident n -> fun state -> begin match Map.find state n with
            | Some v -> v, state
            | None -> 
                    printf "Error: variable not found: %s\n" n;
                    assert false
            end
        | Prefix ({op = Head; _} as e) -> fun s ->
                let (rhs, s) = (eval_expr e.rhs) s in
                val_list_head rhs, s
        | Prefix ({op = Tail; _} as e) -> fun s ->
                let (rhs, s) = (eval_expr e.rhs) s in
                val_list_tail rhs, s
        | Prefix ({op = Negate; _} as e) -> fun s ->
                let (rhs, s) = (eval_expr e.rhs) s in
                val_negate rhs, s
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
        | ListExpr ls -> fun s ->
            let (eval_ls, state) =
                List.fold_left
                    ~init:([], s)
                    ~f:(fun (acc, s) e -> let (ev, s) = eval_expr e s in (ev::acc, s))
                    ls
            in
            List (List.rev eval_ls), state
        | LambdaCall l -> fun s -> (eval_lambda_call l) s
        | IfExpr i -> fun s -> (eval_if_expr i) s
        | BlockExpr ls -> fun s -> eval_block_expr ls s
