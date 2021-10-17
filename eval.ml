open Core
open Printf

let val_add lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs +. rhs)
    | _ -> assert false

let val_sub lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs -. rhs)
    | _ -> assert false

let val_mul lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs *. rhs)
    | _ -> 
            printf "lhs: %s, rhs: %s\n" (string_of_val lhs) (string_of_val rhs);
            assert false

let val_div lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs /. rhs)
    | _ -> assert false

let val_eq lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (lhs = rhs)
    | Boolean lhs, Boolean rhs -> Boolean (lhs = rhs)
    | _ -> assert false

let val_lt lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (lhs < rhs)
    | _ -> assert false

let val_gt lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Boolean (lhs > rhs)
    | _ -> assert false

let rec eval_let lhs rhs = match lhs with
    | SinglePat s -> fun state ->
            Hashtbl.add !state s ((eval_expr rhs) state);
            Unit
    | TuplePat lhs_ls -> fun state ->
            let rhs_evaled = (eval_expr rhs) state in
            match rhs_evaled with
                | Tuple rhs_ls ->
                    let rec aux a b = match a, b with
                        | [], [] -> ()
                        | ((SinglePat lv)::l), (rv::r) ->
                            Hashtbl.add !state lv rv;
                            aux l r
                        | _ -> assert false
                    in 
                    aux lhs_ls rhs_ls;
                    Unit
                | _ -> assert false

(* TODO: Instead of copying state, only copy the overlapping assignments*)
and eval_lambda_call call = fun state ->
    match Hashtbl.find !state call.callee with
        | Lambda (lambda_val) -> begin
            let inner_state = ref (Hashtbl.copy !state) in

            let evaled_args = List.map (fun arg -> (eval_expr arg) state) call.call_args in
            let zipped = List.combine lambda_val.lambda_args evaled_args in
            List.iter (fun (key, value) -> Hashtbl.add !inner_state key value) zipped;

            Hashtbl.add !inner_state call.callee (Lambda (lambda_val));
            let call_result = (eval_expr lambda_val.lambda_expr) inner_state in

            call_result
        end
        | _ -> assert false

and eval_if_expr if_expr = fun state ->
    match (eval_expr if_expr.cond) state with
        | Boolean true -> (eval_expr if_expr.then_expr) state
        | Boolean false -> 
                (eval_expr if_expr.else_expr) state
        | _ -> assert false

and eval_expr: expr -> (string, value) Hashtbl.t ref -> value = fun expr -> match expr with
    | Atomic n -> fun _ -> n
    | Ident v -> fun state -> Hashtbl.find !state v
    | Binary ({op = Add; _} as e) -> fun s -> val_add ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = Sub; _} as e) -> fun s -> val_sub ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = Mul; _} as e) -> fun s -> val_mul ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = Div; _} as e) -> fun s -> val_div ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = EQ; _} as e) -> fun s -> val_eq ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = LT; _} as e) -> fun s -> val_lt ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = GT; _} as e) -> fun s -> val_gt ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Let l -> fun s -> (eval_let l.assignee l.assigned_expr) s
    | TupleExpr ls -> fun s -> Tuple (List.map (fun e -> eval_expr e s) ls)
    | LambdaCall l -> fun s -> (eval_lambda_call l) s
    | IfExpr i -> fun s -> (eval_if_expr i) s
