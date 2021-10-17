open Core

let val_add lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs +. rhs)
    | _ -> assert false

let val_sub lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs -. rhs)
    | _ -> assert false

let val_mul lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs *. rhs)
    | _ -> assert false

let val_div lhs rhs = match lhs, rhs with
    | Number lhs, Number rhs -> Number (lhs /. rhs)
    | _ -> assert false

let rec eval_let lhs rhs = 
    let assign lhs rhs: (string, value) Hashtbl.t ref -> value = fun state ->
        Hashtbl.add !state lhs ((eval_expr rhs) state);
        Unit
    in
    match lhs with
    | SinglePat s -> assign s rhs
    | TuplePat lhs_ls -> fun state ->
            let rhs_evaled = (eval_expr rhs) state in
            match rhs_evaled with
                | Tuple rhs_ls ->
                    let rec aux a b = match a, b with
                        | [], [] -> ()
                        | (((SinglePat lv)::l), (rv::r)) ->
                            Hashtbl.add !state lv rv;
                            aux l r
                        | _ -> assert false
                    in 
                    aux lhs_ls rhs_ls;
                    Unit
                | _ -> assert false

and eval_expr: expr -> (string, value) Hashtbl.t ref -> value = fun expr -> match expr with
    | Atomic n -> fun _ -> n
    | Ident v -> fun state -> Hashtbl.find !state v
    | Binary ({op = Add; _} as e) -> fun s -> val_add ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = Sub; _} as e) -> fun s -> val_sub ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = Mul; _} as e) -> fun s -> val_mul ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Binary ({op = Div; _} as e) -> fun s -> val_div ((eval_expr e.lhs) s) ((eval_expr e.rhs) s)
    | Let (_ as l) -> fun s -> (eval_let l.assignee l.assigned_expr) s
