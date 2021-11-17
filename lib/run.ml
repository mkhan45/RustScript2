open Base
open Stdio
open Types
open Scanner


let eval: static_state -> state -> string -> (value * state) * static_state = fun ss state s ->
    let (parsed, _remaining) = Parser.parse_str s "repl" in
    let static_atoms =
        Preprocess.find_atoms (ExprNode parsed) ss.static_atoms
    in
    let static_idents =
        Preprocess.find_idents (ExprNode parsed) ss.static_idents
    in
    let ss = { ss with static_atoms; static_idents } in
    let parsed = parsed 
        |> fun e -> Preprocess.ExprNode e
        |> fun e -> Preprocess.resolve_atoms e ss.static_atoms
        |> fun e -> Preprocess.resolve_idents e ss.static_idents
        |> Preprocess.unwrap_expr_node
    in
    let eval_closure = Eval.eval_expr parsed ss in
    (eval_closure state), ss

let run_line ss state line =
    match eval ss state line with
        | (Tuple [], new_state), _ -> new_state
        | (evaled, new_state), _ ->
            printf "%s\n" (string_of_val ss evaled);
            Out_channel.flush Stdio.stdout;
            new_state

let run_string in_string filename (ss, state) =
    let locate = Located.locate {line_num = 0; filename = filename} in
    let tokens = in_string |> Scanner.scan ~filename:filename |> skip_newlines in
    let expr_ls =
        let rec aux remaining acc = match (skip_newlines remaining) with
            | [] -> acc
            | remaining ->
                  let (parsed, remaining) = Parser.parse remaining 0 in
                  aux remaining (parsed::acc)
        in
        let (parsed, remaining) = Parser.parse tokens 0 in
        List.rev (aux remaining [parsed])
    in
    let block = BlockExpr expr_ls in
    let block = Preprocess.resolve_imports (Preprocess.ExprNode (locate block)) in
    let expr_ls = match block with
        | {data = BlockExpr ls; _} -> ls
        | _ -> assert false
    in
    let static_atoms =
        Preprocess.find_atoms (ExprNode block) ss.static_atoms
    in
    let static_idents =
        Preprocess.find_idents (ExprNode block) ss.static_idents
    in
    (* List.iter static_idents ~f:(fun (k, v) -> printf "%s: %d\n" k v); *)
    let ss = { ss with static_atoms; static_idents } in
    let expr_ls = expr_ls
        |> List.map ~f:(fun e -> Preprocess.ExprNode e)
        |> List.map ~f:(fun e -> Preprocess.resolve_atoms e ss.static_atoms)
        |> List.map ~f:(fun e -> Preprocess.resolve_idents e ss.static_idents)
        |> List.map ~f:Preprocess.unwrap_expr_node
    in
    let static_block_funcs = 
        Preprocess.find_block_funcs ss (expr_ls |> List.map ~f:Located.extract) ss.static_block_funcs 
    in
    let ss = { ss with static_block_funcs } in
    let block = BlockExpr expr_ls |> Located.locate {line_num = 0; filename = filename} in
    (* List.iter *) 
    (*     static_block_funcs *) 
    (*     ~f:(fun (k, f) -> *) 
    (*             let fn_name = List.Assoc.find_exn (List.Assoc.inverse ss.static_idents) ~equal:Int.equal k in *)
    (*             let is_inlinable = Preprocess.is_function_inlinable k ss f.fn_expr.data in *)
    (*             printf "%s is inlinable: %b\n" fn_name is_inlinable); *)
    (* TODO: If statement syntax errors here? *)
    let block = match (Preprocess.clobbers_declared_fn_test (ExprNode block) ss.static_block_funcs) with
        | false -> block
        | true ->
            printf "Tried to clobber function with variable binding\n";
            Caml.exit 0
    in
    let block = block
        |> fun b -> Preprocess.inline_functions (ExprNode b) ss.static_block_funcs
        |> Preprocess.unwrap_expr_node
    in
    (* printf "%s\n" (string_of_expr ss block.data); *)
    let expr_ls = match block with
    | {data = BlockExpr expr_ls; _} -> expr_ls
    | _ -> assert false
    in
    let fold_step = fun state e -> let _, s = (Eval.eval_expr e ss) state in s in
    ss, List.fold_left ~init:state ~f:fold_step expr_ls

let run_file filename (ss, state) =
    let in_stream = In_channel.create filename in
    let in_string = In_channel.input_all in_stream in
    run_string in_string filename (ss, state)

let base_static_atoms () = [("ok", 0); ("err", 1)]

let base_static_idents () = 
    let builtin_idents = [
        "inspect__builtin";
        "print__builtin";
        "println__builtin";
        "scanln__builtin";
        "to_string__builtin";
        "string_to_num__builtin";
        "string_to_int__builtin";
        "range_step__builtin";
        "fold__builtin";
        "to_charlist__builtin";
        "get__builtin";
        "read_file__builtin";
        "write_file__builtin";
        "list_dir__builtin";
        "map_keys__builtin";
        "map_to_list__builtin";
    ] in
    List.zip_exn builtin_idents (List.range 0 (List.length builtin_idents))

let default_state () = 
    let static_atoms = base_static_atoms () in
    let static_idents = base_static_idents () in
    let static_block_funcs = [] in
    let call_stack = [] in
    let code_string = [%blob "stdlib.rsc"] in
    let ss = {static_atoms; static_idents; static_block_funcs; call_stack} in
    run_string code_string "stdlib.rsc" (ss, (Map.empty (module Int)))

let test_state = default_state
