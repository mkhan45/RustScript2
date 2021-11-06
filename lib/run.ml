open Base
open Stdio
open Types
open Scanner

let static_assoc_dedup ls =
    let ls = List.stable_sort ls ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2) in
    let rec loop ls = match ls with
        | (k1, _)::(((k2, _)::_) as xs) when String.equal k1 k2 -> 
            loop xs
        | x::xs -> x::(loop xs)
        | [] -> []
    in
    loop ls

let eval: static_state -> state -> string -> (value * state) * static_state = fun ss state s ->
    let (parsed, _remaining) = Parser.parse_str s "repl" in
    let static_atoms =
        Preprocess.find_atoms parsed.data ss.static_atoms
            |> static_assoc_dedup
    in
    let static_idents =
        Preprocess.find_idents parsed.data ss.static_idents
            |> static_assoc_dedup
    in
    let ss = { ss with static_atoms; static_idents } in
    let parsed = parsed |> Preprocess.resolve_atoms ss |> Preprocess.resolve_idents ss in
    let eval_closure = Eval.eval_expr parsed ss in
    (eval_closure state), ss

(* TODO: Make it support atoms *)
let run_line ss state line =
    match eval ss state line with
        | (Tuple [], new_state), _ -> new_state
        | (evaled, new_state), _ ->
            printf "%s\n" (string_of_val ss evaled);
            Out_channel.flush Stdio.stdout;
            new_state

let run_file filename (ss, state) =
    let in_stream = In_channel.create filename in
    let in_string = In_channel.input_all in_stream in
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
    let static_atoms =
        Preprocess.find_atoms block ss.static_atoms
            |> static_assoc_dedup
    in
    let static_idents =
        Preprocess.find_idents block ss.static_idents
            |> static_assoc_dedup
    in
    (* List.iter static_idents ~f:(fun (k, v) -> printf "%s: %d\n" k v); *)
    let ss = { ss with static_atoms; static_idents } in
    let expr_ls = List.map ~f:(Preprocess.resolve_atoms ss) expr_ls in
    let expr_ls = List.map ~f:(Preprocess.resolve_idents ss) expr_ls in
    let static_block_funcs = 
        Preprocess.find_block_funcs ss (expr_ls |> List.map ~f:Located.extract) ss.static_block_funcs 
    in
    (* List.iter static_block_funcs ~f:(fun (k, _) -> printf "%d: func\n" k); *)
    let ss = { ss with static_block_funcs } in
    let fold_step = fun state e -> let _, s = (Eval.eval_expr e ss) state in s in
    ss, List.fold_left ~init:state ~f:fold_step expr_ls

let base_static_atoms () = [("ok", 0); ("err", 1)]

let base_static_idents () = 
    let builtin_idents = [
        "inspect";
        "print";
        "println";
        "scanln";
        "to_string";
        "string_to_num";
        "string_to_int";
        "range_step";
        "fold";
        "to_charlist";
        "get";
    ] in
    List.zip_exn builtin_idents (List.range 0 (List.length builtin_idents))

let default_state () = 
    let static_atoms = base_static_atoms () in
    let static_idents = base_static_idents () in
    let static_block_funcs = [] in
    let call_stack = [] in
    run_file "./lib/stdlib.rsc" ({static_atoms; static_idents; static_block_funcs; call_stack}, (Map.empty (module Int)))

let test_state () = 
    let static_atoms = base_static_atoms () in
    let static_idents = base_static_idents () in
    let static_block_funcs = [] in
    let call_stack = [] in
    run_file 
        "../../../lib/stdlib.rsc" 
        ({static_atoms; static_idents; static_block_funcs; call_stack}, (Map.empty (module Int)))
