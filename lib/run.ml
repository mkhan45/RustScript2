open Base
open Stdio
open Types
open Scanner

let eval ss state s = 
    let (parsed, _remaining) = Parser.parse_str s in
    let eval_closure = Eval.eval_expr parsed ss in
    eval_closure state

(* TODO: Make it support atoms *)
let run_line ss state line =
    match eval ss state line with
        | (Tuple [], new_state) -> new_state
        | (evaled, new_state) ->
            printf "%s\n" (string_of_val ss evaled);
            Out_channel.flush Stdio.stdout;
            new_state

let run_file filename (ss, state) =
    let in_stream = In_channel.create filename in
    let in_string = In_channel.input_all in_stream in
    let tokens = in_string |> Scanner.scan |> skip_newlines in
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
    let static_atoms = Preprocess.find_atoms block ss.static_atoms in
    let ss = { ss with static_atoms } in
    let expr_ls = List.map ~f:(Preprocess.resolve_atoms ss) expr_ls in
    let static_block_funcs = Preprocess.find_block_funcs expr_ls ss.static_block_funcs in
    let ss = { ss with static_block_funcs } in
    let fold_step = fun state e -> let _, s = (Eval.eval_expr e ss) state in s in
    ss, List.fold_left ~init:state ~f:fold_step expr_ls

let default_state () = 
    run_file "./lib/stdlib.rsc" ({static_atoms = []; static_block_funcs = []}, (Map.empty (module String)))

let test_state () = 
    run_file "../../../lib/stdlib.rsc" ({static_atoms = []; static_block_funcs = []}, (Map.empty (module String)))
