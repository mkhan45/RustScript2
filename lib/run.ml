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

let reverse_rsc =
    "
    let reverse = {
        let fold_step = fn(ls, x) => [x|ls]
        fn(ls) => fold([], fold_step, ls)
    }
    "

let filter_rev_rsc =
    "
    let filter_rev = fn(f, ls) => {
        let fold_step = fn(ls, x) => if f(x) then [x|ls] else ls
        fold([], fold_step, ls)
    }
    "

let filter_rsc = "let filter = fn(f, ls) => reverse(filter_rev(f, ls))"

let map_rev_rsc = "let map_rev = fn(f, ls) => fold([], fn(ls, x) => [f(x)|ls], ls)"
let map_rsc = "let map = fn(f, ls) => reverse(map_rev(f, ls))"

let range_rsc = "let range = fn(a, b) => range_step(a, b, 1)"

(* TODO: Make this a builtin *)
let zip_rev_rsc = 
    "
    let zip_rev = {
        let helper = fn(acc, l1, l2) => match (l1, l2)
            | ([], _) -> acc
            | (_, []) -> acc
            | ([x|xs], [y|ys]) -> helper([(x, y)|acc], xs, ys)

        fn(l1, l2) => helper([], l1, l2)
    }
    "

let zip_rsc = "let zip = fn(ls) => reverse(zip_rev(ls))"

let length_rsc = "let length = fn(ls) => fold(0, fn(l, _) => l + 1, ls)"

let enumerate_rev_rsc =
    "
    let enumerate_rev = fn(ls) => {
        let len = length(ls)
        zip_rev([0..len], ls)
    }
    "

let enumerate_rsc = "let enumerate = fn(ls) => reverse(enumerate_rev(ls))"

let concat_rsc = "let concat = fn(ls) => fold(\"\", fn(a, b) => a + b, ls)"

let concat_sep_rsc = "let concat_sep = fn(ls, sep) => fold(\"\", fn(a, b) => a + b + sep, ls)"

let load_stdlib state =
    let ss = { static_atoms = [] } in
    let run_line_swap line state = run_line ss state line in
    state
        |> run_line_swap "let sum = fn(ls) => fold(0, fn(a, b) => a + b, ls)"
        |> run_line_swap reverse_rsc
        |> run_line_swap filter_rev_rsc
        |> run_line_swap filter_rsc
        |> run_line_swap map_rev_rsc
        |> run_line_swap map_rsc
        |> run_line_swap range_rsc
        |> run_line_swap zip_rev_rsc
        |> run_line_swap zip_rsc
        |> run_line_swap length_rsc
        |> run_line_swap enumerate_rev_rsc
        |> run_line_swap enumerate_rsc
        |> run_line_swap concat_rsc
        |> run_line_swap concat_sep_rsc

let default_state: state = Map.empty (module String) |> load_stdlib

let run_file filename state =
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
    let ss = { static_atoms = Preprocess.find_atoms block [] } in
    let expr_ls = List.map ~f:(Preprocess.resolve_atoms ss) expr_ls in
    let fold_step = fun state e -> let _, s = (Eval.eval_expr e ss) state in s in
    ss, List.fold_left ~init:state ~f:fold_step expr_ls
