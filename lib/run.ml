open Base
open Stdio
open Types
open Scanner

let eval state s = 
    let (parsed, _remaining) = Parser.parse_str s in
    let eval_closure = Eval.eval_expr parsed in
    eval_closure state

let run_line state line =
    match eval state line with
        | (Tuple [], new_state) -> new_state
        | (evaled, new_state) ->
            printf "%s\n" (string_of_val evaled);
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

let load_stdlib state =
    let run_line_swap line state = run_line state line in
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

let default_state = Map.empty(module String) |> load_stdlib

let run_file filename state = 
    let in_stream = In_channel.create filename in
    let in_string = In_channel.input_all in_stream in
    let tokens = in_string |> Scanner.scan |> skip_newlines in
    let rec aux (parsed, remaining) state =
        let remaining = skip_newlines remaining in
        match (Eval.eval_expr parsed state), remaining with
            | (_, new_state), [] -> new_state
            | (_, new_state), remaining -> aux (Parser.parse remaining 0) new_state
    in aux (Parser.parse tokens 0) state
