open Core
open Base
open Stdio

let eval state s = 
    let (parsed, _remaining) = Parser.parse_str s in
    let eval_closure = Eval.eval_expr parsed in
    eval_closure state

let () =
    let state: state = Map.empty (module String) in
    let args = Sys.get_argv () in
    match args |> Array.to_list with
        | _::exprs -> 
                let _ = List.fold_left 
                ~init:state
                ~f:(fun state expr -> 
                        let (result, state) = eval state expr in
                        result |> string_of_val |> printf "%s\n";
                        state) 
                exprs
                in ()
        | _ -> print_endline "Usage: {cmd} [expression]"
