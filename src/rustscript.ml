open Types
open Base
open Stdio

let eval state s = 
    let (parsed, _remaining) = Parser.parse_str s in
    let eval_closure = Eval.eval_expr parsed in
    eval_closure state

let rec repl state = 
    printf "> ";
    Out_channel.flush stdout;
    match In_channel.input_line ~fix_win_eol:true stdin with
    | Some "\n" -> ()
    | None -> ()
    | Some line -> 
        match eval state line with
            | (Unit, new_state) -> repl new_state
            | (evaled, new_state) ->
                    printf "%s\n" (string_of_val evaled);
                    Out_channel.flush stdout;
                    repl new_state

let run_line state line =
    match eval state line with
        | (Unit, new_state) -> new_state
        | (evaled, new_state) ->
            printf "%s\n" (string_of_val evaled);
            Out_channel.flush stdout;
            new_state

let () =
    let args = Sys.get_argv () in
    let state: state = Map.empty (module String) in
    match args |> Array.to_list with
        | [_; filename] ->
                let in_stream = In_channel.create filename in
                let _ = In_channel.fold_lines in_stream ~fix_win_eol:true ~init:state ~f:run_line
                in ()
        | [_] ->
            repl state
        | _ ->
                printf "Usage: 'rustscript <filename>' or just 'rustscript' for REPL\n"
