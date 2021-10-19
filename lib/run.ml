open Stdio
open Types

let eval state s = 
    let (parsed, _remaining) = Parser.parse_str s in
    let eval_closure = Eval.eval_expr parsed in
    eval_closure state

let run_line state line =
    match eval state line with
        | (Unit, new_state) -> new_state
        | (evaled, new_state) ->
            printf "%s\n" (string_of_val evaled);
            Out_channel.flush stdout;
            new_state


let run_file filename state = 
    let in_stream = In_channel.create filename in
    In_channel.fold_lines in_stream ~fix_win_eol:true ~init:state ~f:run_line
