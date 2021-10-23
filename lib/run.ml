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
