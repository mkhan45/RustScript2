open Rustscript
open Rustscript.Types
open Base
open Stdio

let rec repl state = 
    printf "> ";
    Out_channel.flush stdout;
    let ss = {static_atoms = []; static_block_funcs = []} in
    match In_channel.input_line ~fix_win_eol:true stdin with
    | Some "\n" -> ()
    | None -> ()
    | Some line -> 
        match Rustscript.Run.eval ss state line with
            | (Tuple [], new_state) -> repl new_state
            | (evaled, new_state) ->
                    printf "%s\n" (Rustscript.Types.string_of_val ss evaled);
                    Out_channel.flush stdout;
                    repl new_state

let () =
    let args = Sys.get_argv () in
    let ss, state = Run.default_state () in
    match args |> Array.to_list with
        | [_; filename] -> let _state = Run.run_file filename (ss, state) in ()
        | [_] ->
            repl state
        | _ ->
                printf "Usage: 'rustscript <filename>' or just 'rustscript' for REPL\n"
