open Rustscript
open Base
open Stdio

let rec repl state = 
    printf "> ";
    Out_channel.flush stdout;
    match In_channel.input_line ~fix_win_eol:true stdin with
    | Some "\n" -> ()
    | None -> ()
    | Some line -> 
        match Rustscript.Run.eval {static_atoms = []} state line with
            | (Tuple [], new_state) -> repl new_state
            | (evaled, new_state) ->
                    printf "%s\n" (Rustscript.Types.string_of_val {static_atoms = []} evaled);
                    Out_channel.flush stdout;
                    repl new_state

let () =
    let args = Sys.get_argv () in
    let state = Map.empty (module String) |> Run.load_stdlib in
    match args |> Array.to_list with
        | [_; filename] -> let _state = Run.run_file filename state in ()
        | [_] ->
            repl state
        | _ ->
                printf "Usage: 'rustscript <filename>' or just 'rustscript' for REPL\n"
