open Rustscript
open Rustscript.Types
open Base
open Stdio

let rec repl state ss = 
    printf "> ";
    Out_channel.flush stdout;
    match In_channel.input_line ~fix_win_eol:true stdin with
    | Some "\n" -> ()
    | None -> ()
    | Some line -> 
        match Rustscript.Run.eval ss state line with
            | (Tuple [], new_state), ss -> repl new_state ss
            | (evaled, new_state), ss ->
                    printf "%s\n" (Rustscript.Types.string_of_val ss evaled);
                    Out_channel.flush stdout;
                    repl new_state ss

let () =
    let args = Sys.get_argv () in
    let ss, state = Run.default_state () in
    match args |> Array.to_list with
        | [_] ->
            repl state ss
        | _::"compile"::filenames ->
            let compiled = List.fold_left ~init:"" ~f:(fun s f -> s ^ "\n" ^ Compile.compile_file f) filenames
            in
            printf "%s" compiled
        | _::filenames -> 
            let _ = List.fold_left ~init:(ss, state) ~f:(fun s f -> Run.run_file f s) filenames
            in
            ()
        | _ ->
                printf "Usage: 'rustscript <filename>' or just 'rustscript' for REPL\n"
