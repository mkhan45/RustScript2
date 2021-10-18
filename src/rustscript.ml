open Core

let eval state s = let (parsed, _remaining) = Parser.parse_str s 
                    in (Eval.eval_expr parsed) state;;

let () =
    let state = ref (Hashtbl.create 16) in
    match Sys.argv |> Array.to_list with
        | _::exprs -> List.iter (fun expr -> expr |> eval state |> string_of_val |> print_endline) exprs
        | _ -> print_endline "Usage: {cmd} [expression]"
