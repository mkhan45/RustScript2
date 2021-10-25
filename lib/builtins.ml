open Base
open Types
open Stdio

let inspect_builtin (args, state) =
    match args with
        | Tuple [v] ->
            printf "%s\n" (string_of_val v);
            (v, state)
        | _ ->
            printf "Expected only one argument to inspect";
            assert false

let range_builtin (args, state) =
    match args with
        | Tuple [Number start; Number end_] when (Caml.Float.is_integer start) && (Caml.Float.is_integer end_) ->
            let caml_ls = List.range (Float.to_int start) (Float.to_int end_) in
            let val_ls = List.map ~f:(fun n -> Number (Int.to_float n)) caml_ls in
            ValList val_ls, state
        | _ ->
            printf "Expected two integer arguments to inspect";
            assert false

let fold_builtin (args, state) =
    match args with
        | Tuple [init, Lambda fn, ValList ls] ->
            let call_fn = fun lambda args ->
                let lambda_call = Thunk {thunk_fn = }
            let new_ls = 
                List.fold 
                ~init:init ~f
                ~f:(fun acc, v -> )
        | _ ->
            printf "Expected (init, fn, ls) as arguments to fold";
            assert false
