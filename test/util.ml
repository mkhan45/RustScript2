open Stdio

open Rustscript.Run
open Rustscript.Eval
open Rustscript.Types

let assert_equal_expressions lhs rhs state =
    let (lhs_res, _) = eval state lhs in
    let (rhs_res, _) = eval state rhs in
    match (val_eq lhs_res rhs_res) with
        | Boolean true -> assert true
        | _ ->
            printf "Expected LHS: %s\n" (string_of_val lhs_res);
            printf "Got RHS: %s\n" (string_of_val rhs_res);
            printf "\n";
            assert false
