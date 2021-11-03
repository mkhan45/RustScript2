open Stdio

open Rustscript.Run
open Rustscript.Types

let test_file filename = Printf.sprintf "../../../examples/%s" filename

let assert_equal_expressions lhs rhs ss state =
    let (lhs_res, _) = eval ss state lhs in
    let (rhs_res, _) = eval ss state rhs in
    match (Rustscript.Operators.val_eq lhs_res rhs_res ss {line_num = 0}) with
        | Boolean true -> assert true
        | _ ->
            printf "Expected LHS: %s\n" (string_of_val ss lhs_res);
            printf "Got RHS: %s\n" (string_of_val ss rhs_res);
            printf "\n";
            assert false
