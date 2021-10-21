open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = 
        Map.empty (module String) |> run_file (test_file "tailrec.rsc") in

    (* Evaluating this stack overflows when tail recursion isn't optimized*)
    assert_equal_expressions "sum(300000, 0)" "45000150000" state;

    printf "Passed\n"
