open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        Map.empty (module String) |> run_file (test_file "tailrec.rsc") in
    (* Evaluating this stack overflows when tail recursion isn't optimized*)
    assert_equal_expressions "sum(300000, 0)" "45000150000" ss state;

    let ss, state = 
        Map.empty (module String) |> run_file (test_file "fib_tc.rsc") in
    assert_equal_expressions "fib(30)" "2178309" ss state;

    printf "Passed\n"
