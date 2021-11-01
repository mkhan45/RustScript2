open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        test_state () |> run_file (test_file "match_expr.rsc") in
    assert_equal_expressions "fib(20)" "10946" ss state;

    printf "Passed\n"
