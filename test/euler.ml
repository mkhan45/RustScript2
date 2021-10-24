open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = 
        Map.empty (module String) |> run_file (test_file "euler1.rsc") in
    assert_equal_expressions "sum(filter(predicate, range(1, 1000)))" "233168" state;

    let state = 
        Map.empty (module String) |> run_file (test_file "euler2.rsc") in
    assert_equal_expressions "euler2" "4613732" state;

    let state = 
        Map.empty (module String) |> run_file (test_file "euler3.rsc") in
    assert_equal_expressions "euler3" "6857" state;

    printf "Passed\n"
