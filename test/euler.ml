open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = 
        Map.empty (module String) |> run_file (test_file "euler1.rsc") in
    assert_equal_expressions "sum(filter(predicate, range(1, 1000)))" "233168" state;

    printf "Passed\n"
