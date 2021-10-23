open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = 
        Map.empty (module String) |> run_file (test_file "map.rsc") in

    assert_equal_expressions "get(m, 1)" "2" state;
    assert_equal_expressions "get(m, 3)" "4" state;
    assert_equal_expressions "get(m, (5, 6))" "(7, 8)" state;
    assert_equal_expressions "get(m, 467))" "()" state;

    printf "Passed\n"
