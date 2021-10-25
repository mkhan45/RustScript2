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

    assert_equal_expressions "get(m, 1)" "x" state;
    assert_equal_expressions "get(m, 3)" "y" state;
    assert_equal_expressions "get(m, (5, 6))" "z" state;
    assert_equal_expressions "get(m, 467))" "a" state;

    printf "Passed\n"
