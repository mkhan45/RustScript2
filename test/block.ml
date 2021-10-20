open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = 
        Map.empty (module String) |> run_file ~print_exprs:false (test_file "block.rsc") in
    assert_equal_expressions "a + b" "20" state;
    assert_equal_expressions "f(10, 5, 3)" "28" state;
    printf "Passed\n"
