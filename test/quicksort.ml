open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = 
        default_state |> run_file (test_file "quicksort.rsc") in

    assert_equal_expressions "sort([5, 3, 9, 10, 4, 7, 6])" "[3, 4, 5, 6, 7, 9, 10]" state;

    printf "Passed\n"
