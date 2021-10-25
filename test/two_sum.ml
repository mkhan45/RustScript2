open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = 
        default_state |> run_file (test_file "two_sum.rsc") in

    assert_equal_expressions "two_sum([1,9,13,20,47], 10)" "(0, 1)" state;
    assert_equal_expressions "two_sum([3,2,4,1,9], 12)" "(0, 4)" state;
    assert_equal_expressions "two_sum([], 10)" "()" state;

    printf "Passed\n"
