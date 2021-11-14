open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        test_state () |> run_file (test_file "capture.rsc") in

    assert_equal_expressions "g(3)" "-2" ss state;
    assert_equal_expressions "g(5)" "0" ss state;
    assert_equal_expressions "f(1, 2, 4)" "quadratic(1, 2, 0, 4)" ss state;
    assert_equal_expressions "j(4)" "f(1, 2, 4)" ss state;
    assert_equal_expressions "h(5, 3)" "-2" ss state;

    printf "Passed\n"
