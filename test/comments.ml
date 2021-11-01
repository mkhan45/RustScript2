open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        test_state () |> run_file (test_file "block.rsc") in
    assert_equal_expressions "a + b" "20" ss state;
    assert_equal_expressions "f(10, 5, 3)" "28" ss state;

    let ss, state = 
        test_state () |> run_file (test_file "comment.rsc") in
    let input = "a" in
    let output = "5" in
    assert_equal_expressions input output ss state;

    let input = "b" in
    let output = "(5, 10, 15)" in
    assert_equal_expressions input output ss state;
    printf "Passed\n"
