open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = 
        default_state |> run_file (test_file "euler1.rsc") in
    assert_equal_expressions "euler1" "233168" state;

    let state = 
        default_state |> run_file (test_file "euler1_no_listcomp.rsc") in
    assert_equal_expressions "sum(filter_rev(predicate, range(1, 1000)))" "233168" state;

    let state = 
        default_state |> run_file (test_file "euler1_tup.rsc") in
    assert_equal_expressions "sum(filter(predicate, range(1, 1000)))" "233168" state;

    let state = 
        default_state |> run_file (test_file "euler1_tup.rsc") in
    assert_equal_expressions "sum(filter(predicate, range(1, 1000)))" "233168" state;

    let state = 
        default_state |> run_file (test_file "euler2.rsc") in
    assert_equal_expressions "euler2" "4613732" state;

    let state = 
        default_state |> run_file (test_file "euler3.rsc") in
    assert_equal_expressions "euler3" "6857" state;

    let state = 
        default_state |> run_file (test_file "euler5.rsc") in
    assert_equal_expressions "euler5" "232792560" state;

    printf "Passed\n"
