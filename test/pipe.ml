open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        test_state () |> run_file (test_file "pipe.rsc") in

    assert_equal_expressions "x" "45" ss state;
    assert_equal_expressions "g" "[\"f0\", \"d1\", \"s2\", \"a3\"]" ss state;

    printf "Passed\n"
