open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        test_state () |> run_file (test_file "strings.rsc") in
    assert_equal_expressions "result" "T" ss state;

    printf "Passed\n"
