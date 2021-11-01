open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        test_state () |> run_file (test_file "run_len_encode.rsc") in

    assert_equal_expressions 
    "run_len_encode(test_ls)" 
    "[(1., 2.), (2., 1.), (3., 1.), (4., 3.), (5., 1.), (6., 1.), (1., 1.), (2., 2.)]" 
    ss
    state;

    printf "Passed\n"
