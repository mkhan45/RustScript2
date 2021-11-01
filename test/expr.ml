open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        test_state () |> run_file (test_file "expr.rsc") in

    assert_equal_expressions "eval_str(\"2\")" (Int.to_string 2) ss state;
    assert_equal_expressions "eval_str(\"2 * 5 + 4 / 3\")" (Float.to_string (2. *. 5. +. 4. /. 3.)) ss state;
    assert_equal_expressions 
        "eval_str(\"53 / 76 + 4 - 32 / 21\")" 
        (Float.to_string (53. /. 76. +. 4. -. 32. /. 21.)) 
        ss state;

    printf "Passed\n"
