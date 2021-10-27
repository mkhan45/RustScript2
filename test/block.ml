open Base
open Stdio
open Printf

open Rustscript.Run
open Util

let () =
    let ss, state = 
        Map.empty (module String) |> run_file (test_file "block.rsc") in
    assert_equal_expressions "a + b" "20" ss state;
    assert_equal_expressions "f(10, 5, 3)" "28" ss state;
    assert_equal_expressions "c" (Float.to_string (5. +. (5. +. 10. *. 2.))) ss state;

    let ss, state = 
        Map.empty (module String) |> run_file (test_file "fmap_tuple.rsc") in
    let input = "(5, (10, (20, (30, (1, ())))))" in
    let output = "(10, (20, (40, (60, (2, ())))))" in
    assert_equal_expressions (sprintf "fmap(f, %s)" input) output ss state;
    printf "Passed\n"
