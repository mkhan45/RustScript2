open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = 
        test_state () |> run_file (test_file "map.rsc") in

    assert_equal_expressions "get(m, 1)" "2" ss state;
    assert_equal_expressions "get(m, 3)" "4" ss state;
    assert_equal_expressions "get(m, (5, 6))" "(7, 8)" ss state;
    assert_equal_expressions "get(m, 467))" "()" ss state;

    assert_equal_expressions "get(m, 1)" "x" ss state;
    assert_equal_expressions "get(m, 3)" "y" ss state;
    assert_equal_expressions "get(m, (5, 6))" "z" ss state;
    assert_equal_expressions "get(m, 467))" "a" ss state;

    assert_equal_expressions "m(1)" "x" ss state;
    assert_equal_expressions "m(3)" "y" ss state;
    assert_equal_expressions "m((5, 6))" "z" ss state;
    assert_equal_expressions "m(467)" "a" ss state;

    assert_equal_expressions "one" "1" ss state;
    assert_equal_expressions "two" "2" ss state;
    assert_equal_expressions "three" "3" ss state;

    let ss, state = 
        test_state () |> run_file (test_file "caesar.rsc") in

    assert_equal_expressions "encode(\"HELLO WORLD\", 5)" "\"MJQQT BTWQI\"" ss state;
    assert_equal_expressions "decode(encode(\"HELLO WORLD\", 5), 5)" "\"HELLO WORLD\"" ss state;

    printf "Passed\n"
