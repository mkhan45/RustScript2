open Stdio

open Rustscript.Run
open Util

let () =
    let ss, state = test_state () in
    let (_, state) = eval ss state "let fib = fn(n) => if n < 1 then 1 else fib(n - 1) + fib(n - 2)" in
    assert_equal_expressions "fib(10)" "144" ss state;
    printf "Passed\n"
