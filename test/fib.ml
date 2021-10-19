open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = Map.empty (module String) in
    let (_, state) = eval state "let fib = fn(n) => if n < 1 then 1 else fib(n - 1) + fib(n - 2)" in
    assert_equal_expressions "fib(10)" "144" state;;
    printf "Passed\n"
