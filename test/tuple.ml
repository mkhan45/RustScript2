open Base
open Stdio

open Rustscript.Run
open Util

let () =
    let state = Map.empty (module String) in
    let (_, state) = eval state "let x = (1, 2, (3, 4, 5), (6, 7), 8)" in
    let (_, state) = eval state "let (a, b, c, d, e) = x" in
    let (_, state) = eval state "let (f, g, h) = c" in
    assert_equal_expressions "(a, b, e, f, g, h)" "(1, 2, 8, 3, 4, 5)" state;
    printf "Passed\n"
