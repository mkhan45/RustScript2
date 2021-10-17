# Rustscript V2

V2 of <https://github.com/mkhan45/RustScript>

I wrote RustScript in Java originally because it was part of a school project,
ignoring performance/code quality because I only had one night to do it.

This is an improved version of RustScript with improved performance and more features
written to learn OCaml. Still WIP


### Examples:

Right now it just executes each stdin argument as an expression
in order and prints the return value of the expression.

```
Δ dune exec ./rustscript.exe \
              "let fib = fn (n) => if n < 2 then 1 else fib(n - 1) + fib(n - 2)"\
              "fib(25)"
()
121393.
```
