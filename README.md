# Rustscript V2

V2 of <https://github.com/mkhan45/RustScript>

I wrote RustScript originally in Java because it was part of a school project,
ignoring performance/code quality because I only had one night to do it.

This is an improved version of RustScript with improved performance and more features
written to learn OCaml. Still WIP


### Examples:

Right now it just executes each stdin argument as an expression
in order and prints the return value of the expression.

```
 ~/p/rustscript2 (main) Δ dune exec ./rustscript.exe \
                              "let fib = fn (n) => if n < 2 then 1 else fib(n - 1) + fib(n - 2)"\
                              "fib(25)"\
                              \
                              "let (a, b) = (4, 2)"\
                              "(b, a)"\
                              "let (a, (b, c)) = (4, (5, 10))"\
                              "a * c + b"
()
121393.
lhs: a b, rhs: 4. 2.
()
2., 4.
lhs: a (b, c), rhs: 4. (5., 10.)
lhs: b c, rhs: 5. 10.
()
45.
```
