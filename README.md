<div align=center>
    <img src="assets/logo.png" width="600px">
    <h1>
        RustScript V2
    </h1>
</div>


V2 of <https://github.com/mkhan45/RustScript>

I wrote RustScript originally in Java because it was part of a school project,
ignoring performance/code quality because I only had one night to do it.

This is an improved version of RustScript with improved performance and more features
written to learn OCaml. Still WIP


### Examples:

```
> let fib = fn(n) => if n < 2 then 1 else fib(n - 1) + fib(n - 2)
> fib(25)
121393.
> let (a, b) = (4, 2)
> (b, a)
(2., 4.)
> let (a, (b, c), d) = (4, (5, 6), 10)
> a * c + b / d
24.5
> (a, b, c, d)
(4., 5., 6., 10.)
```
