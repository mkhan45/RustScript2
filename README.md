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

### Build

```bash
dune build
```

Run a file using:

```bash
dune exec ./bin/rustscript_cli.exe <file>
```

Start a REPL using:

```bash
dune exec ./bin/rustscript_cli.exe
```


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

```
let (a, b) = {
    let a = (4, 2)
    let (a, b) = a
    (a * b, 12)
}

let f = fn(a, b, c) => {
    let g = fn(a, b) => a * b + c
    g(b, c) + a
}

inspect(f(10, 5, 3)) # 28
```

```
let fmap = fn (f, ls) => {
    if ls == () then {
        ()
    } else {
        let (hd, tl) = ls
        (f(hd), fmap(f, tl))
    }
}

let f = fn(x) => x * 2

let result = fmap(f, (5, (10, (20, (30, (1, ()))))))
inspect(result) #(10, (20, (40, (60, (2, ())))))
```
