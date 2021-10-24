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

##### REPL
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

##### Tuple Assignment/Matching
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

##### Fmap
Soon, proper linkedlists will be added instead of leveraging tuples
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

##### Project Euler #1
```
let range = {
    let helper = fn (l, r, acc) => 
        if l == r then acc else helper(l, r - 1, (r, acc))
    
    fn (l, r) => helper(l - 1, r, ())
}

let filter = {
    let helper = fn(f, ls, acc) => match ls
        | (hd, ()) -> acc
        | (hd, tl) ->
            if f(hd) 
                then helper(f, tl, (hd, acc))
                else helper(f, tl, acc)

    fn(f, ls) => helper(f, ls, ())
}

let sum = {
    let helper = fn(ls, acc) => match ls
        | (hd, ()) -> hd + acc
        | (hd, tl) -> helper(tl, hd + acc)
    
    fn (ls) => helper(ls, 0)
}

let predicate = fn(n) => (n % 3 == 0) || (n % 5 == 0)
inspect(sum(filter(predicate, range(1, 1000)))) # 233168
```

More project euler problems can be found in the [examples folder](https://github.com/mkhan45/RustScript2/tree/main/examples).
