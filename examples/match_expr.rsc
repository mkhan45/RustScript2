let fib = fn(n) => match n
    | 0 | 1 -> 1
    | _ as x -> fib(x - 1) + fib(n - 2)
