let fib = fn(n) => match n
    | 0 -> 1
    | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2)
