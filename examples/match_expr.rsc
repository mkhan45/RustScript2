let fib = fn(n) => match n
    | 0 | 1 -> 1
    | _ as x -> fib(x - 1) + fib(n - 2)

let fib2 = fn(n) => if let 0 | 1 = n then 1 else fib2(n - 1) + fib2(n - 2)
