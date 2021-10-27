let fib = fn (n) => {
    match n
	| 0 | 1 -> 1
	| _ -> fib(n - 1) + fib(n - 2)
}

# inspect(fib(30))
