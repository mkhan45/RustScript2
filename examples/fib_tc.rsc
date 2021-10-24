let fib = {
    let fib_helper = fn(n, a, b) => if n == 0 then b else fib_helper(n - 1, b, a + b)

    fn (n) => fib_helper(n, 1, 1)
}
