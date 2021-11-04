let sum = fn(n, acc) => {
    if n == 0
        then acc
        else sum(n - 1, acc + n)
}

inspect(sum(100, 0))
