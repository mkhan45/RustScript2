let sum_tailrec = fn(n, acc) => {
    if n == 0
        then acc
        else sum_tailrec(n - 1, acc + n)
}
