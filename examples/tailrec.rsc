let sum_tailrec(n, acc) = {
    if n == 0
        then acc
        else sum_tailrec(n - 1, acc + n)
}

inspect(sum_tailrec(500000, 0))
