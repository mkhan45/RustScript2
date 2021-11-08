let fib(n) = match n
    | 0 | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2)

# fib(4)
# fib(3), fib(2)
# fib(2), fib(1), fib(2)
# fib(1), fib(0), fib(1), fib(2)

let fib_stack(n) = {
    let loop(stack, acc) = match stack
	| [] -> acc
	| [:thunk, n | xs] when n < 2 -> loop([1 | xs], acc)
	| [:thunk, n | xs] -> loop([:thunk, n - 1, :thunk, n - 2 | xs], acc)
	| [n | stack] -> loop(stack, acc + n)

    loop([:thunk, n], 0)
}

let multirecur(init, branches) = {
    let loop = fn(stack, acc) => match stack
	| [] -> acc
	| [x | xs] -> {
	    let inner = fn(branches) => match branches
		| [] -> {
		    println("No branches matched")
		    let () = 1
		}
		| [(cond, recurrences, accumulate) | _] when cond(x) -> {
		    let recurrences = recurrences(x)
		    loop(recurrences + xs, accumulate(acc, x))
		}
		| [_ | branches] -> inner(branches)

	    inner(branches)
	}

    fn(n) => loop([n], init)
}

inspect(fib(8))
inspect(fib_stack(8))

let base_branch = (fn(n) => n < 2, fn(_) => [], fn(acc, _) => acc + 1)
let recur_branch = (fn(n) => n >= 2, fn(n) => [n -  1, n - 2], fn(acc, _) => acc)
let fib_multirecur_stack = multirecur(0, [base_branch, recur_branch])
# inspect(fib_multirecur_stack(5))

let ack_stack(m, n) = {
    let loop(stack) = match stack
	| [n] -> n
	| [n, 0 | stack] -> loop([n + 1 | stack])
	| [0, m | stack] -> loop([1, m - 1 | stack])
	| [n, m | stack] -> loop([n - 1, m, m - 1 | stack])

    loop([n, m])
}

let ack_stack2(m, n) = {
    let loop(stack, acc) = match stack
	| [] -> acc
	| [n, 0 | stack] -> loop([n + 1 | stack], acc)
	| [0, m | stack] -> loop([1, m - 1 | stack], acc)
	| [n, m | stack] -> loop([n - 1, m, m - 1 | stack], acc)
	| [n | stack] -> loop(stack, n)

    loop([n, m], 0)
}

inspect(ack_stack(3, 3))
inspect(ack_stack2(3, 3))

let sum_range(start, end) = {
    let loop(stack, acc) = match stack
	| [] -> acc
	| [start, end | stack] when start == end-> loop([], acc)
	| [start, end | stack] -> loop([start + 1, end | stack], acc + start)

    loop([start, end], 0)
}

inspect(sum_range(1, 100))
inspect(sum([1..100]))

#let ack = {
#    let init = 0
#    let branches = [
#	(fn((m, n)) => m == 0, fn(_) => [], fn(acc, (m, n)) => acc + n + 1),
#	(fn((m, n)) => n == 0, fn((m, _)) => [(m - 1, 1)], fn(acc, _) => acc),
#	(fn(_) => T, fn((m, n)) => [(m - 1, ????)], fn(acc, _) => acc),
#    ]
#    branches
#}
#
#inspect(ack)

# need tuples of base cond, reccurence, accumulate
# loop through the base conditions, if one is true accumulate on the recurrences
