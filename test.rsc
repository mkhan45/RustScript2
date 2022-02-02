let f = fn(x, y) => {
    let c = x * y
    c + x + y
}

let t = (f(10, 5), f(5, 10))
let (a, b) = t

let f = fn((a, b), c) => a * b + c
inspect(f((5, 10), 15))

let g(x) = x * 2
inspect(g(5))

let fib(x) = if x < 2 then x else fib(x - 1) + fib(x - 2)
inspect(fib(10))

let range(l, r) = if l == r then () else (l, range(l + 1, r))
inspect(range(5, 15))

let map(f, ls) = if ls == () then () else {
    inspect(ls)
    let (hd, tl) = ls
    (hd, map(f, tl))
}
inspect(map(fib, range(1, 10)))

let fib2(n) = match n
    | 0 -> 1
    | 1 -> 1
    | _ -> fib(n - 1) + fib(n - 2)
inspect(fib2(10))

fib2(10) |> inspect

10
|> fn(a) => a + 50
|> fn(a) => a * 20
|> fn(a) => a - 20
|> fn(a) => a / 2
|> inspect
inspect((((10 + 50) * 20) - 20) / 2)
