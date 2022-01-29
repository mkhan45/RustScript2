let f = fn(x, y) => {
    let c = x * y
    c + x + y
}

let t = (f(10, 5), f(5, 10))
let (a, b) = t

let f = fn((a, b), c) => a * b + c
println(f((5, 10), 15))

let g(x) = x * 2
println(g(5))

let fib(x) = if x < 2 then x else fib(x - 1) + fib(x - 2)
println(fib(10))

let range(l, r) = if l == r then () else (l, range(l + 1, r))
println(range(5, 15))

let map(f, ls) = if ls == () then () else {
    println(ls)
    let (hd, tl) = ls
    (hd, map(f, tl))
}
println(map(fib, range(1, 10)))
