let add(a, b) = a + b
let sub(a, b) = a - b
let quadratic(a, b, c, x) = a * x * x + b * x + c

let f = add(5, _)
let g = sub(_, 5)

inspect(f(3))
inspect(g(3))

inspect(quadratic(1, 2, 0, 4))
let f = quadratic(_, _, 0, _)

inspect(f(1, 2, 4))
