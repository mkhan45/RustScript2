let f(x) = if x < 2 then 1 else 1 + g(x - 1)
let g(x) = 2 + f(x - 1)

inspect(f(12))
