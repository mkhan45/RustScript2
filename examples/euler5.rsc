let gcd = fn(a, b) => match (a, b)
    | (a, 0) -> a 
    | (a, b) -> gcd(b, a % b)

let lcm = fn(a, b) => (a * b) / gcd(a, b)

let range = {
    let helper = fn (l, r, acc) => 
        if l == r then acc else helper(l, r - 1, (r, acc))
    
    fn (l, r) => helper(l - 1, r, ())
}

let foldl = fn (acc, f, ls) => match ls
        | () -> acc
        | (x, xs) -> foldl(f(acc, x), f, xs)

let euler5 = foldl(1, lcm, range(1, 20))
