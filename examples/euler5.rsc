let gcd = fn(a, b) => match (a, b)
    | (a, 0) -> a 
    | (a, b) -> gcd(b, a mod b)

let lcm = fn(a, b) => (a * b) / gcd(a, b)

let euler5 = fold(1, lcm, [1..20])
