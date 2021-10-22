let gcd = fn(a, b) => match (a, b)
    | (0, b) -> b
    | (a, 0) -> a
    | (a, b) -> {
        if a > b then {
            gcd(b, a)
        } else {
            let remainder = b % a
            if remainder != 0 then (gcd(a, remainder)) else a
        }
    }

let pollard = fn(n) => match n
    | 1 -> ()
    | n -> if n % 2 == 0 then {
        2
    } else {
        let g = fn(x, n) => (x * x + 1) % n
        let iter = fn(x, y, d) => match (x, y, d)
            | (x, y, 1) -> {
                let x = g(x, n)
                let y = g(g(y, n), n)
                let d = gcd(if (x > y) then (x - y) else (y - x), n)
                iter(x, y, d)
            }
            | (_, _, d) -> if d == n then () else d

        iter(2, 2, 1)
    }

let largest_factor = fn(n) => {
    let d = pollard(n)
    if d == () then () else n / d
}

let euler3 = {
    # repeatedly factors until largest is found
    let aux = fn(n) => match largest_factor(n)
        | () -> n
        | f -> if n == f then f else aux(f)

    let n = 600851475143
    aux(n)
}
