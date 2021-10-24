let (a, b) = {
    let a = (4, 2)
    let (a, b) = a
    (a * b, 12)
}

let f = fn(a, b, c) => {
    let g = fn(a, b) => a * b + c
    g(b, c) + a
}

f(10, 5, 3)

let c = 5 + { 5 + 10 * 2 }
