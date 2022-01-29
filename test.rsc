let f = fn(x, y) => {
    let c = x * y
    c + x + y
}

let t = (f(10, 5), f(5, 10))
let (a, b) = t
