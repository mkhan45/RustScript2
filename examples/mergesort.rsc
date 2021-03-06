let merge = fn(xs, ys) => match (xs, ys)
    | (ls, [])|([], ls) -> ls
    | ([x|xs], [y|ys]) when x <= y -> [x | merge(xs, [y|ys])]
    | ([x|xs], [y|ys]) -> [y | merge([x|xs], ys)]

let sort = fn(ls) => {
    let pairs = fn(ls) => match ls
        | [a, b | tl] -> [merge(a, b) | pairs(tl)]
        | _ -> ls

    let loop = fn(ls) => match ls
        | [x] -> x
        | _ -> loop(pairs(ls))

    loop([[x] for x in ls])
}

# inspect(sort([5, 4, 12, 17, 6, 7, 4, 3, 2, 8, 9]))
