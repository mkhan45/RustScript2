let sum_sqr = {
    let s = sum([1..101])
    s * s
}

let sqr_sum = sum([x * x for x in [1..101]])

let euler6 = sum_sqr - sqr_sum

# inspect(euler6) # 25164150
