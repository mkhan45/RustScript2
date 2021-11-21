let predicate = fn(n) => (n mod 3 == 0) || (n mod 5 == 0)
# inspect(sum(filter_rev(predicate, range(1, 1000)))) #233168
